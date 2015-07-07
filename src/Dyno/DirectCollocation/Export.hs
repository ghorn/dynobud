{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Export
       ( toMatlab
       ) where

import Data.List ( unzip6 )
import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )
import qualified Data.Foldable as F

import Accessors ( Lookup, Getter(..), flatten, accessors )

import Dyno.Nlp ( NlpOut(..) )
import Dyno.TypeVecs ( Vec )
import Dyno.Vectorize ( Vectorize, Id(..), fill )
import Dyno.View.View ( View(..) )
import Dyno.View.JV ( splitJV, catJV )
import Dyno.DirectCollocation.Formulate ( CollProblem(..) )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollOcpConstraints(..)
                                    , StageOutputs(..), Quadratures(..)
                                    , getXzus'
                                    )
import Dyno.DirectCollocation.Quadratures ( timesFromTaus )

toMatlab ::
  forall x z u p fp r o c h q qo po n deg
  . ( Lookup (x Double), Vectorize x
    , Lookup (z Double), Vectorize z
    , Lookup (u Double), Vectorize u
    , Lookup (o Double), Vectorize o
    , Lookup (p Double), Vectorize p
    , Lookup (c Double), Vectorize c
    , Vectorize r
    , Lookup (fp Double), Vectorize fp
    , Lookup (h Double), Vectorize h
    , Lookup (q Double), Vectorize q
    , Lookup (po Double), Vectorize po
    , Lookup (qo Double), Vectorize qo
    , Dim n, Dim deg
    )
  => CollProblem x z u p r o c h q qo po fp n deg
  -> fp Double
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO String
toMatlab cp fp nlpOut = do
  let ct@(CollTraj tf' p' _ _) = split (xOpt nlpOut)
      CollTraj lagTf' lagP' _ _ = split (lambdaXOpt nlpOut)
      lagBc' = coBc $ split (lambdaGOpt nlpOut)

  (_, outs, finalQuads) <- cpHellaOutputs cp (cat ct) (catJV fp)
  let _ = outs :: Vec n (StageOutputs x o h q qo po deg Double)
      _ = finalQuads :: Quadratures q qo Double

  let taus :: Vec deg Double
      taus = cpTaus cp
      Id tf = splitJV tf'

      n = reflectDim (Proxy :: Proxy n)

      times :: Vec n (Double, Vec deg Double)
      times = timesFromTaus 0 taus dt
        where
          dt = tf / fromIntegral n

      xTimes = concatMap (\(t0,ts) -> t0 : F.toList ts) (F.toList times) ++ [tf]
      zuoTimes = concatMap (\(_,ts) -> F.toList ts) (F.toList times)

      xss :: Vec n (x Double, Vec deg (x Double))
      xf :: x Double
      zss :: Vec n (Vec deg (z Double))
      uss :: Vec n (Vec deg (u Double))
      ((xss,xf), zss, uss) = getXzus' ct

      fullXs :: [x Double]
      fullXs = concatMap (\(x0, xs') -> x0 : F.toList xs') (F.toList xss) ++ [xf]

      xs :: [x Double]
      xs = concatMap (F.toList . snd) (F.toList xss)

      zs :: [z Double]
      zs = concatMap F.toList (F.toList zss)

      us :: [u Double]
      us = concatMap F.toList (F.toList uss)

      os :: [o Double]
      xdots :: [x Double]
      hs :: [h Double]
      -- drop the interpolated value
      os = map splitJV os'
      xdots = map splitJV xdots'
      hs = map splitJV hs'
      pos = map splitJV pos'
      (os', xdots', hs', pos', _, _) = unzip6 $ F.concatMap (F.toList . soVec) outs
      qsFull :: [Quadratures q qo Double]
      qsFull = fill 0 : F.concatMap toQFull outs
        where
          toQFull :: StageOutputs x o h q qo po deg Double -> [Quadratures q qo Double]
          toQFull stageOutputs = (map (\(_,_,_,_,qs',_) -> qs') (F.toList (soVec stageOutputs))) ++ [soQNext stageOutputs]

      qs :: [Quadratures q qo Double]
      qs = F.concatMap toQ outs
        where
          toQ :: StageOutputs x o h q qo po deg Double -> [Quadratures q qo Double]
          toQ stageOutputs = map (\(_,_,_,_,qs',_) -> qs') (F.toList (soVec stageOutputs))

      toQd :: StageOutputs x o h q qo po deg Double -> [Quadratures q qo Double]
      toQd stageOutputs = (map (\(_,_,_,_,_,qd) -> qd) (F.toList (soVec stageOutputs)))
      qds :: [Quadratures q qo Double]
      qds = F.concatMap toQd outs

      at :: forall xzu . (Vectorize xzu, Lookup (xzu Double)) => [(String, xzu Double -> Double)]
      at = map (\(fn,g,_) -> (fn, toDub g)) $ flatten $ accessors (fill (0 :: Double))
        where
          toDub :: Getter (xzu Double) -> xzu Double -> Double
          toDub (GetDouble f) = f
          toDub (GetFloat f) = realToFrac . f
          toDub (GetInt f) = realToFrac . f
          toDub (GetBool f) = fromIntegral . fromEnum . f
          toDub GetSorry = const (read "NaN")

      woo :: String -> [xzu Double] -> String -> (xzu Double -> Double) -> String
      woo topName xzus name get = topName ++ "." ++ name ++ " = " ++ show (map get xzus) ++ ";"

      wooP :: String -> pq Double -> String -> (pq Double -> Double) -> String
      wooP topName p name get = topName ++ "." ++ name ++ " = " ++ show (get p) ++ ";"

      ret :: String
      ret = unlines $
            map (uncurry (woo "ret.diffStatesFull" fullXs)) at ++
            map (uncurry (woo "ret.diffStates" xs)) at ++
            map (uncurry (woo "ret.diffStateDerivs" xdots)) at ++
            map (uncurry (woo "ret.algVars" zs)) at ++
            map (uncurry (woo "ret.controls" us)) at ++
            map (uncurry (woo "ret.outputs" os)) at ++
            map (uncurry (woo "ret.pathConstraints" hs)) at ++
            map (uncurry (woo "ret.plotOutputs" pos)) at ++
            map (uncurry (woo "ret.quadratureStatesFull" qsFull)) at ++
            map (uncurry (woo "ret.quadratureStates" qs)) at ++
            map (uncurry (woo "ret.quadratureStateDerivs" qds)) at ++
            map (uncurry (wooP "ret.params" (splitJV p'))) at ++
            map (uncurry (wooP "ret.lagrangeMultipliers.params" (splitJV lagP'))) at ++
            map (uncurry (wooP "ret.lagrangeMultipliers.bc" (splitJV lagBc'))) at ++
            map (uncurry (wooP "ret.finalQuadratureStates" finalQuads)) at ++
            [ "ret.lagrangeMultipliers.T = " ++ show (unId (splitJV lagTf')) ++ ";"
            , ""
            , "ret.tx = " ++ show xTimes ++ ";"
            , "ret.tzuo = " ++ show zuoTimes ++ ";"
            , "ret.T = " ++ show tf ++ ";"
            , "ret.N = " ++ show n ++ ";"
            , "ret.deg = " ++ show (reflectDim (Proxy :: Proxy deg)) ++ ";"
            , "ret.collocationRoots = '" ++ show (cpRoots cp) ++ "';"
            ]
  return ret
