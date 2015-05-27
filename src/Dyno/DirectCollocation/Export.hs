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

import Accessors ( Lookup, flatten, accessors )

import Dyno.Nlp ( NlpOut(..) )
import Dyno.TypeVecs ( Vec )
import Dyno.Vectorize ( Vectorize, Id(..), fill )
import Dyno.View.View ( View(..) )
import Dyno.View.JV ( JV, splitJV, catJV )
import Dyno.View.JVec ( JVec(..) )
import Dyno.DirectCollocation.Formulate ( CollProblem(..) )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollOcpConstraints(..)
                                    , CollStage(..), CollPoint(..)
                                    , StageOutputs(..), Quadratures(..)
                                    )
import Dyno.DirectCollocation.Quadratures ( timesFromTaus )

toMatlab ::
  forall x z u p fp r o c h q po n deg
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
    , Dim n, Dim deg
    )
  => CollProblem x z u p r o c h q po fp n deg
  -> fp Double
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO String
toMatlab cp fp nlpOut = do
  let ct@(CollTraj tf' p' stages' xf) = split (xOpt nlpOut)
      CollTraj lagTf' lagP' _ _ = split (lambdaXOpt nlpOut)
      lagBc' = coBc $ split (lambdaGOpt nlpOut)

  (_, outs, finalQuads) <- cpHellaOutputs cp (cat ct) (catJV fp)
  let _ = outs :: Vec n (StageOutputs x o h q po deg Double)
      _ = finalQuads :: Quadratures q Double

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

      stages :: [CollStage (JV x) (JV z) (JV u) deg (Vector Double)]
      stages = map split $ F.toList $ unJVec $ split stages'

      xs :: [x Double]
      xs = concatMap getXs stages ++ [splitJV xf]

      zs :: [z Double]
      zs = concatMap getZs stages

      us :: [u Double]
      us = concatMap getUs stages

      os :: [o Double]
      xdots :: [x Double]
      hs :: [h Double]
      -- drop the interpolated value
      os = map splitJV os'
      xdots = map splitJV xdots'
      hs = map splitJV hs'
      pos = map splitJV pos'
      (os', xdots', hs', pos', _, _) = unzip6 $ F.concatMap (F.toList . soVec) outs
      toQ :: StageOutputs x o h q po deg Double -> [Quadratures q Double]
      toQ stageOutputs = (map (\(_,_,_,_,qs',_) -> qs') (F.toList (soVec stageOutputs))) ++ [soQNext stageOutputs]
      qs :: [Quadratures q Double]
      qs = fill 0 : F.concatMap toQ outs

      toQd :: StageOutputs x o h q po deg Double -> [Quadratures q Double]
      toQd stageOutputs = (map (\(_,_,_,_,_,qd) -> qd) (F.toList (soVec stageOutputs)))
      qds :: [Quadratures q Double]
      qds = F.concatMap toQd outs

      getXs (CollStage x0 xzus) = splitJV x0 : map (getX . split) (F.toList (unJVec (split xzus)))
      getZs (CollStage  _ xzus) =              map (getZ . split) (F.toList (unJVec (split xzus)))
      getUs (CollStage  _ xzus) =              map (getU . split) (F.toList (unJVec (split xzus)))

      getX :: CollPoint (JV x) (JV z) (JV u) (Vector Double) -> x Double
      getX (CollPoint x _ _) = splitJV x

      getZ :: CollPoint (JV x) (JV z) (JV u) (Vector Double) -> z Double
      getZ (CollPoint _ z _) = splitJV z

      getU :: CollPoint (JV x) (JV z) (JV u) (Vector Double) -> u Double
      getU (CollPoint _ _ u) = splitJV u

      at :: (Vectorize xzu, Lookup (xzu Double)) => [(String, xzu Double -> Double)]
      at = flatten $ accessors (fill 0)

      woo :: String -> [xzu Double] -> String -> (xzu Double -> Double) -> String
      woo topName xzus name get = topName ++ "." ++ name ++ " = " ++ show (map get xzus) ++ ";"

      wooP :: String -> pq Double -> String -> (pq Double -> Double) -> String
      wooP topName p name get = topName ++ "." ++ name ++ " = " ++ show (get p) ++ ";"

      ret :: String
      ret = unlines $
            map (uncurry (woo "ret.diffStates" xs)) at ++
            map (uncurry (woo "ret.diffStateDerivs" xdots)) at ++
            map (uncurry (woo "ret.algVars" zs)) at ++
            map (uncurry (woo "ret.controls" us)) at ++
            map (uncurry (woo "ret.outputs" os)) at ++
            map (uncurry (woo "ret.pathConstraints" hs)) at ++
            map (uncurry (woo "ret.plotOutputs" pos)) at ++
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
