{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Export
       ( toMatlab
       ) where

import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F

import Accessors ( Lookup, flatten, accessors )

import Dyno.View.Unsafe.View ( unJ )

import Dyno.Nlp ( NlpOut(..) )
import Dyno.TypeVecs ( Vec )
import Dyno.Vectorize ( Vectorize, Id(..), fill )
import Dyno.View.View ( View(..) )
import Dyno.View.JV ( JV, splitJV )
import Dyno.View.JVec ( JVec(..) )
import Dyno.DirectCollocation.Formulate ( CollProblem(..) )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollStage(..), CollPoint(..) )
import Dyno.DirectCollocation.Quadratures ( timesFromTaus )

toMatlab ::
  forall x z u p r o c h q n deg lol
  . ( Lookup (x Double), Vectorize x
    , Lookup (z Double), Vectorize z
    , Lookup (u Double), Vectorize u
    , Lookup (o Double), Vectorize o
    , Lookup (p Double), Vectorize p
    , Lookup (h Double), Vectorize h
    , Dim n, Dim deg
    )
  => CollProblem x z u p r o c h q n deg
  -> NlpOut (CollTraj x z u p n deg) lol (Vector Double)
  -> IO String
toMatlab cp nlpOut = do
  let ct@(CollTraj tf' p' stages' xf) = split (xOpt nlpOut)
      CollTraj lagTf' lagP' _ _ = split (lambdaXOpt nlpOut)

  outs <- cpOutputs cp (cat ct)

  let taus :: Vec deg Double
      taus = cpTaus cp
      tf = V.head (unJ tf')

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
      (os, xdots, hs) = unzip3 $ F.concatMap (F.toList . fst) outs -- drop the interpolated value

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

      wooP :: String -> p Double -> String -> (p Double -> Double) -> String
      wooP topName p name get = topName ++ "." ++ name ++ " = " ++ show (get p) ++ ";"

      ret :: String
      ret = unlines $
            map (uncurry (woo "ret.diffStates" xs)) at ++
            map (uncurry (woo "ret.diffStateDerivs" xdots)) at ++
            map (uncurry (woo "ret.algVars" zs)) at ++
            map (uncurry (woo "ret.controls" us)) at ++
            map (uncurry (woo "ret.outputs" os)) at ++
            map (uncurry (woo "ret.pathConstraints" hs)) at ++
            map (uncurry (wooP "ret.params" (splitJV p'))) at ++
            map (uncurry (wooP "ret.lagrangeMultipliers.params" (splitJV lagP'))) at ++
            [ "ret.lagrangeMultipliers.T = " ++ show (unId (splitJV lagTf')) ++ ";"
            , ""
            , "ret.tx = " ++ show xTimes ++ ";"
            , "ret.tzuo = " ++ show zuoTimes ++ ";"
            , "ret.N = " ++ show n ++ ";"
            , "ret.deg = " ++ show (reflectDim (Proxy :: Proxy deg)) ++ ";"
            , "ret.collocationRoots = '" ++ show (cpRoots cp) ++ "';"
            ]
  return ret
