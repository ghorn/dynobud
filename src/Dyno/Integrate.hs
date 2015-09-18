{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dyno.Integrate
       ( InitialTime(..)
       , TimeStep(..)
       , rk45
       ) where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Numeric.GSL.ODE as ODE
import qualified Numeric.LinearAlgebra.Data as D

import Dyno.Vectorize ( Vectorize(..), devectorize )

newtype InitialTime = InitialTime {unInitialTime :: Double}
                    deriving (Num, Fractional, Floating, Ord, Eq, Show)
newtype TimeStep = TimeStep {unTimeStep :: Double}
                 deriving (Num, Fractional, Floating, Ord, Eq, Show)

rk45 :: Vectorize x
        => (Double -> x Double -> x Double)
        -> InitialTime -> TimeStep -> x Double -> x Double
rk45 f (InitialTime t0) (TimeStep h) x0 = devectorize $ sv $ last sol
  where
    vs :: V.Vector Double -> SV.Vector Double
    vs = SV.fromList .  V.toList
    sv :: SV.Vector Double -> V.Vector Double
    sv =  V.fromList . SV.toList

    sol = D.toRows $
          ODE.odeSolveV
          ODE.RKf45
--          ODE.RK8pd
--          ODE.MSAdams --todo(benchmark)
          h 1e-8 1e-6 f'
          (vs (vectorize x0))
          (SV.fromList [0, h])
    f' :: Double -> SV.Vector Double -> SV.Vector Double
    f' t x = vs $ vectorize $ f (t + t0) (devectorize (sv x))
