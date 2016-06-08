{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dyno.Integrate
       ( InitialTime(..)
       , TimeStep(..)
       , rk45
       ) where

import GHC.Generics ( Generic, Generic1 )

import Accessors ( Lookup(..) )
import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Binary ( Binary )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Numeric.GSL.ODE as ODE
import qualified Numeric.LinearAlgebra.Data as D

import Dyno.View.Vectorize ( Vectorize(..), devectorize )

newtype InitialTime a = InitialTime {unInitialTime :: a}
  deriving (Functor, Generic, Generic1, FromJSON, ToJSON, Num, Fractional, Floating, Ord, Eq, Show)
instance Vectorize InitialTime
instance Lookup a => Lookup (InitialTime a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . lens1)
    where
      lens1 f y = fmap InitialTime (f (unInitialTime y))
instance Binary a => Binary (InitialTime a)


newtype TimeStep a = TimeStep {unTimeStep :: a}
  deriving (Functor, Generic, Generic1, FromJSON, ToJSON, Num, Fractional, Floating, Ord, Eq, Show)
instance Vectorize TimeStep
instance Lookup a => Lookup (TimeStep a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . lens1)
    where
      lens1 f y = fmap TimeStep (f (unTimeStep y))
instance Binary a => Binary (TimeStep a)

rk45 :: Vectorize x
        => (Double -> x Double -> x Double)
        -> InitialTime Double -> TimeStep Double -> x Double -> x Double
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
