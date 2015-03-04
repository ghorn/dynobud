{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveGeneric #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Quadratures
       ( QuadratureRoots(..)
       , mkTaus
       , interpolate
       , timesFromTaus
       , collocationTimes
       ) where

import GHC.Generics ( Generic )

import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Serialize ( Serialize(..) )
import Linear.V

import JacobiRoots ( shiftedLegendreRoots, shiftedRadauRoots )

import Dyno.View.View ( View, J )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeXis )

data QuadratureRoots = Legendre | Radau deriving (Show, Eq, Ord, Enum, Generic)
instance Serialize QuadratureRoots

mkTaus ::
  forall deg a
  . (Dim deg, Fractional a)
  => QuadratureRoots -> Vec deg a
mkTaus quadratureRoots = case taus of
  Just taus' -> TV.mkVec $ V.map (fromRational . toRational) taus'
  Nothing -> error "makeTaus: too high degree"
  where
    deg = reflectDim (Proxy :: Proxy deg)
    taus :: Maybe (V.Vector Double)
    taus = case quadratureRoots of
      Legendre -> shiftedLegendreRoots deg
      Radau -> fmap (`V.snoc` 1.0) (shiftedRadauRoots (deg-1))


-- todo: code duplication
dot :: forall x deg a b. (Fractional (J x a), Real b, Dim deg) => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unVec elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y


-- todo: code duplication
interpolate :: (Dim deg, Real b, Fractional b, Fractional (J x a), View x) =>
               Vec deg b -> J x a -> Vec deg (J x a) -> J x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : F.toList taus) 1) [0..deg]
    deg = TV.tvlength taus


timesFromTaus ::
  forall n deg a
  . (Num a, Dim n, Dim deg)
  => a -> Vec deg a -> a -> Vec n (a, Vec deg a)
timesFromTaus t0 taus dt = times
  where
    n = reflectDim (Proxy :: Proxy n)

    -- initial time at each collocation stage
    t0s :: Vec n a
    t0s = TV.mkVec' $ take n [t0 + (dt * fromIntegral k) | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (a, Vec deg a)
    times = fmap (\t0' -> (t0', fmap (\tau -> t0' + tau * dt) taus)) t0s

collocationTimes ::
  (Dim n, Dim deg, Fractional a) => a -> QuadratureRoots -> a -> Vec n (a, Vec deg a)
collocationTimes t0 qr dt = timesFromTaus t0 (mkTaus qr) dt
