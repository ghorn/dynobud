{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Dyno.DirectCollocation.Quadratures
       ( mkTaus
       , interpolate
       ) where

import qualified Data.Vector as V
import qualified Data.Foldable as F
import Linear.V

import JacobiRoots ( shiftedLegendreRoots )

import Dyno.View
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeXis )

--data RorL = Radau | Legendre deriving (Eq, Show)

mkTaus :: Fractional a => Int -> Vec deg a
mkTaus deg = case shiftedLegendreRoots deg of
  Just taus -> TV.mkVec $ V.map (fromRational . toRational) taus
  Nothing -> error "makeTaus: too high degree"


dot :: forall x deg a b. (Fractional (J x a), Real b) => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unSeq elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y


interpolate :: (Dim deg, Real b, Fractional b, Fractional (J x a), View x) =>
               Vec deg b -> J x a -> Vec deg (J x a) -> J x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : F.toList taus) 1) [0..deg]
    deg = TV.tvlength taus
