{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Dyno.Models.Aircraft ( AcX(..), AcU(..), aircraftDae ) where

import GHC.Generics ( Generic, Generic1 )

import Linear

import Dyno.Vectorize
import Dyno.Server.Accessors ( Lookup(..) )
import Dyno.Models.AeroCoeffs

data AcX a = AcX { ac_r_n2b_n :: V3 a
                 , ac_v_bn_b :: V3 a
                 , ac_R_n2b :: M33 a
                 , ac_w_bn_b :: V3 a
                 , ac_u :: AcU a
                 } deriving (Eq, Functor, Generic, Generic1, Show)
data AcU a = AcU { acSurfaces :: ControlSurfaces a
                 } deriving (Eq, Functor, Generic, Generic1, Show)
newtype AcZ a = AcZ (None a) deriving (Eq, Functor, Generic, Generic1, Show)
newtype AcR a = AcR (AcX a) deriving (Eq, Functor, Generic, Generic1, Show)
newtype AcP a = AcP (None a) deriving (Eq, Functor, Generic, Generic1, Show)

instance Vectorize AcX
instance Vectorize AcZ
instance Vectorize AcU
instance Vectorize AcP
instance Vectorize AcR

instance (Lookup a, Generic a) => Lookup (AcX a)
instance (Lookup a, Generic a) => Lookup (AcZ a)
instance (Lookup a, Generic a) => Lookup (AcU a)
instance (Lookup a, Generic a) => Lookup (AcP a)
instance (Lookup a, Generic a) => Lookup (AcR a)

subCs :: Num a => ControlSurfaces a -> ControlSurfaces a -> ControlSurfaces a
subCs (ControlSurfaces x0 x1 x2 x3) (ControlSurfaces y0 y1 y2 y3) =
  ControlSurfaces (x0-y0) (x1-y1) (x2-y2) (x3-y3)

aircraftDae :: forall a. Floating a =>
       (a, M33 a) -> AeroForceCoeffs a -> AeroMomentCoeffs a -> AeroRefs a ->
       AcX a -> AcX a -> AcU a -> AcX a
aircraftDae
  (mass, inertia)
  forceCoeffs
  momentCoeffs
  refs
  (AcX r_n2b_n' v_bn_b' dcm_n2b' w_bn_b' (AcU controlSurfaces'))
  (AcX       _  v_bn_b  dcm_n2b  w_bn_b  (AcU controlSurfaces))
  (AcU controlSurfaces'') = daeResidual
  where
    v_bw_b = v_bn_b -- no relative wind
    (aero_forces_body, moments_body) = aeroForcesMoments forceCoeffs momentCoeffs refs v_bw_b w_bn_b controlSurfaces
    forces_body = aero_forces_body + dcm_n2b !* (V3 0 0 (9.81*mass))

    daeResidual =
      AcX { ac_r_n2b_n = (trans dcm_n2b) !* v_bn_b - r_n2b_n'
          , ac_v_bn_b = v_bn_b' + cross w_bn_b v_bn_b - forces_body ^/ mass
          , ac_R_n2b = (trans (skew w_bn_b)) !*! dcm_n2b - dcm_n2b'
          , ac_w_bn_b = inertia !* w_bn_b' + cross w_bn_b (inertia !* w_bn_b) - moments_body
          , ac_u = AcU $ subCs controlSurfaces'' controlSurfaces'
          }
