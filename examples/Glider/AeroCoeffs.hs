{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveGeneric #-}

module Glider.AeroCoeffs where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Foldable as F
import Linear

import Accessors ( Lookup )

import Dyno.Vectorize

atan2' :: Floating a => a -> a -> a
atan2' y x = 2 * atan (y / (sqrt(x*x + y*y + 1e-15) + x) )

data ControlSurfaces a =
  ControlSurfaces { csElev :: a
                  , csRudder :: a
                  , csAil :: a
                  , csFlaps :: a
                  } deriving (Eq, Functor, F.Foldable, Generic, Generic1, Show)
instance Vectorize ControlSurfaces
instance (Lookup a, Generic a) => Lookup (ControlSurfaces a)

data AeroForceCoeffs a =
  AeroForceCoeffs { af_cL0 :: a
                  , af_cL_A :: a
                  , af_cL_elev :: a
                  , af_cL_flaps :: a

                  , af_cD0 :: a
                  , af_cD_A :: a
                  , af_cD_A2 :: a
                  , af_cD_B2 :: a

                  , af_cD_elev :: a
                  , af_cD_elev2 :: a
                  , af_cD_A_elev :: a

                  , af_cD_flaps :: a
                  , af_cD_flaps2 :: a
                  , af_cD_A_flaps :: a

                  , af_cD_rudder :: a
                  , af_cD_rudder2 :: a
                  , af_cD_B_rudder :: a

                  , af_cD_ail :: a
                  , af_cD_ail2 :: a
                  , af_cD_B_ail :: a

                  , af_cY_B :: a
                  , af_cY_rudder :: a
                  } deriving (Functor, Generic, Generic1, Show)
instance Vectorize AeroForceCoeffs

data AeroMomentCoeffs a =
  AeroMomentCoeffs { am_cm0 :: a

                   , am_cl_p :: a
                   , am_cl_q :: a
                   , am_cl_r :: a

                   , am_cm_p :: a
                   , am_cm_q :: a
                   , am_cm_r :: a

                   , am_cn_p :: a
                   , am_cn_q :: a
                   , am_cn_r :: a

                   , am_cl_B :: a
                   , am_cl_AB :: a
                   , am_cm_A :: a
                   , am_cn_B :: a
                   , am_cn_AB :: a

                   , am_cl_ail :: a
                   , am_cm_elev
                   , am_cm_flaps :: a
                   , am_cn_rudder :: a
                   } deriving (Functor, Generic, Generic1, Show)
instance Vectorize AeroMomentCoeffs

data AeroRefs a =
  AeroRefs { ar_sref :: a
           , ar_bref :: a
           , ar_cref :: a
           } deriving (Functor, Generic, Generic1, Show)
instance Vectorize AeroRefs


-- | Compute aerodynamic forces/moments in the body frame.
-- Parameters:
-- dcm_n2b: rotation matrix rotating vectors expressed in NED to vectors expressed in body
-- v_bw_b: body velocity in the wind frame, expressed in the body frame
-- w_bn_b: body angular velocity w.r.t. NED
aeroForcesMoments :: Floating a => AeroForceCoeffs a -> AeroMomentCoeffs a -> AeroRefs a ->
                     V3 a -> V3 a -> ControlSurfaces a -> (V3 a, V3 a)
aeroForcesMoments forceCoeffs momentCoeffs refs v_bw_b w_bn_b controlSurfaces = (forces, moments)
  where
    V3 cL cD cY = aeroForceCoeffs alpha beta controlSurfaces forceCoeffs
    c_lmn = aeroMomentCoeffs alpha beta airspeed w_bn_b controlSurfaces momentCoeffs refs

    -- alpha/beta
    alpha = atan2' v_bw_b_z v_bw_b_x
    beta = asin (v_bw_b_y / airspeed)
    V3 v_bw_b_x v_bw_b_y v_bw_b_z = v_bw_b

    airspeedSquared = quadrance v_bw_b
    airspeed = sqrt airspeedSquared

    moments = rho_sref_v2*^(V3 bref cref bref)*c_lmn
    forces = dragForce + liftForce + sideForce

    dragForce = (-rho_sref_v*cD) *^ v_bw_b
    liftForce = rho_sref_v*cL *^ e_b2L_b_v
    sideForce = rho_sref*cY *^ e_b2Y_b_v2

    -- y axis of aircraft expressed in body frame
    e_b2y_b = V3 0 1 0

    -- lift axis normalized to airspeed
    e_b2L_b_v = cross e_b2y_b v_bw_b
    
    -- sideforces axis normalized to airspeed^2
    e_b2Y_b_v2 = cross e_b2L_b_v (-v_bw_b)
    
    rho_sref = 0.5*rho*sref
    rho_sref_v2 = rho_sref*airspeedSquared
    rho_sref_v = rho_sref*airspeed
    
    sref = ar_sref refs
    bref = ar_bref refs
    cref = ar_cref refs

    rho = 1.23

aeroForceCoeffs :: Num a => a -> a -> ControlSurfaces a -> AeroForceCoeffs a -> V3 a
aeroForceCoeffs alpha beta controlSurfaces coeffs = V3 cL cD cY
  where
    cL_wing = cL_A'*alpha + cL0'
    cD_wing = cD_A'*alpha + cD_A2'*alpha*alpha + cD_B2'*beta*beta + cD0'
    cY_wing = cY_B'*beta

    cL_elev = cL_elev' * elev
    cD_elev = cD_elev2' * elev * elev + cD_A_elev' * elev * alpha + cD_elev' * elev

    cD_ail = cD_ail2'*ail*ail + cD_B_ail'*beta*ail + cD_ail'*ail

    cL_flaps = cL_flaps'*flaps
    cD_flaps = cD_flaps2'*flaps*flaps + cD_A_flaps'*alpha*flaps + cD_flaps'*flaps

    cY_rudder = cY_rudder'*rudder
    cD_rudder = cD_rudder2'*rudder*rudder + cD_B_rudder'*beta*rudder + cD_rudder'*rudder

    cL = cL_wing + cL_elev + cL_flaps
    cD = cD_wing + cD_elev + cD_ail + cD_flaps + cD_rudder
    cY = cY_wing + cY_rudder

    -- inputs
    elev   = csElev   controlSurfaces
    rudder = csRudder controlSurfaces
    ail    = csAil    controlSurfaces
    flaps  = csFlaps  controlSurfaces

    -- unpack aero coeffs
    cL_A'        = af_cL_A coeffs
    cL0'         = af_cL0 coeffs
    cD_A'        = af_cD_A coeffs
    cD_A2'       = af_cD_A2 coeffs
    cD_B2'       = af_cD_B2 coeffs
    cD0'         = af_cD0 coeffs
    cY_rudder'   = af_cY_rudder coeffs
    cD_rudder2'  = af_cD_rudder2 coeffs
    cD_flaps2'   = af_cD_flaps2 coeffs
    cD_elev2'    = af_cD_elev2 coeffs
    cD_flaps'    = af_cD_flaps coeffs
    cD_A_flaps'  = af_cD_A_flaps coeffs
    cD_A_elev'   = af_cD_A_elev coeffs
    cD_elev'     = af_cD_elev coeffs
    cD_ail2'     = af_cD_ail2 coeffs
    cD_ail'      = af_cD_ail coeffs
    cD_B_ail'    = af_cD_B_ail coeffs
    cD_B_rudder' = af_cD_B_rudder coeffs
    cD_rudder'   = af_cD_rudder coeffs
    cL_elev'     = af_cL_elev coeffs
    cL_flaps'    = af_cL_flaps coeffs
    cY_B'        = af_cY_B coeffs


aeroMomentCoeffs :: Fractional a => a -> a -> a -> V3 a -> ControlSurfaces a -> AeroMomentCoeffs a -> AeroRefs a -> V3 a
aeroMomentCoeffs alpha beta airspeed w_bn_b controlSurfaces coeffs refs =
  momentCoeffs0 + momentCoeffs_pqr + momentCoeffs_AB + momentCoeffs_surf
  where
    elev   = csElev   controlSurfaces
    rudder = csRudder controlSurfaces
    ail    = csAil    controlSurfaces
    flaps  = csFlaps  controlSurfaces

    w_bn_b_hat = (V3 bref cref bref) * w_bn_b ^* (0.5/airspeed)

    momentCoeffs0 = V3 cm0 0 0

    momentCoeffs_pqr =
      (V3
       (V3 cl_p cl_q cl_r)
       (V3 cm_p cm_q cm_r)
       (V3 cn_p cn_q cn_r)) !* w_bn_b_hat

    momentCoeffs_AB =
      (V3
       (V3    0 cl_B cl_AB)
       (V3 cm_A    0     0)
       (V3    0 cn_B cn_AB)) !* (V3 alpha beta (alpha*beta))

    momentCoeffs_surf =
      V3
      (cl_ail * ail)
      (cm_elev * elev + cm_flaps * flaps)
      (cn_rudder * rudder)

    -- unpack aero coeffs
    cm0   = am_cm0 coeffs

    cl_p  = am_cl_p coeffs
    cl_q  = am_cl_q coeffs
    cl_r  = am_cl_r coeffs

    cm_p  = am_cm_p coeffs
    cm_q  = am_cm_q coeffs
    cm_r  = am_cm_r coeffs

    cn_p  = am_cn_p coeffs
    cn_q  = am_cn_q coeffs
    cn_r  = am_cn_r coeffs

    cl_B  = am_cl_B coeffs
    cl_AB = am_cl_AB coeffs
    cm_A  = am_cm_A coeffs
    cn_B  = am_cn_B coeffs
    cn_AB = am_cn_AB coeffs
    
    cl_ail    = am_cl_ail coeffs
    cm_elev   = am_cm_elev coeffs
    cm_flaps  = am_cm_flaps coeffs
    cn_rudder = am_cn_rudder coeffs
    
    bref = ar_bref refs
    cref = ar_cref refs
    
trans :: V3 (V3 a) -> V3 (V3 a)
trans (V3
       (V3 e11 e12 e13)
       (V3 e21 e22 e23)
       (V3 e31 e32 e33))
  =
  V3
  (V3 e11 e21 e31)
  (V3 e12 e22 e32)
  (V3 e13 e23 e33)

skew :: Num a => V3 a -> V3 (V3 a)
skew (V3 x y z) =
  V3
  (V3    0  (-z)   y )
  (V3    z    0  (-x))
  (V3  (-y)   x    0 )
