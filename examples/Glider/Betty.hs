{-# OPTIONS_GHC -Wall #-}

module Glider.Betty
       ( bettyFc
       , bettyMc
       , bettyRefs
       , bettyInertia
       , bettyMass
       ) where

import Linear

import Glider.AeroCoeffs

bettyFc :: Floating a => AeroForceCoeffs a
bettyFc = AeroForceCoeffs
  { af_cL0 =  0.203530
  , af_cL_A = 5.786876

  , af_cD_A =  0.018751
  , af_cD_A2 =  1.529989
  , af_cD_B2 =  -0.16247
  , af_cD0 =  0.008767

  , af_cY_B = -0.239789

  --  control surface forces
  , af_cL_elev = -0.0105*180/pi
  , af_cL_flaps = 0.0184*180/pi
  , af_cY_rudder = 0.0035*180/pi
  , af_cD_flaps2 = 3.03874e-05,  af_cD_A_flaps = 0.000101404, af_cD_flaps = 0.000208995
  , af_cD_elev2 = 4.19816e-05, af_cD_A_elev = -9.79647e-05, af_cD_elev = 4.52856e-05
  , af_cD_ail2 = 5.60583e-05, af_cD_B_ail = -6.73139e-06, af_cD_ail = 0
  , af_cD_rudder2 = 2.03105e-05, af_cD_B_rudder = 5.55453e-05, af_cD_rudder = 0
  }

bettyMc :: Floating a => AeroMomentCoeffs a
bettyMc = AeroMomentCoeffs
  { am_cl_p = -0.576, am_cl_q =   0.0, am_cl_r =  0.0707
  , am_cm_p =    0.0, am_cm_q = -15.5, am_cm_r =     0.0
  , am_cn_p = -0.036, am_cn_q =   0.0, am_cn_r = -0.0667

  , am_cl_B = -0.051808
  , am_cl_AB = -0.208344
  , am_cm_A = -0.450643
    --  cm0 valid for CG/bridle location 0.1 meters behind main wing leading edge
  , am_cm0 = 0.028980
  , am_cn_B = 0.037183
  , am_cn_AB = -0.028933

    --  control surface moments
  , am_cl_ail = 0.0073*180/pi
  , am_cm_elev = 0.0352*180/pi
  , am_cm_flaps = 0.0026*180/pi
  , am_cn_rudder = 0.001176*180/pi
  }

bettyRefs :: Fractional a => AeroRefs a
bettyRefs = AeroRefs { ar_sref =  0.684
                    , ar_bref =  2.904 -- sqrt(sref*AR),
                    , ar_cref =  0.2512 -- sqrt(sref/AR),
                    }

bettyInertia :: Fractional a => M33 a
bettyInertia =
  V3
  (V3 0.565 0 0)
  (V3 0 0.161 0)
  (V3 0 0 0.723)

bettyMass :: Fractional a => a
bettyMass = 7.5
