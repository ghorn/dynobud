{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Dyno.DirectCollocation.Integrate
       ( withIntegrator
       ) where

import GHC.Generics ( Generic )

import qualified Control.Concurrent as CC
import Data.Proxy ( Proxy(..) )
import Data.Singletons.TypeLits (KnownNat, natVal, withKnownNat, pattern SNat)
import Data.Singletons.Prelude.Num (PNum((+)), (%+))
import Data.Vector ( Vector )
import qualified Data.Foldable as F

import Casadi.Matrix ( CMatrix )
import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.Viewable ( Viewable )

import Dyno.View.View ( View(..), J, S, JV, JTuple(..), splitJV, catJV, jfill, fmapJ )
import Dyno.View.Fun ( Fun, callSym, expandFun, toFun )
import Dyno.View.JVec ( JVec(..), jreplicate )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.M ( vcat, vsplit )
import qualified Dyno.View.M as M
import Dyno.Vectorize ( Vectorize(..), Id(..), None, unId, vzipWith )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )
import Dyno.Solvers ( Solver )
import Dyno.Nlp ( NlpIn(..), NlpOut(..) )
import Dyno.NlpSolver ( callNlpsol, toNlpSol )
import Dyno.DirectCollocation.Types ( CollStage(..), CollPoint(..) )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots, mkTaus, interpolate, timesFromTaus )

type Sxe = S SX

data IntegratorX x z n deg a =
  IntegratorX
  { ixStages :: J (JVec n (CollStage (JV x) (JV z) (JV None) (JV None) deg)) a
  , ixXf :: J (JV x) a
  } deriving (Generic)

data IntegratorP u p n deg a =
  IntegratorP
  { ipTf :: S a
  , ipParm :: J (JV p) a
  , ipU :: J (JVec n (JVec deg (JV u))) a
  } deriving (Generic)

data IntegratorG x r n deg a =
 IntegratorG
 { igCollPoints :: J (JVec n (JVec deg (JV r))) a
 , igContinuity :: J (JVec n (JV x)) a
 } deriving (Generic)


instance (Vectorize x, Vectorize z, KnownNat n, KnownNat deg)
         => View (IntegratorX x z n deg)
instance (Vectorize u, Vectorize p, KnownNat n, KnownNat deg)
         => View (IntegratorP u p n deg)
instance (Vectorize x, Vectorize r, KnownNat n, KnownNat deg)
         => View (IntegratorG x r n deg)


-- todo: code duplication
dot :: forall x deg a b. (View x, CMatrix a, Real b, KnownNat deg)
    => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unVec elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y


-- todo: code duplication
interpolateXDots' :: (Real b, View x, CMatrix a, KnownNat deg)
                  => Vec deg (Vec deg b) -> Vec deg (J x a) -> Vec deg (J x a)
interpolateXDots' cjks xs = fmap (`dot` xs) cjks

interpolateXDots ::
  forall b deg x a
  . (Real b, KnownNat deg, View x, CMatrix a)
  => Vec (deg + 1) (Vec (deg + 1) b)
  -> Vec (deg + 1) (J x a)
  -> Vec deg (J x a)
interpolateXDots cjks xs =
  withKnownNat (SNat @deg %+ SNat @1) $
  TV.tvtail $ interpolateXDots' cjks xs


-- return dynamics constraints, outputs, and interpolated state
dynStageConstraints' ::
  forall x z u p r deg . (KnownNat deg, View x, View z, View u, View p, View r)
  => Vec (deg + 1) (Vec (deg + 1) Double) -> Vec deg Double
  -> Fun (S :*: J p :*: J x :*: J (CollPoint x z u)) (J r)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: S :*: J p :*: J (JVec deg (JV Id))) MX
  -> (J (JVec deg r) :*: J x) MX
dynStageConstraints' cijs taus dynFun (x0 :*: xzs' :*: us' :*: h :*: p :*: stageTimes') =
  cat (JVec dynConstrs) :*: xnext
  where
    xzs = fmap split (unJVec (split xzs')) :: Vec deg (JTuple x z MX)
    us = unJVec (split us') :: Vec deg (J u MX)

    -- interpolated final state
    xnext :: J x MX
    xnext = interpolate taus x0 xs

    stageTimes = unJVec $ split stageTimes'

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    dynConstrs = TV.tvzipWith4 applyDae xdots xzs us stageTimes

    applyDae :: J x MX -> JTuple x z MX -> J u MX -> S MX -> J r MX
    applyDae x' (JTuple x z) u t = r
      where
        r = callSym dynFun (t :*: p :*: x' :*: collPoint)
        collPoint = cat (CollPoint x z u)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.ms` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap (\(JTuple x _) -> x) xzs


-- dynamics residual and outputs
dynamicsFunction' ::
  forall x z u p r a . (View x, View z, View u, Viewable a)
  => (J x a -> J x a -> J z a -> J u a -> J p a -> S a -> J r a)
  -> (S :*: J p :*: J x :*: J (CollPoint x z u)) a
  -> J r a
dynamicsFunction' dae (t :*: parm :*: x' :*: collPoint) = dae x' x z u parm t
  where
    CollPoint x z u = split collPoint

withIntegrator ::
  forall x z u p r deg n .
  (KnownNat n, KnownNat deg, Vectorize x, Vectorize p, Vectorize u, Vectorize z, Vectorize r)
  => Proxy n
  -> Proxy deg
  -> QuadratureRoots
  -> x Double
  -> (x Sxe -> x Sxe -> z Sxe -> u Sxe -> p Sxe -> Sxe -> r Sxe)
  -> Solver
  -> IO (x Double -> Either (u Double) (Vec n (Vec deg (u Double))) -> p Double -> Double -> IO (Either String (x Double)))
withIntegrator _ _ roots initialX dae solver = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus roots

      n = fromIntegral (natVal (Proxy :: Proxy n))

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (deg + 1) (Vec (deg + 1) Double)
      cijs =
        withKnownNat (SNat @deg %+ SNat @1) $
        lagrangeDerivCoeffs (0 TV.<| taus)

  dynFun <- flip (toFun "dynamics") mempty $ dynamicsFunction' $
            \x0 x1 x2 x3 x4 x5 ->
            let r = dae (vsplit x0) (vsplit x1) (vsplit x2) (vsplit x3)
                    (vsplit x4) (unId (vsplit x5))
            in vcat r

  dynStageConFun <- toFun "dynamicsStageCon" (dynStageConstraints' cijs taus dynFun) mempty
--  let callDynStageConFun = callSym dynStageConFun
  callDynStageConFun <- callSym <$> expandFun dynStageConFun

  let fg :: J (IntegratorX x z n deg) MX
            -> J (IntegratorP u p n deg) MX
            -> (S MX, J (IntegratorG x r n deg) MX)
      fg = getFgIntegrator taus callDynStageConFun

      scaleX = Nothing
      scaleG = Nothing
--        , nlpScaleX' = Just $ cat $ fillCollTraj
--                       (fromMaybe (fill 1) (ocpXScale ocp))
--                       (fromMaybe (fill 1) (ocpZScale ocp))
--                       (fromMaybe (fill 1) (ocpUScale ocp))
--                       (fromMaybe (fill 1) (ocpPScale ocp))
--                       (fromMaybe       1  (ocpTScale ocp))
--
--        , nlpScaleG' = Just $ cat $ fillCollConstraints
--                       (fromMaybe (fill 1) (ocpXScale ocp))
--                       (fromMaybe (fill 1) (ocpResidualScale ocp))

  let toParams :: Either (u Double) (Vec n (Vec deg (u Double)))
                  -> p Double
                  -> Double
                  -> J (IntegratorP u p n deg) (Vector Double)
      toParams us p tf =
        cat $
        IntegratorP
        { ipTf = catJV (Id tf)
        , ipParm = catJV p
        , ipU = case us of
          Left u -> jreplicate (jreplicate (catJV u))
          Right us' -> cat $ JVec $ fmap (cat . JVec . fmap catJV) us'
        }

  let toBounds :: x Double -> J (IntegratorX x z n deg) (Vector (Maybe Double))
      toBounds x0 =
        cat $
        IntegratorX
        { ixStages = cat $ JVec $ TV.mkVec' $ take n $ xs0 : repeat (jfill Nothing)
        , ixXf = jfill Nothing
        }
        where
          xs0 :: J (CollStage (JV x) (JV z) (JV None) (JV None) deg) (Vector (Maybe Double))
          xs0 = cat $ CollStage (catJV (fmap Just x0)) (jfill Nothing) (jfill Nothing) (jfill Nothing)

  nlpSol <- toNlpSol "collocation_integrator" solver fg scaleX scaleG Nothing Nothing

  let initialGuess = cat $
          IntegratorX
          { ixStages = jreplicate $ cat $
                       CollStage initialX' (jreplicate point) (jfill 0) (jfill 0)
          , ixXf = initialX'
          }
        where
          initialX' :: J (JV x) (Vector Double)
          initialX' = catJV initialX
          point = cat $ CollPoint initialX' (jfill 0) (jfill 0)

  initialGuessMVar <- CC.newMVar initialGuess

  let doAnIntegration :: x Double -> Either (u Double) (Vec n (Vec deg (u Double))) -> p Double -> Double -> IO (Either String (x Double))
      doAnIntegration x0 us p tf = do
        xguess <- CC.readMVar initialGuessMVar
        let bx = toBounds x0
            inputs =
              NlpIn
              { nlpX0 = xguess
              , nlpBG = jfill (Just 0, Just 0)
              , nlpP = toParams us p tf
              , nlpBX = fmapJ (\x -> (x, x)) bx
              , nlpLamX0 = Nothing
              , nlpLamG0 = Nothing
              }
        (_, ret) <- callNlpsol nlpSol inputs
        case ret of
          Left msg -> return $ Left $ "failed solving with error: \"" ++ msg ++ "\""
          Right nlpOut -> do
            let xtopt = xOpt nlpOut
            _ <- CC.swapMVar initialGuessMVar xtopt
            return $ Right $ splitJV (ixXf (split xtopt))

  return doAnIntegration


getFgIntegrator ::
  forall x z u p r n deg .
  (KnownNat deg, KnownNat n, Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r)
  => Vec deg Double
  -> ((J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u)) :*: S :*: J (JV p) :*: J (JVec deg (JV Id))) MX -> (J (JVec deg (JV r)) :*: J (JV x)) MX)
  -> J (IntegratorX x z n deg) MX
  -> J (IntegratorP u p n deg) MX
  -> (S MX, J (IntegratorG x r n deg) MX)
getFgIntegrator taus stageFun ix' ip' = (0, cat g)
  where
    ix = split ix'
    ip = split ip'

    xf = ixXf ix
    tf = ipTf ip
    parm = ipParm ip
    stages = unJVec (split (ixStages ix)) :: Vec n (J (CollStage (JV x) (JV z) (JV None) (JV None) deg) MX)

    spstages :: Vec n (CollStage (JV x) (JV z) (JV None) (JV None) deg MX)
    spstages = fmap split stages

    us :: Vec n (J (JVec deg (JV u)) MX)
    us = unJVec $ split $ ipU ip

    -- timestep
    dt = tf / fromIntegral n
    n = natVal (Proxy :: Proxy n)

    -- times at each collocation point
    times :: Vec n (Vec deg (S MX))
    times = fmap snd $ timesFromTaus 0 (fmap realToFrac taus) dt

    times' :: Vec n (J (JVec deg (JV Id)) MX)
    times' = fmap (cat . JVec) times

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    x0s = fmap (\(CollStage x0' _ _ _) -> x0') spstages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (J (JV x) MX)
    xfs = TV.tvshiftl x0s xf

    g = IntegratorG
        { igCollPoints = cat $ JVec dcs
        , igContinuity = cat $ JVec integratorMatchingConstraints
        }
    integratorMatchingConstraints :: Vec n (J (JV x) MX) -- todo: THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = vzipWith (-) interpolatedXs xfs

    dcs :: Vec n (J (JVec deg (JV r)) MX)
    interpolatedXs :: Vec n (J (JV x) MX)
    (dcs, interpolatedXs) = TV.tvunzip $ TV.tvzipWith3 fff spstages us times'
    fff :: CollStage (JV x) (JV z) (JV None) (JV None) deg MX
           -> J (JVec deg (JV u)) MX
           -> J (JVec deg (JV Id)) MX
           -> (J (JVec deg (JV r)) MX, J (JV x) MX)
    fff (CollStage x0' xzs' _ _) us' stageTimes = (dc, interpolatedX')
      where
        dc :*: interpolatedX' = stageFun (x0' :*: xzs :*: us' :*: dt :*: parm :*: stageTimes)

        xzs :: J (JVec deg (JTuple (JV x) (JV z))) MX
        xzs = cat $ JVec $ fmap toTuple $ unJVec $ split xzs'
        toTuple xzu = cat (JTuple x z)
          where
            CollPoint x z _ = split xzu
