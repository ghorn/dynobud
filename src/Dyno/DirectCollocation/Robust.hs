{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Dyno.DirectCollocation.Robust
       ( CovarianceSensitivities(..)
       , CovTraj(..)
       , mkComputeSensitivities
       , mkComputeCovariances
       , mkRobustifyFunction
       , continuousToDiscreetNoiseApprox
       ) where

import GHC.Generics ( Generic, Generic1 )
import GHC.TypeLits
import GHC.TypeLits.Witnesses

import Data.Proxy ( Proxy(..) )
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.DM ( DM )
import Casadi.Viewable ( Viewable )

import Dyno.View.Unsafe ( mkM )

import Dyno.View.View ( View(..), J, S, JV, JTuple(..) )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.Cov ( Cov, toMat, fromMat )
import Dyno.View.Fun
import Dyno.View.M ( M, vcat, vsplit )
import qualified Dyno.View.M as M
import Dyno.View.JVec ( JVec(..) )
import Dyno.View.Scheme ( Scheme )
import Dyno.View.Vectorize ( Vectorize(..), Id(..), unId, vzipWith4 )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..), mkTaus, interpolate )

data CovTraj sx n a =
  CovTraj
  { ctAllButLast :: J (JVec n (Cov (JV sx))) a
  , ctLast :: J (Cov (JV sx)) a
  } deriving (Eq, Show, Generic, Generic1)
instance (Vectorize sx, KnownNat n) => View (CovTraj sx n)


data CovarianceSensitivities xe we n a =
  CovarianceSensitivities
  { csFs :: M (JVec n xe) xe a
  , csWs :: M (JVec n xe) we a
  } deriving (Eq, Show, Generic, Generic1)
instance (View xe, View we, KnownNat n) => Scheme (CovarianceSensitivities xe we n)

type Sxe = S SX

mkComputeSensitivities ::
  forall x z u p sx sz sw sr deg n .
  ( KnownNat deg, KnownNat n, Vectorize x, Vectorize p, Vectorize u, Vectorize z
  , Vectorize sr, Vectorize sw, Vectorize sz, Vectorize sx
  )
  => QuadratureRoots
  -> (x Sxe -> x Sxe -> z Sxe -> u Sxe -> p Sxe -> Sxe
      -> sx Sxe -> sx Sxe -> sz Sxe -> sw Sxe
      -> sr Sxe)
  -> IO (J (CollTraj x z u p n deg) MX -> CovarianceSensitivities (JV sx) (JV sw) n MX)
mkComputeSensitivities roots covDae = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus roots

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (deg + 1) (Vec (deg + 1) Double)
      cijs =
        withNatOp (%+) (Proxy :: Proxy deg) (Proxy :: Proxy 1) $
        lagrangeDerivCoeffs (0 TV.<| taus)

  errorDynFun <- flip (toFun "error dynamics") mempty $ errorDynamicsFunction $
            \x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 ->
            let r = covDae
                    (vsplit x0) (vsplit x1) (vsplit x2) (vsplit x3) (vsplit x4)
                    (unId (vsplit x5)) (vsplit x6) (vsplit x7) (vsplit x8) (vsplit x9)
            in vcat r

  let edscf :: (J (ErrorInD (JV sx) (JV sw) (JV sz) deg) :*:
                (ErrorIn0 (JV x) (JV z) (JV u) (JV p) deg)) MX
            -> M (ErrorOut (JV sr) (JV sx) deg)
                 (ErrorInD (JV sx) (JV sw) (JV sz) deg)
                 MX
      edscf (inputD :*: input0) = M.jacobian edo inputD
        where
          edo = errorDynStageConstraints cijs taus errorDynFun inputD input0

  errorDynStageConFunJac <- toFun "errorDynamicsStageCon" edscf mempty

  let _ = errorDynStageConFunJac :: Fun
                                    (J (ErrorInD (JV sx) (JV sw) (JV sz) deg) :*:
                                     (ErrorIn0 (JV x) (JV z) (JV u) (JV p) deg))
                                    (M (ErrorOut (JV sr) (JV sx) deg)
                                     (ErrorInD (JV sx) (JV sw) (JV sz) deg))

  sensitivityStageFun <- flip (toFun "sensitivity stage function") mempty $
                         sensitivityStageFunction (\x y -> callSym errorDynStageConFunJac (cat x :*: y))

  let sens :: S MX
              -> J (JV p) MX
              -> J (JVec deg (JV Id)) MX
              -> J (JV x) MX
              -> J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX
              -> (M (JV sx) (JV sx) MX, M (JV sx) (JV sw) MX)
      sens dt p stagetimes x0 xzus = (y0,y1)
        where
          y0 :*: y1 = callSym sensitivityStageFun (dt :*: p :*: stagetimes :*: x0 :*: xzus)

  let computeAllSensitivities :: J (CollTraj x z u p n deg) MX
             -> CovarianceSensitivities (JV sx) (JV sw) n MX
      computeAllSensitivities collTraj = CovarianceSensitivities (M.vcat' fs) (M.vcat' ws)
        where
          -- split up the design vars
          CollTraj _ _ stages' _ = split collTraj
          stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) (JV p) deg) MX)
          spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) (JV p) deg MX)

          -- timestep
          n = fromIntegral (natVal (Proxy :: Proxy n))

          -- times at each collocation point
          times :: Int -> S MX -> Vec deg (S MX)
          times k dt = fmap (\tau -> t0 + realToFrac tau * dt) taus
            where
              t0 = dt * fromIntegral k

          ks :: Vec n Int
          ks = TV.mkVec' (take n [(0::Int)..])

          fs :: Vec n (M (JV sx) (JV sx) MX)
          ws :: Vec n (M (JV sx) (JV sw) MX)
          (fs, ws) = TV.tvunzip $ mkFw <$> ks <*> spstages
          mkFw k (CollStage x0' xzus' parm tf) = sens dt parm stagetimes x0' xzus'
            where
              stagetimes :: J (JVec deg (JV Id)) MX
              stagetimes = cat $ JVec $ times k dt
              dt = tf / fromIntegral n

  return computeAllSensitivities
--  toFun "compute all sensitivities" computeAllSensitivities mempty


-- todo: calculate by first multiplying all the Fs
mkComputeCovariances ::
  forall x z u p sx sw n deg .
  ( KnownNat deg, KnownNat n
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Vectorize sx, Vectorize sw
  )
  => (M (JV sx) (JV sx) MX -> M (JV sx) (JV sw) MX -> J (Cov (JV sw)) MX -> S MX
      -> M (JV sx) (JV sx) MX)
  -> (J (CollTraj x z u p n deg) MX -> CovarianceSensitivities (JV sx) (JV sw) n MX)
  -> J (Cov (JV sw)) DM
  -> IO (J (Cov (JV sx)) MX -> J (CollTraj x z u p n deg) MX ->  J (CovTraj sx n) MX)
mkComputeCovariances c2d computeSens qc' = do
  propOneCovFun <- toFun "propogate one covariance" (propOneCov c2d) mempty

  let computeCovs :: J (Cov (JV sx)) MX -> J (CollTraj x z u p n deg) MX ->  J (CovTraj sx n) MX
      computeCovs p0 collTraj = cat covTraj
        where
          sensitivities = computeSens collTraj

          covTraj =
            CovTraj
            { ctAllButLast = cat (JVec covs)
            , ctLast = pF
            }

          covs :: Vec n (J (Cov (JV sx)) MX) -- all but last covariances
          pF :: J (Cov (JV sx)) MX -- last covariances
          (pF, covs) = T.mapAccumL ffs p0 $
                           TV.tvzip (M.vsplit' (csFs sensitivities)) (M.vsplit' (csWs sensitivities))

          qc :: J (Cov (JV sw)) MX
          qc = M.fromDM qc'

          ffs :: J (Cov (JV sx)) MX
                 -> (M (JV sx) (JV sx) MX, M (JV sx) (JV sw) MX)
                -> (J (Cov (JV sx)) MX, J (Cov (JV sx)) MX)
          ffs p0' (f, g) = (p1', p0')
            where
              p1' = callSym propOneCovFun (f :*: g :*: p0' :*: qc :*: dt)

          -- split up the design vars
          CollTraj tf _ _ _ = split collTraj

          -- timestep
          dt = tf / fromIntegral n
          n = natVal (Proxy :: Proxy n)

  return computeCovs
--  toFun "compute all covariances" computeCovs mempty

-- todo: code duplication
dot :: forall x deg a b. (Fractional (J x a), Real b, KnownNat deg) => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unVec elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y

-- todo: code duplication
interpolateXDots' :: (Real b, Fractional (J x a), KnownNat deg) => Vec deg (Vec deg b) -> Vec deg (J x a) -> Vec deg (J x a)
interpolateXDots' cjks xs = fmap (`dot` xs) cjks

interpolateXDots ::
  forall b deg x a
  . (Real b, KnownNat deg, Fractional (J x a))
  => Vec (deg + 1) (Vec (deg + 1) b)
  -> Vec (deg + 1) (J x a)
  -> Vec deg (J x a)
interpolateXDots cjks xs =
  withNatOp (%+) (Proxy :: Proxy deg) (Proxy :: Proxy 1) $
  TV.tvtail $ interpolateXDots' cjks xs


-- dynamics residual and outputs
errorDynamicsFunction ::
  forall x z u p r sx sz sw a .
  (View x, View z, View u, Viewable a)
  => (J x a -> J x a -> J z a -> J u a -> J p a -> S a
      -> J sx a -> J sx a -> J sz a -> J sw a -> J r a)
  -> (S :*: J p :*: J x :*: J (CollPoint x z u) :*: J sx :*: J sx :*: J sz :*: J sw) a
  -> J r a
errorDynamicsFunction dae (t :*: parm :*: x' :*: collPoint :*: sx' :*: sx :*: sz :*: sw) =
  r
  where
    CollPoint x z u = split collPoint
    r = dae x' x z u parm t sx' sx sz sw


data ErrorIn0 x z u p deg a =
  ErrorIn0 (J x a) (J (JVec deg (CollPoint x z u)) a) (S a) (J p a) (J (JVec deg (JV Id)) a)
  deriving Generic

data ErrorInD sx sw sz deg a =
  ErrorInD (J sx a) (J sw a) (J (JVec deg (JTuple sx sz)) a)
  deriving Generic

data ErrorOut sr sx deg a =
  ErrorOut (J (JVec deg sr) a) (J sx a)
  deriving Generic

instance (View x, View z, View u, View p, KnownNat deg) => Scheme (ErrorIn0 x z u p deg)
instance (View sx, View sw, View sz, KnownNat deg) => View (ErrorInD sx sw sz deg)
instance (View sr, View sx, KnownNat deg) => View (ErrorOut sr sx deg)

-- return error dynamics constraints and interpolated state
errorDynStageConstraints ::
  forall x z u p sx sz sw sr deg .
  (KnownNat deg, View x, View z, View u, View p,
   View sr, View sw, View sz, View sx)
  => Vec (deg + 1) (Vec (deg + 1) Double)
  -> Vec deg Double
  -> Fun (S :*: J p :*: J x :*: J (CollPoint x z u) :*: J sx :*: J sx :*: J sz :*: J sw)
           (J sr)
  -> J (ErrorInD sx sw sz deg) MX
  -> ErrorIn0 x z u p deg MX
  -> J (ErrorOut sr sx deg) MX
errorDynStageConstraints cijs taus dynFun errorInD (ErrorIn0 x0 xzus' h p stageTimes')
  = cat (ErrorOut (cat (JVec dynConstrs)) sxnext)
  where
    ErrorInD sx0 sw0 sxzs' = split errorInD

    xzus = unJVec (split xzus')

    xs :: Vec deg (J x MX)
    xs = fmap ((\(CollPoint x _ _) -> x) . split) xzus

    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.ms` (1 / h)) $ interpolateXDots cijs (x0 TV.<| xs)

--    -- interpolated final state
--    xnext :: J x MX
--    xnext = interpolate taus x0 xs

    -- interpolated final state
    sxnext :: J sx MX
    sxnext = interpolate taus sx0 sxs

    stageTimes = unJVec $ split stageTimes'

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J sr MX)
    dynConstrs = TV.tvzipWith6 applyDae sxdots sxs szs xdots xzus stageTimes

    applyDae
      :: J sx MX -> J sx MX -> J sz MX
         -> J x MX -> J (CollPoint x z u) MX -> S MX
         -> J sr MX
    applyDae sx' sx sz x' xzu t =
      callSym dynFun
      (t :*: p :*: x' :*: xzu :*: sx' :*: sx :*: sz :*: sw0)

    -- error state derivatives
    sxdots :: Vec deg (J sx MX)
    sxdots = fmap (`M.ms` (1/h)) $ interpolateXDots cijs (sx0 TV.<| sxs)

    sxs :: Vec deg (J sx MX)
    szs :: Vec deg (J sz MX)
    (sxs, szs) = TV.tvunzip
                 $ fmap ((\(JTuple sx sz) -> (sx,sz)) . split)
                 $ unJVec $ split sxzs'


continuousToDiscreetNoiseApprox :: (View sx, View sw)
       => M sx sx MX -> M sx sw MX -> J (Cov sw) MX -> S MX -> M sx sx MX
continuousToDiscreetNoiseApprox _dsx1_dsx0 dsx1_dsw0 qs h = qd
  where
    -- Qs' = G * Qs * G.T
    qs' = dsx1_dsw0 `M.mm` (toMat qs) `M.mm` M.trans dsx1_dsw0

    qd = qs' `M.ms` (1/h)
--         + (dsx1_dsx0 `M.mm` qs' + qs' `M.mm` (M.trans dsx1_dsx0)) `M.ms` (h*h/2)
--         + (dsx1_dsx0 `M.mm` qs' `M.mm` (M.trans dsx1_dsx0)) `M.ms` (h*h*h/3)


propOneCov ::
  forall sx sw
  . View sx
  => (M sx sx MX -> M sx sw MX -> J (Cov sw) MX -> S MX -> M sx sx MX)
  -> (M sx sx :*: M sx sw :*: J (Cov sx) :*: J (Cov sw) :*: S) MX
  -> J (Cov sx) MX
propOneCov c2d (dsx1_dsx0 :*: dsx1_dsw0 :*: p0 :*: qs :*: h) = fromMat p1
  where
    qd = c2d dsx1_dsx0 dsx1_dsw0 qs h

    p1 :: M sx sx MX
    p1 = dsx1_dsx0 `M.mm` (toMat p0) `M.mm` M.trans dsx1_dsx0 + qd


sensitivityStageFunction ::
  forall x z u p sx sz sw deg sr
  . (KnownNat deg, View x, View z, View u, View p, View sx, View sz, View sw, View sr)
  => (ErrorInD sx sw sz deg MX -> ErrorIn0 x z u p deg MX
      -> M (ErrorOut sr sx deg) (ErrorInD sx sw sz deg) MX)
  -> (S :*: J p :*: J (JVec deg (JV Id)) :*: J x :*: J (JVec deg (CollPoint x z u))) MX
  -> (M sx sx :*: M sx sw) MX
sensitivityStageFunction dynStageConJac
  (dt :*: parm :*: stageTimes :*: x0' :*: xzus') = dsx1_dsx0 :*: dsx1_dsw0
  where
    sx0 :: J sx MX
    sx0  = M.zeros
    sw0 :: J sw MX
    sw0  = M.zeros
    sxzs :: J (JVec deg (JTuple sx sz)) MX
    sxzs = M.zeros

    mat :: M.M (ErrorOut sr sx deg) (ErrorInD sx sw sz deg) MX
    mat =
      dynStageConJac
      (ErrorInD sx0 sw0 sxzs)
      (ErrorIn0 x0' xzus' dt parm stageTimes)

    df_dsx0 :: M (JVec deg sr) sx MX
    df_dsw0 :: M (JVec deg sr) sw MX
    df_dsxz :: M (JVec deg sr) (JVec deg (JTuple sx sz)) MX
    dg_dsx0 :: M sx sx MX
    dg_dsw0 :: M sx sw MX
    dg_dsxz :: M sx (JVec deg (JTuple sx sz)) MX
    ((df_dsx0, df_dsw0, df_dsxz), (dg_dsx0, dg_dsw0, dg_dsxz)) =
      case fmap F.toList (F.toList (M.blocksplit mat)) of
      [[x00,x01,x02],[x10,x11,x12]] -> ((mkM x00, mkM x01, mkM x02),
                                        (mkM x10, mkM x11, mkM x12))
      _ -> error "stageFunction: got wrong number of elements in jacobian"

    -- TODO: this should be much simpler for radau

    -- TODO: check these next 4 lines
    dsxz_dsx0 = - (M.solve' df_dsxz df_dsx0) :: M (JVec deg (JTuple sx sz)) sx MX
    dsxz_dsw0 = - (M.solve' df_dsxz df_dsw0) :: M (JVec deg (JTuple sx sz)) sw MX

    dsx1_dsx0 = dg_dsx0 + dg_dsxz `M.mm` dsxz_dsx0 :: M sx sx MX
    dsx1_dsw0 = dg_dsw0 + dg_dsxz `M.mm` dsxz_dsw0 :: M sx sw MX


mkRobustifyFunction ::
  forall x sx shr p .
  (Vectorize x, Vectorize sx, Vectorize shr, Vectorize p)
  => (x Sxe -> sx Sxe -> x Sxe)
  -> (x Sxe -> sx Sxe -> p Sxe -> shr Sxe)
  -> IO (J (JV shr) MX -> J (JV p) MX -> J (JV x) MX -> J (Cov (JV sx)) MX -> J (JV shr) MX)
mkRobustifyFunction project robustifyPathC = do
  let proj :: (J (JV sx) :*: J (JV x)) SX
           -> M (JV x) (JV sx) SX
      proj (x0 :*: x1) = M.jacobian (vcat (project (vsplit x1) (vsplit x0))) x0

  projJac <- toFun "errorSpaceProjectionJac" proj mempty
  let _ = projJac :: Fun
                     (J (JV sx) :*: J (JV x))
                     (M (JV x) (JV sx))

  let zerosx = M.zeros :: J (JV sx) SX
  simplifiedPropJac <- toFun "simplified error space projection jacobian"
                       (\x0 -> callSym projJac (zerosx :*: x0))
                       mempty
  let _ = simplifiedPropJac :: Fun
                               (J (JV x))
                               (M.M (JV x) (JV sx))

  let rpc :: (J (JTuple (JV x) (JV sx)) :*: J (JV p)) SX
          -> (M (JV shr) (JTuple (JV x) (JV sx)) :*: J (JV shr)) SX
      rpc (xe :*: parm) = M.jacobian lol xe :*: lol
        where
          lol :: J (JV shr) SX
          lol = vcat $ robustifyPathC (vsplit x) (vsplit e) (vsplit parm)
          JTuple x e = split xe
  robustHJac <- toFun "robust constraint" rpc mempty
  let _ = robustHJac :: Fun (J (JTuple (JV x) (JV sx)) :*: J (JV p))
                            (M (JV shr) (JTuple (JV x) (JV sx)) :*: J (JV shr))

  let srh :: (J (JV x) :*: J (JV p)) SX -> (M (JV shr) (JTuple (JV x) (JV sx)) :*: J (JV shr)) SX
      srh (x :*: p) = ret
        where

          xe = M.zeros :: J (JV sx) SX
          xxe = cat (JTuple x xe) :: J (JTuple (JV x) (JV sx)) SX

          ret :: (M (JV shr) (JTuple (JV x) (JV sx)) :*: J (JV shr)) SX
          ret = callSym robustHJac (xxe :*: p)

  simplifiedHJac <- toFun "simplified robust constraint jacobian" srh mempty
  let _ = simplifiedHJac :: Fun
                            (J (JV x) :*: J (JV p))
                            (M (JV shr) (JTuple (JV x) (JV sx)) :*: J (JV shr))

  let gogo :: J (JV shr) MX -> J (JV p) MX -> J (JV x) MX -> J (Cov (JV sx)) MX -> J (JV shr) MX
      gogo gammas' theta x pe' = rcs'
          where
            gammas = vsplit gammas' :: shr (S MX)

            jHx :: M (JV shr) (JV x) MX
            jHe :: M (JV shr) (JV sx) MX
            (jHx, jHe) = M.hsplitTup jacH'

            jacH' :: M (JV shr) (JTuple (JV x) (JV sx)) MX
            h0vec :: J (JV shr) MX
            jacH' :*: h0vec = callSym simplifiedHJac (x :*: theta)

            f :: M.M (JV x) (JV sx) MX
            f = callSym simplifiedPropJac x

            pe :: M.M (JV sx) (JV sx) MX
            pe = toMat pe'

            fpef :: M.M (JV x) (JV x) MX
            fpef = fpe `M.mm` (M.trans f)

            fpe :: M.M (JV x) (JV sx) MX
            fpe = f `M.mm` pe

            jHxs :: shr (M.M (JV Id) (JV x) MX)
            jHxs = M.vsplit jHx

            jHes :: shr (M.M (JV Id) (JV sx) MX)
            jHes = M.vsplit jHe

            shr' = vsplit h0vec :: shr (S MX)

            rcs' :: J (JV shr) MX
            rcs' = vcat rcs

            rcs :: shr (S MX)
            rcs = vzipWith4 robustify gammas shr' jHxs jHes

            robustify :: S MX
                         -> S MX
                         -> M.M (JV Id) (JV x) MX
                         -> M.M (JV Id) (JV sx) MX
                         -> S MX
            robustify gamma h0 gHx gHe = h0 + gamma * sqrt sigma2
              where
                sigma2 :: S MX
                sigma2 =
                  gHx `M.mm` fpef `M.mm` (M.trans gHx) +
                  2 * gHx `M.mm` fpe `M.mm` (M.trans gHe) +
                  gHe `M.mm` pe `M.mm` (M.trans gHe)

  retFun <- toFun "robust constraint violations"
            (\(x0 :*: x1 :*: x2 :*: x3) -> gogo x0 x1 x2 x3) mempty -- >>= expandFun

  return (\x y z w -> callSym retFun (x :*: y :*: z :*: w))
