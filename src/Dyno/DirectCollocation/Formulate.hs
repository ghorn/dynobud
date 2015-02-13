{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Formulate
       ( CovTraj(..)
       , CollProblem(..)
       , CollCovProblem(..)
       , makeCollProblem
       , makeCollCovProblem
       , mkTaus
       , makeGuess
       , makeGuessSim
       ) where

import GHC.Generics ( Generic )

import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Matrix hiding ( trace )
import Linear.V

import Casadi.DMatrix ( DMatrix )
import Casadi.MX ( MX )

import Dyno.SXElement ( sxCatJV, sxSplitJV )
import Dyno.View.View ( View(..), J, jfill, JTuple(..), JNone(..), v2d, d2v )
import qualified Dyno.View.M as M
import Dyno.View.Cov ( Cov )
import Dyno.View.JV ( JV, splitJV, catJV, catJV' )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.Fun
import Dyno.View.JVec( JVec(..), jreplicate )
import Dyno.View.Viewable ( Viewable )
import Dyno.View.Scheme ( Scheme )
import Dyno.Vectorize ( Vectorize(..), Id(..), fill, vlength, vzipWith )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )
import Dyno.Nlp ( Nlp'(..), Bounds )
import Dyno.Ocp ( OcpPhase(..), OcpPhaseWithCov(..) )

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( DynPlotPoints, dynPlotPoints )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..), mkTaus, interpolate, timesFromTaus )
import Dyno.DirectCollocation.Robust

data CollProblem x z u p r c h o n deg =
  CollProblem
  { cpNlp :: Nlp' (CollTraj x z u p n deg) JNone (CollOcpConstraints n deg x r c h) MX
  , cpOcp :: OcpPhase x z u p r o c h
  , cpPlotPoints :: J (CollTraj x z u p n deg) (Vector Double) -> IO (DynPlotPoints Double)
  , cpOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                 -> IO (Vec n (Vec deg (o Double, x Double), x Double))
  , cpTaus :: Vec deg Double
  , cpRoots :: QuadratureRoots
  }

makeCollProblem ::
  forall x z u p r o c h deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize r, Vectorize o, Vectorize h, Vectorize c)
  => OcpPhase x z u p r o c h
  -> IO (CollProblem x z u p r c h o n deg)
makeCollProblem ocp = do
  let -- the collocation points
      roots :: QuadratureRoots
      roots = Legendre

      taus :: Vec deg Double
      taus = mkTaus roots

      n = reflectDim (Proxy :: Proxy n)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
      cijs = lagrangeDerivCoeffs (0 TV.<| taus)

      interpolate' :: (J (JV x) :*: J (JVec deg (JV x))) MX -> J (JV x) MX
      interpolate' (x0 :*: xs) = interpolate taus x0 (unJVec (split xs))
      interpolateScalar' :: (J (JV Id) :*: J (JVec deg (JV Id))) MX -> J (JV Id) MX
      interpolateScalar' (x0 :*: xs) = interpolate taus x0 (unJVec (split xs))

  interpolateFun <- toMXFun "interpolate (JV x)" interpolate' >>= expandMXFun
  interpolateScalarFun <- toMXFun "interpolate (JV Id)" interpolateScalar' >>= expandMXFun
  let callInterpolateScalar :: J (JV Id) MX -> Vec deg (J (JV Id) MX) -> J (JV Id) MX
      callInterpolateScalar x0 xs = call interpolateScalarFun (x0 :*: cat (JVec xs))

      callInterpolate :: J (JV x) MX -> Vec deg (J (JV x) MX) -> J (JV x) MX
      callInterpolate x0 xs = call interpolateFun (x0 :*: cat (JVec xs))

  bcFun <- toSXFun "bc" $ \(x0:*:x1) -> sxCatJV $ ocpBc ocp (sxSplitJV x0) (sxSplitJV x1)
  mayerFun <- toSXFun "mayer" $ \(x0:*:x1:*:x2) ->
    sxCatJV $ Id $ ocpMayer ocp (unId (sxSplitJV x0)) (sxSplitJV x1) (sxSplitJV x2)
  lagrangeFun <- toSXFun "lagrange" $ \(x0:*:x1:*:x2:*:x3:*:x4:*:x5:*:x6) ->
    sxCatJV $ Id $ ocpLagrange ocp (sxSplitJV x0) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (sxSplitJV x4) (unId (sxSplitJV x5)) (unId (sxSplitJV x6))
  quadFun <- toMXFun "quadratures" $ evaluateQuadraturesFunction lagrangeFun callInterpolateScalar cijs n
--  let callQuadFun = call quadFun
  callQuadFun <- fmap call (expandMXFun quadFun)

  dynFun <- toSXFun "dynamics" $ dynamicsFunction $
            \x0 x1 x2 x3 x4 x5 ->
            let (r,o) = ocpDae ocp (sxSplitJV x0) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (sxSplitJV x4) (unId (sxSplitJV x5))
            in (sxCatJV r, sxCatJV o)

  pathConFun <- toSXFun "pathConstraints" $ pathConFunction $
                \x0 x1 x2 x3 x4 x5 -> sxCatJV $ ocpPathC ocp (sxSplitJV x0) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (sxSplitJV x4) (unId (sxSplitJV x5))
  pathStageConFun <- toMXFun "pathStageCon" (pathStageConstraints pathConFun)

  dynStageConFun <- toMXFun "dynamicsStageCon" (dynStageConstraints callInterpolate cijs dynFun)

  stageFun <- toMXFun "stageFunction" $ stageFunction pathStageConFun (call dynStageConFun)
--  let callStageFun = call stageFun
  callStageFun <- fmap call (expandMXFun stageFun)

  outputFun <- toMXFun "stageOutputs" $ outputFunction callInterpolate cijs taus dynFun

  -- prepare callbacks
  let nlpX0 = jfill 0 :: J (CollTraj x z u p n deg) (Vector Double)

      f :: J (JV o) DMatrix ->  J (JV x) DMatrix
           -> (J (JV o) (Vector Double), J (JV x) (Vector Double))
      f o' x' = (d2v o', d2v x')

      callOutputFun :: J (JV p) (Vector Double)
                       -> J (JV Id) (Vector Double)
                       -> J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double)
                       -> J (JV Id) (Vector Double)
                       -> IO (Vec deg (J (JV o) (Vector Double), J (JV x) (Vector Double)), J (JV x) (Vector Double))
      callOutputFun p h stage k = do
        (_ :*: xdot :*: out :*: xnext) <-
          eval outputFun $ (v2d stage) :*: (v2d p) :*: (v2d h) :*: (v2d k)
        let outs0 = unJVec (split out) :: Vec deg (J (JV o) DMatrix)
            xdots0 = unJVec (split xdot) :: Vec deg (J (JV x) DMatrix)
        return (TV.tvzipWith f outs0 xdots0, d2v xnext)

      mapOutputFun :: J (CollTraj x z u p n deg) (Vector Double)
                      -> IO (Vec n (Vec deg (J (JV o) (Vector Double), J (JV x) (Vector Double)), J (JV x) (Vector Double)))
      mapOutputFun ct = do
        let CollTraj tf p stages _ = split ct
            h = catJV $ Id (tf' / fromIntegral n)
              where
                Id tf' = splitJV tf

            vstages = unJVec (split stages)
                :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
            ks :: Vec n (J (JV Id) (Vector Double))
            ks = TV.mkVec' $ map (catJV . Id . realToFrac) (take n [(0::Int)..])

        T.sequence $ TV.tvzipWith (callOutputFun p h) vstages ks

      getPlotPoints :: J (CollTraj x z u p n deg) (Vector Double) -> IO (DynPlotPoints Double)
      getPlotPoints traj = do
        outputs <- mapOutputFun traj
        return (dynPlotPoints roots (split traj) outputs)

      getOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                    -> IO (Vec n (Vec deg (o Double, x Double), x Double))
      getOutputs traj = do
        outputs <- mapOutputFun traj
        let devec :: Vec deg (J (JV o) (Vector Double), J (JV x) (Vector Double))
                  -> Vec deg (o Double, x Double)
            devec = fmap (\(x,y) -> (splitJV x, splitJV y))
        return $ fmap (\(x,y) -> (devec x, splitJV y)) outputs

  let nlp = Nlp' {
        nlpFG' =
           getFg taus
           (bcFun :: SXFun (J (JV x) :*: J (JV x)) (J (JV c)))
           (mayerFun :: SXFun (J (JV Id) :*: (J (JV x) :*: (J (JV x)))) (J (JV Id)))
           (callQuadFun :: (J (JV p) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (JVec deg (JV o)) :*: J (JV Id) :*: J (JVec deg (JV Id))) MX
                        -> J (JV Id) MX)
           (callStageFun :: (J (JV Id) :*: J (JV p) :*: J (JVec deg (JV Id)) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u))) MX
                      -> (J (JVec deg (JV r)) :*: J (JVec deg (JV o)) :*: J (JVec deg (JV h)) :*: J (JV x)) MX)
        , nlpBX' = cat $ fillCollTraj
                   (ocpXbnd ocp)
                   (ocpZbnd ocp)
                   (ocpUbnd ocp)
                   (ocpPbnd ocp)
                   (ocpTbnd ocp)
        , nlpBG' = cat (getBg ocp)
        , nlpX0' = nlpX0
        , nlpP' = cat JNone
        , nlpLamX0' = Nothing
        , nlpLamG0' = Nothing
        , nlpScaleF' = ocpObjScale ocp
        , nlpScaleX' = Just $ cat $ fillCollTraj
                       (fromMaybe (fill 1) (ocpXScale ocp))
                       (fromMaybe (fill 1) (ocpZScale ocp))
                       (fromMaybe (fill 1) (ocpUScale ocp))
                       (fromMaybe (fill 1) (ocpPScale ocp))
                       (fromMaybe       1  (ocpTScale ocp))

        , nlpScaleG' = Just $ cat $ fillCollConstraints
                       (fromMaybe (fill 1) (ocpXScale ocp))
                       (fromMaybe (fill 1) (ocpResidualScale ocp))
                       (fromMaybe (fill 1) (ocpBcScale ocp))
                       (fromMaybe (fill 1) (ocpPathCScale ocp))
        }
  return $ CollProblem { cpNlp = nlp
                       , cpOcp = ocp
                       , cpPlotPoints = getPlotPoints
                       , cpOutputs = getOutputs
                       , cpTaus = taus
                       , cpRoots = roots
                       }


data CollCovProblem x z u p r o c h n deg sx sw sh shr sc =
  CollCovProblem
  { ccpNlp :: Nlp'
              (CollTrajCov sx x z u p n deg)
              JNone
              (CollOcpCovConstraints n deg x r c h sh shr sc) MX
  , ccpPlotPoints :: J (CollTrajCov sx x z u p n deg) (Vector Double) -> IO (DynPlotPoints Double)
  , ccpOutputs ::
       J (CollTrajCov sx x z u p n deg) (Vector Double)
       -> IO ( Vec n (Vec deg (o Double, x Double), x Double)
             , Vec n (J (Cov (JV sx)) (Vector Double))
             , J (Cov (JV sx)) (Vector Double)
             )
  , ccpSensitivities :: MXFun
                        (J (CollTraj x z u p n deg))
                        (CovarianceSensitivities (JV sx) (JV sw) n)
  , ccpCovariances :: MXFun
                      (J (CollTrajCov sx x z u p n deg)) (J (CovTraj sx n))
  , ccpRoots :: QuadratureRoots
  }

makeCollCovProblem ::
  forall x z u p r o c h sx sz sw sr sh shr sc deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize sr, Vectorize sw, Vectorize sz, Vectorize sx,
   Vectorize r, Vectorize o, Vectorize h, Vectorize c,
   View sh, Vectorize shr, View sc)
  => OcpPhase x z u p r o c h
  -> OcpPhaseWithCov (OcpPhase x z u p r o c h) sx sz sw sr sh shr sc
  -> IO (CollCovProblem x z u p r o c h n deg sx sw sh shr sc)
makeCollCovProblem ocp ocpCov = do
  let -- the collocation points
      roots = Legendre

      taus :: Vec deg Double
      taus = mkTaus roots

  computeSensitivities <- mkComputeSensitivities roots (ocpCovDae ocpCov)
  computeCovariances <- mkComputeCovariances continuousToDiscreetNoiseApprox
                        (computeSensitivities) (ocpCovSq ocpCov)

  sbcFun <- toSXFun "sbc" $ \(x0:*:x1) -> ocpCovSbc ocpCov x0 x1
  shFun <- toSXFun "sh" $ \(x0:*:x1) -> ocpCovSh ocpCov (sxSplitJV x0) x1
  mayerFun <- toSXFun "cov mayer" $ \(x0:*:x1:*:x2:*:x3:*:x4) ->
    sxCatJV $ Id $ ocpCovMayer ocpCov (unId (sxSplitJV x0)) (sxSplitJV x1) (sxSplitJV x2) x3 x4
  lagrangeFun <- toSXFun "cov lagrange" $ \(x0:*:x1:*:x2:*:x3) ->
    sxCatJV $ Id $ ocpCovLagrange ocpCov (unId (sxSplitJV x0)) (sxSplitJV x1) x2 (unId (sxSplitJV x3))

  cp0 <- makeCollProblem ocp

  robustify <- mkRobustifyFunction (ocpCovProjection ocpCov) (ocpCovRobustifyPathC ocpCov)

  let nlp0 = cpNlp cp0
      gammas' = ocpCovGammas ocpCov :: shr Double

      gammas :: J (JV shr) MX
      gammas = catJV' (fmap realToFrac gammas')

      rpathCUb :: shr Bounds
      rpathCUb = fill (Nothing, Just 0)

      robustPathCUb :: J (JV shr) (Vector Bounds)
      robustPathCUb = catJV rpathCUb

      -- the NLP
      fg :: J (CollTrajCov sx x z u p n deg) MX
            -> J JNone MX
            -> (J (JV Id) MX, J (CollOcpCovConstraints n deg x r c h sh shr sc) MX)
      fg = getFgCov taus
        computeCovariances
        gammas
        (robustify :: (J (JV shr) MX -> J (JV p) MX -> J (JV x) MX -> J (Cov (JV sx)) MX -> J (JV shr) MX))
        (sbcFun :: SXFun (J (Cov (JV sx)) :*: J (Cov (JV sx))) (J sc))
        (shFun :: SXFun (J (JV x) :*: J (Cov (JV sx))) (J sh))
        (lagrangeFun :: SXFun (J (JV Id) :*: J (JV x) :*: J (Cov (JV sx)) :*: J (JV Id)) (J (JV Id)))
        (mayerFun :: SXFun (J (JV Id) :*: (J (JV x) :*: (J (JV x) :*: (J (Cov (JV sx)) :*: J (Cov (JV sx)))))) (J (JV Id)))
        (nlpFG' nlp0)

  computeCovariancesFun' <- toMXFun "compute covariances" computeCovariances
  -- callbacks
  let getPlotPoints :: J (CollTrajCov sx x z u p n deg) (Vector Double) -> IO (DynPlotPoints Double)
      getPlotPoints collTrajCov = do
        let CollTrajCov _ collTraj = split collTrajCov
        cpPlotPoints cp0 collTraj

      getOutputs :: J (CollTrajCov sx x z u p n deg) (Vector Double)
                    -> IO ( Vec n (Vec deg (o Double, x Double), x Double)
                          , Vec n (J (Cov (JV sx)) (Vector Double))
                          , J (Cov (JV sx)) (Vector Double)
                          )
      getOutputs collTrajCov = do
        let CollTrajCov _ collTraj = split collTrajCov
        outputs <- (cpOutputs cp0) collTraj
        covTraj <- fmap split $ eval computeCovariancesFun' (v2d collTrajCov)
        let covs' = ctAllButLast covTraj
            pF = ctLast covTraj
        let covs = unJVec (split covs') :: Vec n (J (Cov (JV sx)) DMatrix)
        return (outputs, fmap d2v covs, d2v pF)

      nlp =
        Nlp'
        { nlpFG' = fg
        , nlpBX' = cat $ CollTrajCov (ocpCovS0bnd ocpCov) (nlpBX' nlp0)
        , nlpBG' = cat $ CollOcpCovConstraints
                   { cocNormal = nlpBG' nlp0
                   , cocCovPathC = jreplicate (ocpCovShBnds ocpCov)
                   , cocCovRobustPathC = jreplicate robustPathCUb
                   , cocSbc = ocpCovSbcBnds ocpCov
                   }
        , nlpX0' = cat $ CollTrajCov (jfill 0) (nlpX0' nlp0)
        , nlpP' = cat JNone
        , nlpLamX0' = Nothing
        , nlpLamG0' = Nothing
        , nlpScaleF' = ocpObjScale ocp
        , nlpScaleX' = Just $ cat $
                       CollTrajCov (fromMaybe (jfill 1) (ocpCovSScale ocpCov)) $
                       cat $ fillCollTraj
                       (fromMaybe (fill 1) (ocpXScale ocp))
                       (fromMaybe (fill 1) (ocpZScale ocp))
                       (fromMaybe (fill 1) (ocpUScale ocp))
                       (fromMaybe (fill 1) (ocpPScale ocp))
                       (fromMaybe       1  (ocpTScale ocp))

        , nlpScaleG' = Just $ cat $ CollOcpCovConstraints
                       { cocNormal = cat $ fillCollConstraints
                                     (fromMaybe (fill 1) (ocpXScale ocp))
                                     (fromMaybe (fill 1) (ocpResidualScale ocp))
                                     (fromMaybe (fill 1) (ocpBcScale ocp))
                                     (fromMaybe (fill 1) (ocpPathCScale ocp))
                       , cocCovPathC = jreplicate (fromMaybe (jfill 1) (ocpCovPathCScale ocpCov))
                       , cocCovRobustPathC = jreplicate $
                                             fromMaybe (jfill 1) $
                                             fmap catJV (ocpCovRobustPathCScale ocpCov)
                       , cocSbc = fromMaybe (jfill 1) (ocpCovSbcScale ocpCov)
                       }
        }
  computeSensitivitiesFun' <- toMXFun "compute sensitivities" computeSensitivities
  return $ CollCovProblem { ccpNlp = nlp
                          , ccpPlotPoints = getPlotPoints
                          , ccpOutputs = getOutputs
                          , ccpSensitivities = computeSensitivitiesFun'
                          , ccpCovariances = computeCovariancesFun'
                          , ccpRoots = roots
                          }

getFg ::
  forall z x u p r o c h n deg .
  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize r, Vectorize o, Vectorize c, Vectorize h)
  => Vec deg Double
  -> SXFun (J (JV x) :*: J (JV x)) (J (JV c))
  -> SXFun
      (J (JV Id) :*: J (JV x) :*: J (JV x)) (J (JV Id))
  -> ((J (JV p) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (JVec deg (JV o)) :*: J (JV Id) :*: J (JVec deg (JV Id))) MX ->
      (J (JV Id)) MX)
  -> ((J (JV Id) :*: J (JV p) :*: J (JVec deg (JV Id)) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u))) MX -> (J (JVec deg (JV r)) :*: J (JVec deg (JV o)) :*: J (JVec deg (JV h)) :*: J (JV x)) MX)
  -> J (CollTraj x z u p n deg) MX
  -> J JNone MX
  -> (J (JV Id) MX, J (CollOcpConstraints n deg x r c h) MX)
getFg taus bcFun mayerFun quadFun stageFun collTraj _ = (obj, cat g)
  where
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    spstagesPoints :: Vec n (J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX)
    spstagesPoints = fmap (\(CollStage _ cps) -> cps) spstages

    obj = objLagrange + objMayer

    objMayer = call mayerFun (tf :*: x0 :*: xf)

    objLagrange :: J (JV Id) MX
    objLagrange = F.sum $ TV.tvzipWith3 oneStage spstagesPoints outputs times'
    oneStage :: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX -> J (JVec deg (JV o)) MX -> J (JVec deg (JV Id)) MX
                -> J (JV Id) MX
    oneStage stagePoints stageOutputs stageTimes =
      quadFun (parm :*: stagePoints :*: stageOutputs :*: dt :*: stageTimes)

    -- timestep
    dt = tf / fromIntegral n
    n = reflectDim (Proxy :: Proxy n)

    -- times at each collocation point
    times :: Vec n (Vec deg (J (JV Id) MX))
    times = fmap snd $ timesFromTaus 0 (fmap realToFrac taus) dt

    times' :: Vec n (J (JVec deg (JV Id)) MX)
    times' = fmap (cat . JVec) times

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    x0s = fmap (\(CollStage x0' _) -> x0') spstages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (J (JV x) MX)
    xfs = TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead spstages)
    g = CollOcpConstraints
        { coCollPoints = cat $ JVec dcs
        , coContinuity = cat $ JVec integratorMatchingConstraints
        , coPathC = cat $ JVec hs
        , coBc = call bcFun (x0 :*: xf)
        }

    integratorMatchingConstraints :: Vec n (J (JV x) MX) -- THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = vzipWith (-) interpolatedXs xfs

    dcs :: Vec n (J (JVec deg (JV r)) MX)
    outputs :: Vec n (J (JVec deg (JV o)) MX)
    hs :: Vec n (J (JVec deg (JV h)) MX)
    interpolatedXs :: Vec n (J (JV x) MX)
    (dcs, outputs, hs, interpolatedXs) = TV.tvunzip4 $ fmap fff $ TV.tvzip spstages times'
    fff :: (CollStage (JV x) (JV z) (JV u) deg MX, J (JVec deg (JV Id)) MX) ->
           (J (JVec deg (JV r)) MX, J (JVec deg (JV o)) MX, J (JVec deg (JV h)) MX, J (JV x) MX)
    fff (CollStage x0' xzus, stageTimes) = (dc, output, stageHs, interpolatedX')
      where
        dc :*: output :*: stageHs :*: interpolatedX' =
          stageFun (dt :*: parm :*: stageTimes :*: x0' :*: xzs :*: us)

        xzs = cat (JVec xzs') :: J (JVec deg (JTuple (JV x) (JV z))) MX
        us = cat (JVec us') :: J (JVec deg (JV u)) MX
        (xzs', us') = TV.tvunzip $ fmap toTuple $ unJVec (split xzus)
        toTuple xzu = (cat (JTuple x z), u)
          where
            CollPoint x z u = split xzu


getFgCov ::
  forall z x u p r c h sx sh shr sc n deg .
  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize h, Vectorize c, Vectorize r,
   Vectorize sx, View sc, View sh, Vectorize shr)
  -- taus
  => Vec deg Double
  -> (J (CollTrajCov sx x z u p n deg) MX -> J (CovTraj sx n) MX)
  -- gammas
  -> J (JV shr) MX
  -- robustify
  -> (J (JV shr) MX -> J (JV p) MX -> J (JV x) MX -> J (Cov (JV sx)) MX -> J (JV shr) MX)
   -- sbcFun
  -> SXFun (J (Cov (JV sx)) :*: J (Cov (JV sx))) (J sc)
   -- shFun
  -> SXFun (J (JV x) :*: J (Cov (JV sx))) (J sh)
   -- lagrangeFun
  -> SXFun
      (J (JV Id) :*: J (JV x) :*: J (Cov (JV sx)) :*: J (JV Id)) (J (JV Id))
   -- mayerFun
  -> SXFun
      (J (JV Id) :*: J (JV x) :*: J (JV x) :*: J (Cov (JV sx)) :*: J (Cov (JV sx))) (J (JV Id))
  -> (J (CollTraj x z u p n deg) MX -> J JNone MX -> (J (JV Id) MX, J (CollOcpConstraints n deg x r c h) MX)
     )
  -> J (CollTrajCov sx x z u p n deg) MX
  -> J JNone MX
  -> (J (JV Id) MX, J (CollOcpCovConstraints n deg x r c h sh shr sc) MX)
getFgCov
  taus computeCovariances
  gammas robustify sbcFun shFun lagrangeFun mayerFun
  normalFG collTrajCov nlpParams =
  (obj0 + objectiveLagrangeCov + objectiveMayerCov, cat g)
  where
    CollTrajCov p0 collTraj = split collTrajCov
    (obj0, g0) = normalFG collTraj nlpParams

    g = CollOcpCovConstraints
        { cocNormal = g0
        , cocCovPathC = cat (JVec covPathConstraints)
        , cocCovRobustPathC = cat (JVec robustifiedPathC)
        , cocSbc = call sbcFun (p0 :*: pF)
        }
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    objectiveMayerCov = call mayerFun (tf :*: x0 :*: xf :*: p0 :*: pF)

    -- timestep
    dt = tf / fromIntegral n
    n = reflectDim (Proxy :: Proxy n)

    -- times at each collocation point
    t0s :: Vec n (J (JV Id) MX)
    (t0s, _) = TV.tvunzip $ timesFromTaus 0 (fmap realToFrac taus) dt

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    x0s = fmap (\(CollStage x0' _) -> x0') spstages

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead spstages)

--    sensitivities = call computeSensitivities collTraj

    covs :: Vec n (J (Cov (JV sx)) MX)
    covs = unJVec (split covs')

    covs' :: J (JVec n (Cov (JV sx))) MX -- all but last covariance
    pF :: J (Cov (JV sx)) MX -- last covariances
    CovTraj covs' pF = split (computeCovariances collTrajCov)

    -- lagrange term
    objectiveLagrangeCov = (lagrangeF + lagrange0s) / fromIntegral n
      where
      lagrangeF = call lagrangeFun (tf :*: xf :*: pF :*: tf)
      lagrange0s =
        sum $ F.toList $
        TV.tvzipWith3 (\tk xk pk -> call lagrangeFun (tk :*: xk :*: pk :*: tf)) t0s x0s covs

    covPathConstraints :: Vec n (J sh MX)
    covPathConstraints = TV.tvzipWith (\xk pk -> call shFun (xk:*:pk)) x0s covs

    robustifiedPathC :: Vec n (J (JV shr) MX)
    robustifiedPathC = TV.tvzipWith (robustify gammas parm) x0s covs





getBg :: forall x z u p r o c h deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize r, Vectorize c, Vectorize h)
  => OcpPhase x z u p r o c h
  -> CollOcpConstraints n deg x r c h (Vector Bounds)
getBg ocp =
  CollOcpConstraints
  { coCollPoints = jreplicate (jfill (Just 0, Just 0)) -- dae residual constraint
  , coContinuity = jreplicate (jfill (Just 0, Just 0)) -- continuity constraint
  , coPathC = jreplicate (jreplicate hbnds)
  , coBc = catJV (ocpBcBnds ocp)
  }
  where
    hbnds = catJV (ocpPathCBnds ocp)

evaluateQuadraturesFunction ::
  forall x z u p o deg .
  (Dim deg, View x, View z, View u, View o, View p)
  => SXFun (J x :*: J z :*: J u :*: J p :*: J o :*: J (JV Id) :*: J (JV Id)) (J (JV Id))
  -> (J (JV Id) MX -> Vec deg (J (JV Id) MX) -> J (JV Id) MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> Int
  -> (J p :*: J (JVec deg (CollPoint x z u)) :*: J (JVec deg o) :*: J (JV Id) :*: J (JVec deg (JV Id))) MX
  -> J (JV Id) MX
evaluateQuadraturesFunction f interpolate' cijs' n (p :*: stage' :*: outputs' :*: dt :*: stageTimes') =
  dt * qnext
  where
    tf = dt * fromIntegral n

    stage :: Vec deg (CollPoint x z u MX)
    stage = fmap split $ unJVec $ split stage'

    outputs :: Vec deg (J o MX)
    outputs = unJVec (split outputs')

    stageTimes :: Vec deg (J (JV Id) MX)
    stageTimes = unJVec (split stageTimes')

    qnext :: J (JV Id) MX
    qnext = interpolate' 0 qs

    qdots :: Vec deg (J (JV Id) MX)
    qdots = TV.tvzipWith3 (\(CollPoint x z u) o t -> call f (x:*:z:*:u:*:p:*:o:*:t:*:tf)) stage outputs stageTimes

    qs = cijInvFr !* qdots

    cijs :: Vec deg (Vec deg Double)
    cijs = TV.tvtail $ fmap TV.tvtail cijs'

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs

    cijInv' :: Mat.Matrix Double
    cijInv' = LA.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (J (JV Id) MX))
    cijInvFr = fmap (fmap realToFrac) cijInv

-- todo: code duplication
dot :: forall x deg a b. (Fractional (J x a), Real b, Dim deg) => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unVec elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y


-- todo: code duplication
interpolateXDots' :: (Real b, Fractional (J x a), Dim deg) => Vec deg (Vec deg b) -> Vec deg (J x a) -> Vec deg (J x a)
interpolateXDots' cjks xs = fmap (`dot` xs) cjks

interpolateXDots ::
  (Real b, Dim deg, Fractional (J x a)) =>
  Vec (TV.Succ deg) (Vec (TV.Succ deg) b)
  -> Vec (TV.Succ deg) (J x a)
  -> Vec deg (J x a)
interpolateXDots cjks xs = TV.tvtail $ interpolateXDots' cjks xs


-- dynamics residual and outputs
dynamicsFunction ::
  forall x z u p r o a . (View x, View z, View u, View r, View o, Viewable a)
  => (J x a -> J x a -> J z a -> J u a -> J p a -> J (JV Id) a -> (J r a, J o a))
  -> (J (JV Id) :*: J p :*: J x :*: J (CollPoint x z u)) a
  -> (J r :*: J o) a
dynamicsFunction dae (t :*: parm :*: x' :*: collPoint) =
  r :*: o
  where
    CollPoint x z u = split collPoint
    (r,o) = dae x' x z u parm t

-- path constraints
pathConFunction ::
  forall x z u p o h a . (View x, View z, View u, View o, View h, Viewable a)
  => (J x a -> J z a -> J u a -> J p a -> J o a -> J (JV Id) a -> J h a)
  -> (J (JV Id) :*: J p :*: J o :*: J (CollPoint x z u)) a
  -> J h a
pathConFunction pathC (t :*: parm :*: o :*: collPoint) =
  pathC x z u parm o t
  where
    CollPoint x z u = split collPoint

-- return dynamics constraints, outputs, and interpolated state
dynStageConstraints ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> SXFun (J (JV Id) :*: J p :*: J x :*: J (CollPoint x z u))
           (J r :*: J o)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J (JV Id) :*: J p :*: J (JVec deg (JV Id))) MX
  -> (J (JVec deg r) :*: J x :*: J (JVec deg o)) MX
dynStageConstraints interpolate' cijs dynFun (x0 :*: xzs' :*: us' :*: h :*: p :*: stageTimes') =
  cat (JVec dynConstrs) :*: xnext :*: cat (JVec outputs)
  where
    xzs = fmap split (unJVec (split xzs')) :: Vec deg (JTuple x z MX)
    us = unJVec (split us') :: Vec deg (J u MX)

    -- interpolated final state
    xnext :: J x MX
    xnext = interpolate' x0 xs

    stageTimes = unJVec $ split stageTimes'

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    outputs :: Vec deg (J o MX)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith4 applyDae xdots xzs us stageTimes

    applyDae :: J x MX -> JTuple x z MX -> J u MX -> J (JV Id) MX -> (J r MX, J o MX)
    applyDae x' (JTuple x z) u t = (r, o)
      where
        r :*: o = call dynFun (t :*: p :*: x' :*: collPoint)
        collPoint = cat (CollPoint x z u)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap (\(JTuple x _) -> x) xzs


data ErrorIn0 x z u p deg a =
  ErrorIn0 (J x a) (J (JVec deg (CollPoint x z u)) a) (J (JV Id) a) (J p a) (J (JVec deg (JV Id)) a)
  deriving Generic
data ErrorInD sx sw sz deg a =
  ErrorInD (J sx a) (J sw a) (J (JVec deg (JTuple sx sz)) a)
  deriving Generic
data ErrorOut sr sx deg a =
  ErrorOut (J (JVec deg sr) a) (J sx a)
  deriving Generic

instance (View x, View z, View u, View p, Dim deg) => Scheme (ErrorIn0 x z u p deg)
instance (View sx, View sw, View sz, Dim deg) => View (ErrorInD sx sw sz deg)
instance (View sr, View sx, Dim deg) => View (ErrorOut sr sx deg)



-- outputs
outputFunction ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (J (JV Id) :*: J p :*: J x :*: J (CollPoint x z u))
           (J r :*: J o)
  -> (J (CollStage x z u deg) :*: J p :*: J (JV Id) :*: J (JV Id)) MX
  -> (J (JVec deg r) :*: J (JVec deg x) :*: J (JVec deg o) :*: J x) MX
outputFunction callInterpolate cijs taus dynFun (collStage :*: p :*: h :*: k) =
  cat (JVec dynConstrs) :*: cat (JVec xdots) :*: cat (JVec outputs) :*: xnext
  where
    xzus = unJVec (split xzus') :: Vec deg (J (CollPoint x z u) MX)
    CollStage x0 xzus' = split collStage
    -- times at each collocation point
    stageTimes :: Vec deg (J (JV Id) MX)
    stageTimes = fmap (\tau -> t0 + realToFrac tau * h) taus
    t0 = k*h

    xnext = callInterpolate x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    outputs :: Vec deg (J o MX)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots xzus stageTimes

    applyDae :: J x MX -> J (CollPoint x z u) MX -> J (JV Id) MX -> (J r MX, J o MX)
    applyDae x' xzu t = (r, o)
      where
        r :*: o = call dynFun (t :*: p :*: x' :*: xzu)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap ((\(CollPoint x _ _) -> x) . split) xzus



-- return dynamics constraints, outputs, and interpolated state
pathStageConstraints ::
  forall x z u p o h deg . (Dim deg, View x, View z, View u, View p, View o, View h)
  => SXFun (J (JV Id) :*: J p :*: J o :*: J (CollPoint x z u))
           (J h)
  -> (J p :*: J (JVec deg (JV Id)) :*: J (JVec deg o) :*: J (JVec deg (CollPoint x z u))) MX
  -> J (JVec deg h) MX
pathStageConstraints pathCFun
  (p :*: stageTimes' :*: outputs :*: collPoints) =
  cat (JVec hs)
  where
    stageTimes = unJVec $ split stageTimes'
    cps = fmap split (unJVec (split collPoints)) :: Vec deg (CollPoint x z u MX)

    -- dae constraints (dynamics)
    hs :: Vec deg (J h MX)
    hs = TV.tvzipWith3 applyH cps stageTimes (unJVec (split outputs))

    applyH :: CollPoint x z u MX -> J (JV Id) MX -> J o MX -> J h MX
    applyH (CollPoint x z u) t o = pathc'
      where
        pathc' = call pathCFun (t :*: p :*: o :*: collPoint)
        collPoint = cat (CollPoint x z u)


stageFunction ::
  forall x z u p o r h deg . (Dim deg, View x, View z, View u, View p, View r, View o, View h)
  => MXFun (J p :*: J (JVec deg (JV Id)) :*: J (JVec deg o) :*: J (JVec deg (CollPoint x z u)))
           (J (JVec deg h))
  -> ((J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J (JV Id) :*: J p :*: J (JVec deg (JV Id))) MX
      -> (J (JVec deg r) :*: J x :*: J (JVec deg o)) MX)
  -> (J (JV Id) :*: J p :*: J (JVec deg (JV Id)) :*: J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u)) MX
  -> (J (JVec deg r) :*: J (JVec deg o) :*: J (JVec deg h) :*: J x) MX
stageFunction pathConStageFun dynStageCon
  (dt :*: parm :*: stageTimes :*: x0' :*: xzs' :*: us) =
    dynConstrs :*: outputs :*: hs :*: interpolatedX
  where
    collPoints = cat $ JVec $ TV.tvzipWith catXzu (unJVec (split xzs')) (unJVec (split us))

    catXzu :: J (JTuple x z) MX -> J u MX -> J (CollPoint x z u) MX
    catXzu xz u = cat $ CollPoint x z u
      where
        JTuple x z = split xz

    dynConstrs :: J (JVec deg r) MX
    outputs :: J (JVec deg o) MX
    interpolatedX :: J x MX
    (dynConstrs :*: interpolatedX :*: outputs) =
      dynStageCon (x0' :*: xzs' :*: us :*: dt :*: parm :*: stageTimes)

    hs :: J (JVec deg h) MX
    hs = call pathConStageFun (parm :*: stageTimes :*: outputs :*: collPoints)


-- | make an initial guess
makeGuess ::
  forall x z u p deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize p)
  => QuadratureRoots
  -> Double -> (Double -> x Double) -> (Double -> z Double) -> (Double -> u Double)
  -> p Double
  -> CollTraj x z u p n deg (Vector Double)
makeGuess quadratureRoots tf guessX guessZ guessU parm =
  CollTraj (jfill tf) (catJV parm) guesses (catJV (guessX tf))
  where
    -- timestep
    dt = tf / fromIntegral n
    n = vlength (Proxy :: Proxy (Vec n))

    -- initial time at each collocation stage
    t0s :: Vec n Double
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Double, Vec deg Double)
    times = fmap (\t0 -> (t0, fmap (\tau -> t0 + tau*dt) taus)) t0s

    mkGuess' :: (Double, Vec deg Double) -> CollStage (JV x) (JV z) (JV u) deg (Vector Double)
    mkGuess' (t,ts) =
      CollStage (catJV (guessX t)) $
      cat $ JVec $ fmap (\t' -> cat (CollPoint (catJV (guessX t')) (catJV (guessZ t')) (catJV (guessU t')))) ts

    guesses :: J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) (Vector Double)
    guesses = cat $ JVec $ fmap (cat . mkGuess') times

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus quadratureRoots


-- | make an initial guess
makeGuessSim ::
  forall x z u p deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize p)
  => QuadratureRoots
  -> Double
  -> x Double
  -> (x Double -> u Double -> x Double)
  -> (x Double -> Double -> u Double)
  -> p Double
  -> CollTraj x z u p n deg (Vector Double)
makeGuessSim quadratureRoots tf x00 ode guessU p =
  CollTraj (jfill tf) (catJV p) (cat (JVec stages)) (catJV xf)
  where
    -- timestep
    dt = tf / fromIntegral n
    n = vlength (Proxy :: Proxy (Vec n))

    -- initial time at each collocation stage
    t0s :: Vec n Double
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    xf :: x Double
    stages :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
    (xf, stages) = T.mapAccumL stageGuess x00 t0s

    stageGuess :: x Double -> Double
                  -> (x Double, J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
    stageGuess x0 t0 = (integrate 1, cat (CollStage (catJV x0) points))
      where
        points = cat $ JVec $ fmap (toCollPoint . integrate) taus
        u = guessU x0 t0
        f x = ode x u
        toCollPoint x = cat $ CollPoint (catJV x) (catJV (fill 0 :: z Double)) (catJV u)
        integrate localTau = rk4 f (localTau * dt) x0

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus quadratureRoots

    rk4 :: (x Double -> x Double) -> Double -> x Double -> x Double
    rk4 f h x0 = x0 ^+^ ((k1 ^+^ (2 *^ k2) ^+^ (2 *^ k3) ^+^ k4) ^/ 6)
      where
        k1 = (f  x0)            ^* h
        k2 = (f (x0 ^+^ (k1^/2))) ^* h
        k3 = (f (x0 ^+^ (k2^/2))) ^* h
        k4 = (f (x0 ^+^ k3))    ^* h

        (^+^) :: x Double -> x Double -> x Double
        y0 ^+^ y1 = devectorize $ V.zipWith (+) (vectorize y0) (vectorize y1)

        (*^) :: Double -> x Double -> x Double
        y0 *^ y1 = devectorize $ V.map (y0 *) (vectorize y1)

        (^*) :: x Double -> Double -> x Double
        y0 ^* y1 = devectorize $ V.map (* y1) (vectorize y0)

        (^/) :: x Double -> Double -> x Double
        y0 ^/ y1 = devectorize $ V.map (/ y1) (vectorize y0)
