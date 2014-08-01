{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}

module Dyno.DirectCollocation.Formulate
       ( makeCollNlp
       , makeCollCovNlp
       , mkTaus
       , interpolate
       , makeGuess
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Matrix hiding ( trace )
import Linear.V

import Dyno.Cov
import Dyno.View
import Dyno.Vectorize ( Vectorize(..), fill, vlength, vzipWith )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )
import Dyno.Nlp ( Nlp'(..), Bounds )
import Dyno.Ocp ( OcpPhase(..), OcpPhaseWithCov(..) )
import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( DynCollTraj, ctToDynamic )
import Dyno.DirectCollocation.Quadratures ( mkTaus, interpolate )
import Dyno.Casadi.MX ( solve, mm, trans, d2m, zeros )
import Dyno.Casadi.SXElement ( SXElement )
import Dyno.Casadi.SX ( sdata, sdense, svector )
import Dyno.Casadi.DMatrix ( dvector, ddata, ddense )

--data RorL = Radau | Legendre deriving (Eq, Show)

de :: Vectorize v => J (JV v) SX -> v SXElement
de = devectorize . sdata . sdense . unJ

de' :: J S SX -> SXElement
de' = V.head . sdata . sdense . unJ

re :: Vectorize v => v SXElement -> J (JV v) SX
re = mkJ . svector . vectorize

re' :: SXElement -> J S SX
re' = mkJ . svector . V.singleton


makeCollNlp ::
  forall x z u p r o c h deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize r, Vectorize o, Vectorize h, Vectorize c)
  => OcpPhase x z u p r o c h
  -> IO ( Nlp' (CollTraj x z u p n deg) JNone (CollOcpConstraints n deg x r c h) MX
        , J (CollTraj x z u p n deg) (Vector Double)
          -> IO (DynCollTraj (Vector Double), Vec n (Vec deg (o Double)))
        )
makeCollNlp ocp = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus deg

      deg = reflectDim (Proxy :: Proxy deg)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
      cijs = lagrangeDerivCoeffs (0 TV.<| taus)

  bcFun <- toSXFun "bc" $ \(x0:*:x1) -> re $ ocpBc ocp (de x0) (de x1)
  mayerFun <- toSXFun "mayer" $ \(x0:*:x1:*:x2) ->
    re' $ ocpMayer ocp (de' x0) (de x1) (de x2)
  lagrangeFun <- toSXFun "lagrange" $ \(x0:*:x1:*:x2:*:x3:*:x4:*:x5) ->
    re' $ ocpLagrange ocp (de x0) (de x1) (de x2) (de x3) (de x4) (de' x5)
  quadFun <- toMXFun "quadratures" $ evaluateQuadraturesFunction lagrangeFun cijs taus
--  let callQuadFun = callMXFun quadFun
  callQuadFun <- fmap callSXFun (expandMXFun quadFun)

  dynFun <- toSXFun "dynamics" $ dynamicsFunction $
            \x0 x1 x2 x3 x4 x5 ->
            let (r,o) = ocpDae ocp (de x0) (de x1) (de x2) (de x3) (de x4) (de' x5)
            in (re r, re o)

  pathConFun <- toSXFun "pathConstraints" $ pathConFunction $
                \x0 x1 x2 x3 x4 x5 -> re $ ocpPathC ocp (de x0) (de x1) (de x2) (de x3) (de x4) (de' x5)
  pathStageConFun <- toMXFun "pathStageCon" (pathStageConstraints pathConFun)

  dynStageConFun <- toMXFun "dynamicsStageCon" (dynStageConstraints cijs taus dynFun)

  stageFun <- toMXFun "stageFunction" $ stageFunction pathStageConFun (callMXFun dynStageConFun)
--  let callStageFun = callMXFun stageFun
  callStageFun <- fmap callSXFun (expandMXFun stageFun)

  outputFun <- toMXFun "stageOutputs" $ outputFunction cijs taus dynFun

  -- prepare callbacks
  let nlpX0 = jfill 0 :: J (CollTraj x z u p n deg) (Vector Double)

      f :: J (JV o) DMatrix -> J (JV o) (Vector Double)
      f o' = mkJ (ddata (ddense (unJ o')))

      n = reflectDim (Proxy :: Proxy n)

      dmToDv :: J a (Vector Double) -> J a DMatrix
      dmToDv (UnsafeJ v) = UnsafeJ (dvector v)

      callOutputFun :: J (JV p) (Vector Double)
                       -> J S (Vector Double)
                       -> J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double)
                       -> J S (Vector Double)
                       -> IO (Vec deg (J (JV o) (Vector Double)))
      callOutputFun p tf stage k = do
        (_ :*: out) <- evalMXFun outputFun $
                       (dmToDv stage) :*: (dmToDv p) :*: (dmToDv tf) :*: (dmToDv k)
        let outs0 = unJVec (split out) :: Vec deg (J (JV o) DMatrix)
        return (fmap f outs0)

      mapOutputFun :: J (CollTraj x z u p n deg) (Vector Double) -> IO (Vec n (Vec deg (J (JV o) (Vector Double))))
      mapOutputFun ct = do
        let CollTraj tf p stages _ = split ct
            vstages = unJVec (split stages)
                :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
            ks :: Vec n (J S (Vector Double))
            ks = TV.mkVec' $ map (mkJ . V.singleton . realToFrac) (take n [(0::Int)..])

        T.sequence $ TV.tvzipWith (callOutputFun p tf) vstages ks

      callback :: J (CollTraj x z u p n deg) (Vector Double)
                  -> IO (DynCollTraj (Vector Double), Vec n (Vec deg (o Double)))
      callback traj = do
        outputs <- mapOutputFun traj
        let -- devectorize outputs
            devec :: J (JV o) (Vector Double) -> o Double
            devec (UnsafeJ os) = devectorize os
        return (ctToDynamic traj outputs, fmap (fmap devec) outputs)

  return (Nlp' {
    nlpFG' =
       getFg taus
       (bcFun :: SXFun (J (JV x) :*: J (JV x)) (J (JV c)))
       (mayerFun :: SXFun (J S :*: (J (JV x) :*: (J (JV x)))) (J S))
       (callQuadFun :: (J (JV p) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (JVec deg (JV o)) :*: J S :*: J (JVec deg S)) MX
                    -> J S MX)
       (callStageFun :: (J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u))) MX
                  -> (J (JVec deg (JV r)) :*: J (JVec deg (JV o)) :*: J (JVec deg (JV h)) :*: J (JV x)) MX)
    , nlpBX' = cat (getBx ocp)
    , nlpBG' = cat (getBg ocp)
    , nlpX0' = nlpX0
    , nlpP' = cat JNone
    }, callback)


makeCollCovNlp ::
  forall x z u p r o c h sx sz sw sr sh sc deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize sr, Vectorize sw, Vectorize sz, Vectorize sx,
   Vectorize r, Vectorize o, Vectorize h, Vectorize c,
   View sh, View sc)
  => OcpPhase x z u p r o c h
  -> OcpPhaseWithCov (OcpPhase x z u p r o c h) sx sz sw sr sh sc
  -> IO ( Nlp' (CollTrajCov sx x z u p n deg) JNone (CollOcpCovConstraints n deg x r c h sh sc) MX
        , J (CollTrajCov sx x z u p n deg) (Vector Double)
          -> IO (DynCollTraj (Vector Double), Vec n (Vec deg (o Double)))
        )
makeCollCovNlp ocp ocpCov = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus deg

      deg = reflectDim (Proxy :: Proxy deg)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
      cijs = lagrangeDerivCoeffs (0 TV.<| taus)

  sbcFun <- toSXFun "sbc" $ \(x0:*:x1) -> ocpCovSbc ocpCov x0 x1
  shFun <- toSXFun "sh" $ \(x0:*:x1) -> ocpCovSh ocpCov (de x0) x1
  mayerFun <- toSXFun "cov mayer" $ \(x0:*:x1:*:x2:*:x3:*:x4) ->
    re' $ ocpCovMayer ocpCov (de' x0) (de x1) (de x2) x3 x4
  lagrangeFun <- toSXFun "cov lagrange" $ \(x0:*:x1:*:x2:*:x3) ->
    re' $ ocpCovLagrange ocpCov (de' x0) (de x1) x2 (de' x3)

  errorDynFun <- toSXFun "error dynamics" $ errorDynamicsFunction $
            \x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 ->
            let r = ocpCovDae ocpCov
                    (de x0) (de x1) (de x2) (de x3) (de x4)
                    (de' x5) (de x6) (de x7) (de x8) (de x9)
            in re r

  errorDynStageConFunJac <- toFunJac' "errorDynamicsStageConJac"
                            (errorDynStageConstraints cijs taus errorDynFun)

  covStageFun <- toMXFun "covStageFunction" $ covStageFunction errorDynStageConFunJac
--  let callCovStageFun = callMXFun covStageFun
  callCovStageFun <- fmap callSXFun (expandMXFun covStageFun)

  (nlp0, callback0) <- makeCollNlp ocp

  let fg :: J (CollTrajCov sx x z u p n deg) MX
            -> J JNone MX
            -> (J S MX, J (CollOcpCovConstraints n deg x r c h sh sc) MX)
      fg = getFgCov taus
        (ocpCovSq ocpCov :: J (Cov (JV sw)) DMatrix)
        (sbcFun :: SXFun (J (Cov (JV sx)) :*: J (Cov (JV sx))) (J sc))
        (shFun :: SXFun (J (JV x) :*: J (Cov (JV sx))) (J sh))
        (lagrangeFun :: SXFun (J S :*: J (JV x) :*: J (Cov (JV sx)) :*: J S) (J S))
        (mayerFun :: SXFun (J S :*: (J (JV x) :*: (J (JV x) :*: (J (Cov (JV sx)) :*: J (Cov (JV sx)))))) (J S))
        (callCovStageFun :: (J (Cov (JV sx)) :*: J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (Cov (JV sw))) MX
                 -> J (Cov (JV sx)) MX)
        (nlpFG' nlp0)

  let callback collTrajCov = do
        let CollTrajCov _ collTraj = split collTrajCov
        (dynCollTraj, outputs) <- callback0 collTraj
        return (dynCollTraj, outputs)

  return (Nlp' {
    nlpFG' = fg
    , nlpBX' = cat $ CollTrajCov (ocpCovS0bnd ocpCov) (nlpBX' nlp0)
    , nlpBG' = cat $ CollOcpCovConstraints
               { cocNormal = nlpBG' nlp0
               , cocCovPathC = jreplicate (ocpCovShBnds ocpCov)
               , cocSbc = ocpCovSbcBnds ocpCov
               }
    , nlpX0' = cat $ CollTrajCov (jfill 0) (nlpX0' nlp0)
    , nlpP' = cat JNone
    }
    , callback
    )

getFg ::
  forall z x u p r o c h n deg .
  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize r, Vectorize o, Vectorize c, Vectorize h)
  => Vec deg Double
  -> SXFun (J (JV x) :*: J (JV x)) (J (JV c))
  -> SXFun
      (J S :*: J (JV x) :*: J (JV x)) (J S)
  -> ((J (JV p) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (JVec deg (JV o)) :*: J S :*: J (JVec deg S)) MX ->
      (J S) MX)
  -> ((J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u))) MX -> (J (JVec deg (JV r)) :*: J (JVec deg (JV o)) :*: J (JVec deg (JV h)) :*: J (JV x)) MX)
  -> J (CollTraj x z u p n deg) MX
  -> J JNone MX
  -> (J S MX, J (CollOcpConstraints n deg x r c h) MX)
getFg taus bcFun mayerFun quadFun stageFun collTraj _ = (obj, cat g)
  where
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    spstagesPoints :: Vec n (J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX)
    spstagesPoints = fmap (\(CollStage _ cps) -> cps) spstages

    obj = objLagrange + objMayer

    objMayer = callSXFun mayerFun (tf :*: x0 :*: xf)

    objLagrange :: J S MX
    objLagrange = F.sum $ TV.tvzipWith3 oneStage spstagesPoints outputs times'
    oneStage :: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX -> J (JVec deg (JV o)) MX -> J (JVec deg S) MX
                -> J S MX
    oneStage stagePoints stageOutputs stageTimes =
      quadFun (parm :*: stagePoints :*: stageOutputs :*: dt :*: stageTimes)

    -- timestep
    dt = tf / fromIntegral n
    n = reflectDim (Proxy :: Proxy n)

    -- initial time at each collocation stage
    t0s :: Vec n (J S MX)
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Vec deg (J S MX))
    times = fmap (\t0 -> fmap (\tau -> t0 + realToFrac tau * dt) taus) t0s

    times' :: Vec n (J (JVec deg S) MX)
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
        , coBc = callSXFun bcFun (x0 :*: xf)
        }

    integratorMatchingConstraints :: Vec n (J (JV x) MX) -- THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = vzipWith (-) interpolatedXs xfs

    dcs :: Vec n (J (JVec deg (JV r)) MX)
    outputs :: Vec n (J (JVec deg (JV o)) MX)
    hs :: Vec n (J (JVec deg (JV h)) MX)
    interpolatedXs :: Vec n (J (JV x) MX)
    (dcs, outputs, hs, interpolatedXs) = TV.tvunzip4 $ fmap fff $ TV.tvzip spstages times'
    fff :: (CollStage (JV x) (JV z) (JV u) deg MX, J (JVec deg S) MX) ->
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
  forall z x u p r c h sx sw sh sc n deg .
  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize h, Vectorize c, Vectorize r,
   Vectorize sx, Vectorize sw, View sc, View sh)
   -- taus
  => Vec deg Double
   -- sq
  -> J (Cov (JV sw)) DMatrix
   -- sbcFun
  -> SXFun (J (Cov (JV sx)) :*: J (Cov (JV sx))) (J sc)
   -- shFun
  -> SXFun (J (JV x) :*: J (Cov (JV sx))) (J sh)
   -- lagrangeFun
  -> SXFun
      (J S :*: J (JV x) :*: J (Cov (JV sx)) :*: J S) (J S)
   -- mayerFun
  -> SXFun
      (J S :*: J (JV x) :*: J (JV x) :*: J (Cov (JV sx)) :*: J (Cov (JV sx))) (J S)
   -- covStageFun
  -> ((J (Cov (JV sx)) :*: J S :*: J (JV p) :*: J (JVec deg S)
      :*: J (JV x) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (Cov (JV sw))) MX
      -> J (Cov (JV sx)) MX)
  -> (J (CollTraj x z u p n deg) MX -> J JNone MX -> (J S MX, J (CollOcpConstraints n deg x r c h) MX)
     )
  -> J (CollTrajCov sx x z u p n deg) MX
  -> J JNone MX
  -> (J S MX, J (CollOcpCovConstraints n deg x r c h sh sc) MX)
getFgCov taus sq sbcFun shFun lagrangeFun mayerFun covStageFun
  normalFG collTrajCov nlpParams =
  (obj0 + objectiveLagrangeCov + objectiveMayerCov, cat g)
  where
    CollTrajCov p0 collTraj = split collTrajCov
    (obj0, g0) = normalFG collTraj nlpParams

    g = CollOcpCovConstraints
        { cocNormal = g0
        , cocCovPathC = cat (JVec covPathConstraints)
        , cocSbc = callSXFun sbcFun (p0 :*: pF)
        }
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    objectiveMayerCov = callSXFun mayerFun (tf :*: x0 :*: xf :*: p0 :*: pF)

    -- timestep
    dt = tf / fromIntegral n
    n = reflectDim (Proxy :: Proxy n)

    -- initial time at each collocation stage
    t0s :: Vec n (J S MX)
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Vec deg (J S MX))
    times = fmap (\t0 -> fmap (\tau -> t0 + realToFrac tau * dt) taus) t0s

    times' :: Vec n (J (JVec deg S) MX)
    times' = fmap (cat . JVec) times

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    x0s = fmap (\(CollStage x0' _) -> x0') spstages

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead spstages)

    -- lagrange term
    objectiveLagrangeCov = (lagrangeF + lagrange0s) / fromIntegral n
      where
      lagrangeF = callSXFun lagrangeFun (tf :*: xf :*: pF :*: tf)
      lagrange0s =
        sum $ F.toList $
        TV.tvzipWith3 (\tk xk pk -> callSXFun lagrangeFun (tk :*: xk :*: pk :*: tf)) t0s x0s covs

    -- Q
    covInjections :: Vec n (J (Cov (JV sw)) MX)
    covInjections = fill (mkJ (d2m (unJ sq)))

    covPathConstraints :: Vec n (J sh MX)
    covPathConstraints = TV.tvzipWith (\xk pk -> callSXFun shFun (xk:*:pk)) x0s covs

    covs :: Vec n (J (Cov (JV sx)) MX) -- all but last covariances
    pF :: J (Cov (JV sx)) MX -- last covariances
    (pF, covs) = T.mapAccumL ff p0 $ TV.tvzip3 spstages times' covInjections

    ff :: J (Cov (JV sx)) MX
          -> (CollStage (JV x) (JV z) (JV u) deg MX, J (JVec deg S) MX, J (Cov (JV sw)) MX)
          -> (J (Cov (JV sx)) MX, J (Cov (JV sx)) MX)
    ff cov0 (CollStage x0' xzus, stageTimes, covInj) = (cov1, cov0)
      where
        cov1 = covStageFun (cov0 :*: dt :*: parm :*: stageTimes :*: x0' :*: xzus :*: covInj)


getBx :: forall x z u p r o c h n deg .
         (Dim n, Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize p)
         => OcpPhase x z u p r o c h
         -> CollTraj x z u p n deg (Vector Bounds)
getBx ocp = ct
  where
    ct :: CollTraj x z u p n deg (Vector Bounds)
    ct = CollTraj (mkJ (V.singleton tb)) pb (jreplicate (cat cs)) xb

    cs :: CollStage (JV x) (JV z) (JV u) deg (Vector Bounds)
    cs = CollStage xb (jreplicate (cat cp))

    cp :: CollPoint (JV x) (JV z) (JV u) (Vector Bounds)
    cp = CollPoint xb zb ub
    xb = mkJ $ vectorize $ ocpXbnd ocp
    ub = mkJ $ vectorize $ ocpUbnd ocp
    zb = mkJ $ vectorize $ ocpZbnd ocp
    pb = mkJ $ vectorize $ ocpPbnd ocp
    tb = ocpTbnd ocp

getBg :: forall x z u p r o c h deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize r, Vectorize c, Vectorize h)
  => OcpPhase x z u p r o c h
  -> CollOcpConstraints n deg x r c h (Vector Bounds)
getBg ocp =
  CollOcpConstraints
  { coCollPoints = jreplicate (jfill (Just 0, Just 0)) -- dae residual constraint
  , coContinuity = jreplicate (jfill (Just 0, Just 0)) -- continuity constraint
  , coPathC = jreplicate (jreplicate hbnds)
  , coBc = mkJ $ vectorize $ ocpBcBnds ocp
  }
  where
    hbnds = mkJ $ vectorize $ ocpPathCBnds ocp

evaluateQuadraturesFunction ::
  forall x z u p o deg .
  (Dim deg, View x, View z, View u, View o, View p)
  => SXFun (J x :*: J z :*: J u :*: J p :*: J o :*: J S) (J S)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> Vec deg Double
  -> (J p :*: J (JVec deg (CollPoint x z u)) :*: J (JVec deg o) :*: J S :*: J (JVec deg S)) MX
  -> J S MX
evaluateQuadraturesFunction f cijs' taus (p :*: stage' :*: outputs' :*: dt :*: stageTimes') =
  dt * qnext
  where
    stage :: Vec deg (CollPoint x z u MX)
    stage = fmap split $ unJVec $ split stage'

    outputs :: Vec deg (J o MX)
    outputs = unJVec (split outputs')

    stageTimes :: Vec deg (J S MX)
    stageTimes = unJVec (split stageTimes')

    qnext :: J S MX
    qnext = interpolate taus 0 qs

    qdots :: Vec deg (J S MX)
    qdots = TV.tvzipWith3 (\(CollPoint x z u) o t -> callSXFun f (x:*:z:*:u:*:p:*:o:*:t)) stage outputs stageTimes

    qs = cijInvFr !* qdots

    cijs :: Vec deg (Vec deg Double)
    cijs = TV.tvtail $ fmap TV.tvtail cijs'

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs

    cijInv' :: Mat.Matrix Double
    cijInv' = LA.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (J S MX))
    cijInvFr = fmap (fmap realToFrac) cijInv

dot :: forall x deg a b. (Fractional (J x a), Real b) => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unSeq elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y


interpolateXDots' :: (Real b, Fractional (J x a)) => Vec deg (Vec deg b) -> Vec deg (J x a) -> Vec deg (J x a)
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
  => (J x a -> J x a -> J z a -> J u a -> J p a -> J S a -> (J r a, J o a))
  -> (J S :*: J p :*: J x :*: J (CollPoint x z u)) a
  -> (J r :*: J o) a
dynamicsFunction dae (t :*: parm :*: x' :*: collPoint) =
  r :*: o
  where
    CollPoint x z u = split collPoint
    (r,o) = dae x' x z u parm t

-- dynamics residual and outputs
errorDynamicsFunction ::
  forall x z u p r sx sz sw a .
  (View x, View z, View u, View r, View sx, View sz, View sw, Viewable a)
  => (J x a -> J x a -> J z a -> J u a -> J p a -> J S a
      -> J sx a -> J sx a -> J sz a -> J sw a -> J r a)
  -> (J S :*: J p :*: J x :*: J (CollPoint x z u) :*: J sx :*: J sx :*: J sz :*: J sw) a
  -> J r a
errorDynamicsFunction dae (t :*: parm :*: x' :*: collPoint :*: sx' :*: sx :*: sz :*: sw) =
  r
  where
    CollPoint x z u = split collPoint
    r = dae x' x z u parm t sx' sx sz sw

-- path constraints
pathConFunction ::
  forall x z u p o h a . (View x, View z, View u, View o, View h, Viewable a)
  => (J x a -> J z a -> J u a -> J p a -> J o a -> J S a -> J h a)
  -> (J S :*: J p :*: J o :*: J (CollPoint x z u)) a
  -> J h a
pathConFunction pathC (t :*: parm :*: o :*: collPoint) =
  pathC x z u parm o t
  where
    CollPoint x z u = split collPoint

-- return dynamics constraints, outputs, and interpolated state
dynStageConstraints ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (J S :*: J p :*: J x :*: J (CollPoint x z u))
           (J r :*: J o)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J S :*: J p :*: J (JVec deg S)) MX
  -> (J (JVec deg r) :*: J x :*: J (JVec deg o)) MX
dynStageConstraints cijs taus dynFun (x0 :*: xzs' :*: us' :*: UnsafeJ h :*: p :*: stageTimes') =
  cat (JVec dynConstrs) :*: xnext :*: cat (JVec outputs)
  where
    xzs = fmap split (unJVec (split xzs')) :: Vec deg (JTuple x z MX)
    us = unJVec (split us') :: Vec deg (J u MX)

    -- interpolated final state
    xnext :: J x MX
    xnext = interpolate taus x0 xs

    stageTimes = unJVec $ split stageTimes'

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    outputs :: Vec deg (J o MX)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith4 applyDae xdots xzs us stageTimes

    applyDae :: J x MX -> JTuple x z MX -> J u MX -> J S MX -> (J r MX, J o MX)
    applyDae x' (JTuple x z) u t = (r, o)
      where
        r :*: o = callSXFun dynFun (t :*: p :*: x' :*: collPoint)
        collPoint = cat (CollPoint x z u)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (/ UnsafeJ h) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap (\(JTuple x _) -> x) xzs


-- return error dynamics constraints and interpolated state
errorDynStageConstraints ::
  forall x z u p sx sz sw sr deg .
  (Dim deg, View x, View z, View u, View p,
   View sr, View sw, View sz, View sx)
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> Vec deg Double
  -> SXFun (J S :*: J p :*: J x :*: J (CollPoint x z u) :*: J sx :*: J sx :*: J sz :*: J sw)
           (J sr)
  -> ( (J sx :*: J sw :*: J (JVec deg (JTuple sx sz))) MX
     , (J x :*: J (JVec deg (CollPoint x z u)) :*: J S :*: J p :*: J (JVec deg S)) MX
     )
  -> (J (JVec deg sr) :*: J sx) MX
errorDynStageConstraints cijs taus dynFun
  ( sx0 :*: sw0 :*: sxzs'
  , x0 :*: xzus' :*: UnsafeJ h :*: p :*: stageTimes'
  ) = cat (JVec dynConstrs) :*: sxnext
  where
    xzus = unJVec (split xzus')

    xs :: Vec deg (J x MX)
    xs = fmap ((\(CollPoint x _ _) -> x) . split) xzus

    xdots :: Vec deg (J x MX)
    xdots = fmap (/ UnsafeJ h) $ interpolateXDots cijs (x0 TV.<| xs)

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
         -> J x MX -> J (CollPoint x z u) MX -> J S MX
         -> J sr MX
    applyDae sx' sx sz x' xzu t =
      callSXFun dynFun
      (t :*: p :*: x' :*: xzu :*: sx' :*: sx :*: sz :*: sw0)

    -- error state derivatives
    sxdots :: Vec deg (J sx MX)
    sxdots = fmap (/ UnsafeJ h) $ interpolateXDots cijs (sx0 TV.<| sxs)

    sxs :: Vec deg (J sx MX)
    szs :: Vec deg (J sz MX)
    (sxs, szs) = TV.tvunzip
                 $ fmap ((\(JTuple sx sz) -> (sx,sz)) . split)
                 $ unJVec $ split sxzs'


-- outputs
outputFunction ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (J S :*: J p :*: J x :*: J (CollPoint x z u))
           (J r :*: J o)
  -> (J (CollStage x z u deg) :*: J p :*: J S :*: J S) MX
  -> (J (JVec deg r) :*: J (JVec deg o)) MX
outputFunction cijs taus dynFun (collStage :*: p :*: h'@(UnsafeJ h) :*: k) =
  cat (JVec dynConstrs) :*: cat (JVec outputs)
  where
    xzus = unJVec (split xzus') :: Vec deg (J (CollPoint x z u) MX)
    CollStage x0 xzus' = split collStage
    -- times at each collocation point
    stageTimes :: Vec deg (J S MX)
    stageTimes = fmap (\tau -> t0 + realToFrac tau * h') taus
    t0 = k*h'

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    outputs :: Vec deg (J o MX)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots xzus stageTimes

    applyDae :: J x MX -> J (CollPoint x z u) MX -> J S MX -> (J r MX, J o MX)
    applyDae x' xzu t = (r, o)
      where
        r :*: o = callSXFun dynFun (t :*: p :*: x' :*: xzu)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (/ UnsafeJ h) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap ((\(CollPoint x _ _) -> x) . split) xzus




-- return dynamics constraints, outputs, and interpolated state
pathStageConstraints ::
  forall x z u p o h deg . (Dim deg, View x, View z, View u, View p, View o, View h)
  => SXFun (J S :*: J p :*: J o :*: J (CollPoint x z u))
           (J h)
  -> (J p :*: J (JVec deg S) :*: J (JVec deg o) :*: J (JVec deg (CollPoint x z u))) MX
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

    applyH :: CollPoint x z u MX -> J S MX -> J o MX -> J h MX
    applyH (CollPoint x z u) t o = pathc'
      where
        pathc' = callSXFun pathCFun (t :*: p :*: o :*: collPoint)
        collPoint = cat (CollPoint x z u)


stageFunction ::
  forall x z u p o r h deg . (Dim deg, View x, View z, View u, View p, View r, View o, View h)
  => MXFun (J p :*: J (JVec deg S) :*: J (JVec deg o) :*: J (JVec deg (CollPoint x z u)))
           (J (JVec deg h))
  -> ((J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J S :*: J p :*: J (JVec deg S)) MX
      -> (J (JVec deg r) :*: J x :*: J (JVec deg o)) MX)
  -> (J S :*: J p :*: J (JVec deg S) :*: J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u)) MX
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
    hs = callMXFun pathConStageFun (parm :*: stageTimes :*: outputs :*: collPoints)


covStageFunction ::
  forall x z u p sx sz sw deg . (Dim deg, View x, View z, View u, View p, View sx, View sz, View sw)
  => (( (J sx :*: J sw :*: J (JVec deg (JTuple sx sz))) MX
      , (J x :*: J (JVec deg (CollPoint x z u)) :*: J S :*: J p :*: J (JVec deg S)) MX
      )
      -> Vector (Vector MX))
  -> (J (Cov sx) :*: J S :*: J p :*: J (JVec deg S)
      :*: J x :*: J (JVec deg (CollPoint x z u)) :*: J (Cov sw)) MX
  -> J (Cov sx) MX
covStageFunction dynStageConJac
  (p0' :*: dt :*: parm :*: stageTimes :*: x0' :*: xzus' :*: q0') =
    p1
  where
    sx0  = mkJ $ zeros (size (Proxy :: Proxy sx),                        1)
    sw0  = mkJ $ zeros (size (Proxy :: Proxy sw),                        1)
    sxzs = mkJ $ zeros (size (Proxy :: Proxy (JVec deg (JTuple sx sz))), 1)
    jac = dynStageConJac ( sx0 :*: sw0 :*: sxzs
                         , x0' :*: xzus' :*: dt :*: parm :*: stageTimes
                         )

    ((df_dsx0, df_dsw0, df_dsxz), (dg_dsx0, dg_dsw0, dg_dsxz)) = case fmap F.toList (F.toList jac) of
      [[x00,x01,x02],[x10,x11,x12]] -> ((x00,x01,x02),(x10,x11,x12))
      _ -> error "stageFunction: got wrong number of elements in jacobian"

    q0 = toMatrix' q0'
    p0 = toMatrix' p0'

    -- TODO: check these next 4 lines
    dsxz_dsx0 = - (solve df_dsxz df_dsx0) :: MX
    dsxz_dsw0 = - (solve df_dsxz df_dsw0) :: MX

    dsx1_dsx0 = dg_dsx0 + dg_dsxz `mm` dsxz_dsx0
    dsx1_dsw0 = dg_dsw0 + dg_dsxz `mm` dsxz_dsw0

    p1' :: MX
    p1' = dsx1_dsx0 `mm` p0 `mm` trans dsx1_dsx0 +
          dsx1_dsw0 `mm` q0 `mm` trans dsx1_dsw0

    -- supress casadi zero size matrix error
    p1 :: J (Cov sx) MX
    p1 = if size (Proxy :: Proxy sx) == 0 then p0' else fromMatrix' p1'


-- | make an initial guess
makeGuess ::
  forall x z u p deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize p)
  => Double -> (Double -> x Double) -> (Double -> z Double) -> (Double -> u Double)
  -> p Double
  -> CollTraj x z u p n deg (Vector Double)
makeGuess tf guessX guessZ guessU parm =
  CollTraj (jfill tf) (v2j parm) guesses (v2j (guessX tf))
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
      CollStage (v2j (guessX t)) $
      cat $ JVec $ fmap (\t' -> cat (CollPoint (v2j (guessX t')) (v2j (guessZ t')) (v2j (guessU t')))) ts

    guesses :: J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) (Vector Double)
    guesses = cat $ JVec $ fmap (cat . mkGuess') times

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus deg

    deg = vlength (Proxy :: Proxy (Vec deg))

    v2j :: Vectorize v => v Double -> J (JV v) (Vector Double)
    v2j = mkJ . vectorize
