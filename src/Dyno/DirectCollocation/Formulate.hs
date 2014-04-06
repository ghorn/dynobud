{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}

module Dyno.DirectCollocation.Formulate
       ( makeCollNlp
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

import JacobiRoots ( shiftedLegendreRoots )

import Dyno.Cov
import Dyno.View
import Dyno.Vectorize ( Vectorize(..), fill, vlength, vzipWith )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs , lagrangeXis )
import Dyno.Nlp ( Nlp'(..), Bounds )
import Dyno.Ocp ( OcpPhase(..) )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollStage(..), CollPoint(..), CollOcpConstraints(..), ctN )
import Dyno.Casadi.MX ( solve, mm, trans, d2m )
import Dyno.Casadi.SXElement ( SXElement )
import Dyno.Casadi.SX ( sdata, sdense, svector )
import Dyno.Casadi.DMatrix ( dvector )

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
  forall x z u p r o c h s sh sc deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize r, Vectorize o, Vectorize h, Vectorize c, View s, View sh, View sc) =>
  OcpPhase x z u p r o c h s sh sc ->
  IO (Nlp' (CollTraj x z u p s n deg) JNone (CollOcpConstraints n deg x r c h sh sc) MX)
makeCollNlp ocp = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus deg

      deg = reflectDim (Proxy :: Proxy deg)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
      cijs = lagrangeDerivCoeffs (0 TV.<| taus)

  bcFun <- toSXFun "bc" $ \(x0:*:x1) -> re $ ocpBc ocp (de x0) (de x1)
  sbcFun <- toSXFun "sbc" $ \(x0:*:x1) -> ocpSbc ocp x0 x1
  shFun <- toSXFun "sh" $ \(x0:*:x1) -> ocpSh ocp (de x0) x1
  mayerFun <- toSXFun "mayer" $ \(x0:*:x1:*:x2:*:x3:*:x4) ->
    re' $ ocpMayer ocp (de' x0) (de x1) (de x2) x3 x4
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

  dynStageConFunJac <- toFunJac' "dynamicsStageConJac" (dynStageConstraints cijs taus dynFun)
  dynStageConFun <- toMXFun "dynamicsStageCon" (dynStageConstraints' cijs taus dynFun)

  covStageFun <- toMXFun "covStageFunction" $ covStageFunction dynStageConFunJac
--  let callCovStageFun = callMXFun covStageFun
  callCovStageFun <- fmap callSXFun (expandMXFun covStageFun)

  stageFun <- toMXFun "stageFunction" $ stageFunction pathStageConFun (callMXFun dynStageConFun)
--  let callStageFun = callMXFun stageFun
  callStageFun <- fmap callSXFun (expandMXFun stageFun)

  return Nlp' {
    nlpFG' =
       getFg taus
       (ocpSq ocp :: J (Cov s) DMatrix)
       (bcFun :: SXFun (J (JV x) :*: J (JV x)) (J (JV c)))
       (sbcFun :: SXFun (J (Cov s) :*: J (Cov s)) (J sc))
       (shFun :: SXFun (J (JV x) :*: J (Cov s)) (J sh))
       (mayerFun :: SXFun (J S :*: (J (JV x) :*: (J (JV x) :*: (J (Cov s) :*: J (Cov s))))) (J S))
       (callQuadFun :: (J (JV p) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (JVec deg (JV o)) :*: J S :*: J (JVec deg S)) MX
                    -> J S MX)
       (callStageFun :: (J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u))) MX
                  -> (J (JVec deg (JV r)) :*: J (JVec deg (JV o)) :*: J (JVec deg (JV h)) :*: J (JV x)) MX)
       (callCovStageFun :: (J (Cov s) :*: J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u)) :*: J (Cov s)) MX
                        -> J (Cov s) MX)
    , nlpBX' = cat (getBx ocp)
    , nlpBG' = cat (getBg ocp)
    , nlpX0' = jfill 0
    , nlpP' = cat JNone
    }


mkTaus :: Fractional a => Int -> Vec deg a
mkTaus deg = case shiftedLegendreRoots deg of
  Just taus -> TV.mkVec $ V.map (fromRational . toRational) taus
  Nothing -> error "makeTaus: too high degree"

getFg ::
  forall z x u p r o c h s sh sc n deg .
  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize r, Vectorize o, Vectorize c, Vectorize h,
   View s, View sc, View sh)
  => Vec deg Double
  -> J (Cov s) DMatrix
  -> SXFun (J (JV x) :*: J (JV x)) (J (JV c))
  -> SXFun (J (Cov s) :*: J (Cov s)) (J sc)
  -> SXFun (J (JV x) :*: J (Cov s)) (J sh)
  -> SXFun
      (J S :*: J (JV x) :*: J (JV x) :*: J (Cov s) :*: J (Cov s)) (J S)
  -> ((J (JV p) :*: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) :*: J (JVec deg (JV o)) :*: J S :*: J (JVec deg S)) MX ->
      (J S) MX)
  -> ((J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u))) MX -> (J (JVec deg (JV r)) :*: J (JVec deg (JV o)) :*: J (JVec deg (JV h)) :*: J (JV x)) MX)
  -> ((J (Cov s) :*: J S :*: J (JV p) :*: J (JVec deg S) :*: J (JV x) :*: J (JVec deg (JTuple (JV x) (JV z))) :*: J (JVec deg (JV u)) :*: J (Cov s)) MX -> J (Cov s) MX)
  -> J (CollTraj x z u p s n deg) MX
  -> J JNone MX
  -> (J S MX, J (CollOcpConstraints n deg x r c h sh sc) MX)
getFg taus sq bcFun sbcFun shFun mayerFun quadFun stageFun covStageFun collTraj _ = (obj, cat g)
  where
    -- split up the design vars
    ct@(CollTraj tf p0 parm stages' xf) = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    spstagesPoints :: Vec n (J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX)
    spstagesPoints = fmap (\(CollStage _ cps) -> cps) spstages

    obj = objLagrange + objMayer

    objMayer = callSXFun mayerFun (tf :*: x0 :*: xf :*: p0 :*: pF)

    objLagrange :: J S MX
    objLagrange = F.sum $ TV.tvzipWith3 oneStage spstagesPoints outputs times'
    oneStage :: J (JVec deg (CollPoint (JV x) (JV z) (JV u))) MX -> J (JVec deg (JV o)) MX -> J (JVec deg S) MX
                -> J S MX
    oneStage stagePoints stageOutputs stageTimes =
      quadFun (parm :*: stagePoints :*: stageOutputs :*: dt :*: stageTimes)

    -- timestep
    dt = tf / fromIntegral n
    n = ctN ct

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
        , coCovPathC = cat (JVec covPathConstraints)
        , coBc = callSXFun bcFun (x0 :*: xf)
        , coSbc = callSXFun sbcFun (p0 :*: pF)
        }

    -- Q
    covInjections :: Vec n (J (Cov s) MX)
    covInjections = fill (mkJ (d2m (unJ sq)))

    covPathConstraints :: Vec n (J sh MX)
    covPathConstraints = TV.tvzipWith (\xk pk -> callSXFun shFun (xk:*:pk)) x0s covs

    integratorMatchingConstraints :: Vec n (J (JV x) MX) -- THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = vzipWith (-) interpolatedXs xfs

    (pF, covs) = T.mapAccumL ff p0 $ TV.tvzip3 spstages times' covInjections

    ff :: J (Cov s) MX -> (CollStage (JV x) (JV z) (JV u) deg MX, J (JVec deg S) MX, J (Cov s) MX) ->
          (J (Cov s) MX, J (Cov s) MX)
    ff cov0 (CollStage x0' xzus, stageTimes, covInj) = (cov1, cov0)
      where
        cov1 =
          covStageFun (cov0 :*: dt :*: parm :*: stageTimes :*: x0' :*: xzs :*: us :*: covInj)

        xzs = cat (JVec xzs') :: J (JVec deg (JTuple (JV x) (JV z))) MX
        us = cat (JVec us') :: J (JVec deg (JV u)) MX
        (xzs', us') = TV.tvunzip $ fmap toTuple $ unJVec (split xzus)
        toTuple xzu = (cat (JTuple x z), u)
          where
            CollPoint x z u = split xzu

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



getBx :: forall x z u p r o c h s sh sc n deg .
         (Dim n, Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize p)
         => OcpPhase x z u p r o c h s sh sc
         -> CollTraj x z u p s n deg (Vector Bounds)
getBx ocp = ct
  where
    ct :: CollTraj x z u p s n deg (Vector Bounds)
    ct = CollTraj (mkJ (V.singleton tb)) sb pb (jreplicate (cat cs)) xb

    cs :: CollStage (JV x) (JV z) (JV u) deg (Vector Bounds)
    cs = CollStage xb (jreplicate (cat cp))

    cp :: CollPoint (JV x) (JV z) (JV u) (Vector Bounds)
    cp = CollPoint xb zb ub
    xb = mkJ $ vectorize $ ocpXbnd ocp
    ub = mkJ $ vectorize $ ocpUbnd ocp
    zb = mkJ $ vectorize $ ocpZbnd ocp
    pb = mkJ $ vectorize $ ocpPbnd ocp
    tb = ocpTbnd ocp

    sb = ocpSbnd ocp

getBg :: forall x z u p r o c h s sh sc deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize r, Vectorize c, Vectorize h, View sh, View sc)
  => OcpPhase x z u p r o c h s sh sc
  -> CollOcpConstraints n deg x r c h sh sc (Vector Bounds)
getBg ocp =
  CollOcpConstraints
  { coCollPoints = jreplicate (jfill (Just 0, Just 0)) -- dae residual constraint
  , coContinuity = jreplicate (jfill (Just 0, Just 0)) -- continuity constraint
  , coPathC = jreplicate (jreplicate hbnds)
  , coCovPathC = jreplicate (ocpShBnds ocp) -- covariance path constraint
  , coBc = mkJ $ vectorize $ ocpBcBnds ocp
  , coSbc = ocpSbcBnds ocp
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

-- return dynamics constraints and interpolated state
dynStageConstraints ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (J S :*: J p :*: J x :*: J (CollPoint x z u))
           (J r :*: J o)
  -> ((J x :*: J (JVec deg (JTuple x z))) MX, (J (JVec deg u) :*: J S :*: J p :*: J (JVec deg S)) MX)
  -> (J (JVec deg r) :*: J x) MX
dynStageConstraints cijs taus dynFun (x0 :*: xzs, us :*: h :*: p :*: stageTimes) =
  dyncon :*: xnext
  where
    dyncon :*: xnext :*: _ =
      dynStageConstraints' cijs taus dynFun (x0 :*: xzs :*: us :*: h :*: p :*: stageTimes)

-- return dynamics constraints, outputs, and interpolated state
dynStageConstraints' ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (J S :*: J p :*: J x :*: J (CollPoint x z u))
           (J r :*: J o)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J S :*: J p :*: J (JVec deg S)) MX
  -> (J (JVec deg r) :*: J x :*: J (JVec deg o)) MX
dynStageConstraints' cijs taus dynFun (x0 :*: xzs' :*: us' :*: UnsafeJ h :*: p :*: stageTimes') =
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
  forall x z u p s deg . (Dim deg, View x, View z, View u, View p, View s)
  => (((J x :*: J (JVec deg (JTuple x z))) MX,
                    (:*:) (J (JVec deg u)) (J S :*: J p :*: J (JVec deg S)) MX)
                   -> Vector (Vector MX))
  -> (J (Cov s) :*: J S :*: J p :*: J (JVec deg S) :*: J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J (Cov s)) MX
  -> J (Cov s) MX
covStageFunction dynStageConJac
  (p0' :*: dt :*: parm :*: stageTimes :*: x0' :*: xzs' :*: us :*: q0') =
    p1
  where
    jac = dynStageConJac (x0' :*: xzs', us :*: dt :*: parm :*: stageTimes)

    --((df_dx0, dg_dx0), (df_dxz, dg_dxz)) = case fmap F.toList (F.toList jac) of
    ((df_dx0, df_dxz), (dg_dx0, dg_dxz)) = case fmap F.toList (F.toList jac) of
      [[x00,x01],[x10,x11]] -> ((x00,x01),(x10,x11))
      _ -> error "stageFunction: got wrong number of elements in jacobian"

    q0 = toMatrix' q0'
    p0 = toMatrix' p0'

    dxz_dx0 = - (solve df_dxz df_dx0) :: MX
    dx1_dx0 = dg_dx0 + dg_dxz `mm` dxz_dx0

    p1' :: MX
    p1' = dx1_dx0 `mm` p0 `mm` trans dx1_dx0 + q0

    -- supress casadi zero size matrix error
    p1 :: J (Cov s) MX
    p1 = if size (Proxy :: Proxy s) == 0 then p0' else fromMatrix' p1'


interpolate :: (Dim deg, Real b, Fractional b, Fractional (J x a), View x) =>
               Vec deg b -> J x a -> Vec deg (J x a) -> J x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : F.toList taus) 1) [0..deg]
    deg = TV.tvlength taus


-- | make an initial guess
makeGuess ::
  forall x z u p s deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize p)
  => Double -> (Double -> x Double) -> (Double -> z Double) -> (Double -> u Double)
  -> J (Cov s) DMatrix -> p Double
  -> CollTraj x z u p s n deg DMatrix
makeGuess tf guessX guessZ guessU cov' parm =
  CollTraj (mkJ (realToFrac tf)) cov' (v2j parm) guesses (v2j (guessX tf))
  where
    -- timestep
    dt = tf / fromIntegral n
    n = vlength (undefined :: Vec n ())

    -- initial time at each collocation stage
    t0s :: Vec n Double
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Double, Vec deg Double)
    times = fmap (\t0 -> (t0, fmap (\tau -> t0 + tau*dt) taus)) t0s

    mkGuess' :: (Double, Vec deg Double) -> CollStage (JV x) (JV z) (JV u) deg DMatrix
    mkGuess' (t,ts) =
      CollStage (v2j (guessX t)) $
      cat $ JVec $ fmap (\t' -> cat (CollPoint (v2j (guessX t')) (v2j (guessZ t')) (v2j (guessU t')))) ts

    guesses :: J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) DMatrix
    guesses = cat $ JVec $ fmap (cat . mkGuess') times

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus deg

    deg = vlength (undefined :: Vec deg ())

    v2j :: Vectorize v => v Double -> J (JV v) DMatrix
    v2j = mkJ . dvector . vectorize
