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

--import Debug.Trace
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Matrix hiding ( trace )
import Linear.V

import JacobiRoots ( shiftedLegendreRoots )

import Dyno.Cov
import Dyno.Vectorize
import Dyno.View.View
import Dyno.View.Viewable ( Viewable )
import Dyno.View.Symbolic
import Dyno.View.Function
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs , lagrangeXis )
import Dyno.Nlp ( Nlp(..), Bounds )
import Dyno.Ocp ( OcpPhase(..), Dae )

import Dyno.DirectCollocation.Types
import Dyno.Casadi.SX -- ( smm, sjacobian, ssize2, ssize1, strans, ssolve )
import Dyno.Casadi.MX ( d2m )

--data RorL = Radau | Legendre deriving (Eq, Show)

mkTaus :: Fractional a => Int -> Vec deg a
mkTaus deg = case shiftedLegendreRoots deg of
  Just taus -> TV.mkVec $ V.map (fromRational . toRational) taus
  Nothing -> error "makeTaus: too high degree"

getFg :: forall z x u p r o c h s sh sc n deg .
         (Dim deg, Dim n, View x, View z, View u, View p, View r, View c, View h, View s, View sc, View sh) =>
         OcpPhase x z u p r o c h s sh sc ->
         (SXFun
          ((J (Cov s) :*: J (CollStage x z u deg) :*: J (CollDynConstraint deg r) :*: J x :*: J (Cov s)))
          (J (Cov s))) ->
         (J (CollTraj x z u p s n deg) MX, J JNone MX) ->
         (J S MX, J (CollOcpConstraints n deg x r c h sh sc) MX)
getFg ocp pCovFun (collTraj, _) =
  (obj, cat g)
  where
    -- split up the design vars
    ct@(CollTraj tf p0 parm stages' xf) = split collTraj
    --stages = split stages' :: JVec n (CollStage x z u deg) MX
    stages = unJVec (split stages') :: Vec n (J (CollStage x z u deg) MX)
    spstages = fmap split stages :: Vec n (CollStage x z u deg MX)

    collPoints :: Vec n (Vec deg (CollPoint x z u MX))
    collPoints = fmap (\(CollStage _ cps) -> fmap split (unJVec (split cps))) spstages
    
    obj = objLagrange + objMayer

    objMayer = ocpMayer ocp tf x0 xf p0 pF
    objLagrange = evaluateQuadratures (ocpLagrange ocp) parm spstages outputs h taus times

    -- timestep
    h = tf / fromIntegral n
    n = ctN ct

    -- initial time at each collocation stage
    t0s :: Vec n (J S MX)
    t0s = TV.mkVec' $ take n [h * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Vec deg (J S MX))
    times = fmap (\t0 -> fmap (\tau -> t0 + (realToFrac tau)*h) taus) t0s

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus deg

    deg = ctDeg ct

    -- coefficients for getting xdot by lagrange interpolating polynomials
    cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
    cijs = lagrangeDerivCoeffs (0 TV.<| taus)

    -- initial point at each stage
    x0s :: Vec n (J x MX)
    x0s = fmap (\(CollStage x0' _) -> x0') spstages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (J x MX)
    xfs = TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead spstages)
    g = CollOcpConstraints
        { coStages = cat (CollTrajConstraints (cat (JVec (fmap cat stageConstraints))))
        , coPathC = cat $ JVec (TV.tvzipWith3 mkPathConstraints collPoints outputs times)
        , coBc = ocpBc ocp x0 xf
        , coSbc = (ocpSc ocp) p0 pF
        }
    pF = TV.tvlast ps

    -- Q
    covInjections :: Vec n (J (Cov s) MX)
    covInjections = fill (mkJ (d2m (unJ (ocpSq ocp))))

    cmf :: J (Cov s) MX -> (J (CollStage x z u deg) :*: J (CollDynConstraint deg r) :*: J x :*: J (Cov s)) MX
           -> J (Cov s) MX
    cmf pk blahs = callSXFun pCovFun (pk :*: blahs)

    ps :: Vec n (J (Cov s) MX)
    ps = tvscanl' cmf p0 (TV.tvzipWith4 (\x y z u -> x:*:cat y:*:z:*:u) stages dcs interpolatedX covInjections)
      where
        tvscanl' f acc0 vs = devectorize $ V.scanl' f acc0 (vectorize vs)

    stageConstraints :: Vec n (CollStageConstraints x deg r sh MX)
    stageConstraints = vzipWith3 (\dc -> CollStageConstraints (cat dc))
                       dcs integratorMatchingConstraints covPathConstraints

    covPathConstraints :: Vec n (J sh MX)
    covPathConstraints = TV.tvzipWith (ocpSh ocp) x0s (TV.tvshiftr p0 ps)

    integratorMatchingConstraints :: Vec n (J x MX) -- THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = vzipWith (-) interpolatedX xfs

    dcs :: Vec n (CollDynConstraint deg r MX)
    outputs :: Vec n (Vec deg (J o MX))
    interpolatedX :: Vec n (J x MX)
    (dcs,outputs, interpolatedX) =
      TV.tvunzip3 $
      TV.tvzipWith (dynConstraints cijs (ocpDae ocp) taus h parm) times spstages

    mkPathConstraints :: Vec deg (CollPoint x z u MX) -> Vec deg (J o MX) ->
                         Vec deg (J S MX) -> J (JVec deg h) MX
    mkPathConstraints cps outs = cat . JVec . TV.tvzipWith3 mkPathC cps outs
      where
        mkPathC :: CollPoint x z u MX -> J o MX -> J S MX -> J h MX
        mkPathC (CollPoint x z u) = ocpPathC ocp x z u parm

makeCovFun ::
  (Dim deg, View x, View z, View u, View r, View s) =>
  IO (SXFun
      ((J (Cov s) :*: J (CollStage x z u deg) :*: J (CollDynConstraint deg r) :*: J x :*: J (Cov s)))
      (J (Cov s)))
makeCovFun = toSXFun propogateCovariance



makeCollNlp ::
  forall x z u p r o c h s sh sc deg n .
  (Dim deg, Dim n, View x, View p, View u, View z,
   View r, View o, View h, View c, View s, View sh, View sc) =>
  OcpPhase x z u p r o c h s sh sc ->
  IO (Nlp (CollTraj x z u p s n deg) JNone (CollOcpConstraints n deg x r c h sh sc) MX)
makeCollNlp ocp = do
  pcf <- makeCovFun
--  print pcf
  return $ Nlp { nlpFG = getFg ocp pcf
               , nlpBX = cat (getBx ocp)
               , nlpBG = cat (getBg ocp)
               , nlpX0 = jfill 0
               , nlpP = cat JNone
               }

getBx :: (Dim n, Dim deg, View x, View z, View u)
         => OcpPhase x z u p r o c h s sh sc
         -> CollTraj x z u p s n deg (Vector Bounds)
getBx ocp = ct
  where
    --ct :: CollTraj x z u p s n deg (Vector Bounds)
    ct = CollTraj tb sb pb (jreplicate (cat cs)) xb

    --cs :: CollStage x z u deg (Vector Bounds)
    cs = CollStage xb (jreplicate (cat cp))

    --cp :: CollPoint x z u (Vector Bounds)
    cp = CollPoint xb zb ub
    xb = ocpXbnd ocp
    ub = ocpUbnd ocp
    zb = ocpZbnd ocp
    pb = ocpPbnd ocp
    tb = ocpTbnd ocp

    sb = ocpSbnd ocp

getBg :: forall x z u p r o c h s sh sc deg n .
  (Dim n, Dim deg, View x, View r, View c, View h, View sh, View sc)
  => OcpPhase x z u p r o c h s sh sc ->
  CollOcpConstraints n deg x r c h sh sc (Vector Bounds)
getBg ocp =
  CollOcpConstraints
  { coStages = cat $ CollTrajConstraints (jreplicate (cat stageCon))
  , coPathC = jreplicate (jreplicate (ocpPathCBnds ocp))
  , coBc = ocpBcBnds ocp
  , coSbc = ocpScBnds ocp
  }
  where
    stageCon :: CollStageConstraints x deg r sh (Vector Bounds)
    stageCon = CollStageConstraints
               (jfill (Just 0, Just 0)) -- dae residual constraint
               (jfill (Just 0, Just 0)) -- continuity constraint
               (ocpShBnds ocp) -- covariance path constraint

evaluateQuadratures ::
  forall x z u p o n deg a .
  (Dim deg, Dim n, View x, View z, View u, Fractional (J S a), Viewable a) =>
  (J x a -> J z a -> J u a -> J p a -> J o a -> J S a -> J S a) ->
  J p a -> Vec n (CollStage x z u deg a) -> Vec n (Vec deg (J o a)) -> J S a ->
  Vec deg Double -> Vec n (Vec deg (J S a)) -> J S a
evaluateQuadratures f p stages outputs h taus times =
  (h *) $ V.sum $ TV.unVec $ TV.tvzipWith3 oneStage stages outputs times
  where
    oneStage :: CollStage x z u deg a -> Vec deg (J o a) -> Vec deg (J S a) -> J S a
    oneStage (CollStage _ stage) output stageTimes = qnext
      where
        qdots :: Vec deg (J S a)
        qdots = TV.tvzipWith3 (\(CollPoint x z u) o t -> f x z u p o t) stagelol output stageTimes
        stagelol = (fmap split (unJVec (split stage))) :: Vec deg (CollPoint x z u a)

        qs = cijInvFr !* qdots

        qnext = interpolate taus 0 qs

    cijs' :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
    cijs' = lagrangeDerivCoeffs (0 TV.<| taus)

    cijs :: Vec deg (Vec deg Double)
    cijs = TV.tvtail $ fmap TV.tvtail cijs'

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs

    cijInv' :: Mat.Matrix Double
    cijInv' = LA.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (J S a))
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


-- return dynamics constraints, outputs, and interpolated state
dynConstraints ::
  forall x z u p r o deg a . (Dim deg, View x, View z, View u, View r, Viewable a, Fractional (J x a))
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Dae x z u p r o a ->
  Vec deg Double -> J S a -> J p a -> Vec deg (J S a) ->
  CollStage x z u deg a ->
  (CollDynConstraint deg r a, Vec deg (J o a), J x a)
dynConstraints cijs dae taus (UnsafeJ h) p stageTimes (CollStage x0 cps') =
  (CollDynConstraint (cat (JVec dynConstrs)), outputs, xnext)
  where
    -- interpolated final state
    xnext :: J x a
    xnext = interpolate taus x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r a)
    outputs :: Vec deg (J o a)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots cps stageTimes

    applyDae :: J x a -> CollPoint x z u a -> J S a -> (J r a, J o a)
    applyDae x' (CollPoint x z u) = dae x' x z u p

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x a)
    xdots = fmap (/(UnsafeJ h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x a)
    xs = fmap getX cps

    cps = fmap split (unJVec (split cps')) :: Vec deg (CollPoint x z u a)


propogateCovariance ::
  forall x z u s r deg . (Dim deg, View x, View z, View u, View r, View s)
  => (J (Cov s) :*: J (CollStage x z u deg) :*: J (CollDynConstraint deg r) :*: J x :*: J (Cov s)) SX
  -> J (Cov s) SX
propogateCovariance (p0' :*: collStage :*: collDynConstraint :*: interpolatedX :*: q0') =
  if size (Proxy :: Proxy s) == 0
  then p0' -- supress casadi zero size matrix error
  else 
--    (x0',
--     (ssize1 (unJ x0'), ssize2 (unJ x0')),
--     (ssize1 df_dx0, ssize2 df_dx0),
--     (ssize1 df_dz, ssize2 df_dz),
--     (ssize1 dz_dx0, ssize2 dz_dx0),
--     (ssize1 dx1_dx0, ssize2 dx1_dx0),
--     (ssize1 p1, ssize2 p1)
--    )
--    `traceShow`
    fromMatrix p1
  where
    CollStage x0 cps' = split collStage
    CollDynConstraint dynConstrs = split collDynConstraint
    
    q0 = toMatrix q0'
    p0 = toMatrix p0'

    f' :: J (JVec deg r) SX
    f' = dynConstrs

    z' :: J (JVec deg (JTuple x z)) SX
    z' = cat (JVec z'') :: J (JVec deg (JTuple x z)) SX
      where
        z'' = fmap (\(CollPoint x z _) -> cat (JTuple x z)) cps :: Vec deg (J (JTuple x z) SX)
        cps :: Vec deg (CollPoint x z u SX)
        cps = fmap split cps''
          where
            cps'' :: Vec deg (J (CollPoint x z u) SX)
            cps'' = unJVec $ split cps'

    x0' :: J x SX
    x0' = x0

    -- make this sparse!
    df_dx0 = sjacobian (unJ f') (unJ x0')
    df_dz = sjacobian (unJ f') (unJ z')
    dz_dx0 = - (ssolve df_dz df_dx0)

    g' = unJ interpolatedX
    dg_dx0 = sjacobian g' (unJ x0')
    dg_dz = sjacobian g' (unJ z')
    dx1_dx0 = dg_dx0 + smm dg_dz dz_dx0

    p1 = dx1_dx0 `smm` p0 `smm` (strans dx1_dx0) + q0


interpolate :: (Dim deg, Real b, Fractional b, Fractional (J x a), View x) =>
               Vec deg b -> J x a -> Vec deg (J x a) -> J x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : F.toList taus) 1) [0..deg]
    deg = TV.tvlength taus


-- | make an initial guess
makeGuess ::
  forall x z u p s deg n .
  (Dim n, Dim deg, View x, View z, View u)
  => Double -> (Double -> J x DMatrix) -> (Double -> J z DMatrix) -> (Double -> J u DMatrix)
  -> J (Cov s) DMatrix -> J p DMatrix
  -> CollTraj x z u p s n deg DMatrix -- (Vector Double)
makeGuess tf guessX guessZ guessU cov' parm =
  CollTraj (mkJ (realToFrac tf)) cov' parm guesses (guessX tf)
  where

    -- timestep
    h = tf / fromIntegral n
    n = vlength (undefined :: Vec n ())

    -- initial time at each collocation stage
    t0s :: Vec n Double
    t0s = TV.mkVec' $ take n [h * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Double, Vec deg Double)
    times = fmap (\t0 -> (t0, fmap (\tau -> t0 + tau*h) taus)) t0s

    mkGuess' :: (Double, Vec deg Double) -> CollStage x z u deg DMatrix
    mkGuess' (t,ts) = CollStage (guessX t) $
                      cat $ JVec $ fmap (\t' -> cat (CollPoint (guessX t') (guessZ t') (guessU t'))) ts

    guesses :: J (JVec n (CollStage x z u deg)) DMatrix
    guesses = cat $ JVec $ fmap (cat . mkGuess') times

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus deg

    deg = vlength (undefined :: Vec deg ())
