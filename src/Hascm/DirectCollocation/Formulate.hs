{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Hascm.DirectCollocation.Formulate
       ( makeCollNlp
       , mkTaus
       , interpolate
       , makeGuess
       ) where

--import Debug.Trace
import Control.Applicative ( pure )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Vector
import Linear.Matrix hiding ( trace )
import Linear.V

import JacobiRoots ( shiftedLegendreRoots )

import Hascm.Cov
import Hascm.Casadi.SXElement ( SXElement )
import Hascm.Casadi.SX
import Hascm.Vectorize
import qualified Hascm.TypeVecs as TV
import Hascm.TypeVecs ( Vec, Succ )
import Hascm.LagrangePolynomials ( lagrangeDerivCoeffs , lagrangeXis )
import Hascm.Nlp ( Nlp(..), NlpInputs(..), NlpFun(..) )
import Hascm.Ocp ( OcpPhase(..), Dae )

import Hascm.DirectCollocation.Types

--data RorL = Radau | Legendre deriving (Eq, Show)

mkTaus :: Fractional a => Int -> Vec deg a
mkTaus deg = case shiftedLegendreRoots deg of
  Just taus -> TV.mkVec $ V.map (fromRational . toRational) taus
  Nothing -> error "makeTaus: too high degree"

getFg :: forall z x u p r o c h s sh sc n deg .
         (Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize r, Vectorize s, Dim n) =>
         OcpPhase x z u p r o c h s sh sc -> NlpInputs (CollTraj x z u p s n deg) None SXElement ->
         NlpFun (CollOcpConstraints n deg x r c h sh sc) SXElement
getFg ocp (NlpInputs collTraj@(CollTraj tf p0 parm stages xf) _) = NlpFun obj g
  where
    obj = objLagrange + objMayer

    objMayer = ocpMayer ocp xf tf
    objLagrange = evaluateQuadratures (ocpLagrange ocp) collTraj outputs h taus times

    -- timestep
    h = tf / fromIntegral n
    n = ctN collTraj

    -- initial time at each collocation stage
    t0s :: Vec n SXElement
    t0s = TV.mkVec' $ take n [h * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Vec deg SXElement)
    times = fmap (\t0 -> fmap (\tau -> t0 + tau*h) taus) t0s

    -- the collocation points
    taus :: forall b. Fractional b => Vec deg b
    taus = mkTaus deg

    deg = ctDeg collTraj

    -- coefficients for getting xdot by lagrange interpolating polynomials
    cijs :: Vec (Succ deg) (Vec (Succ deg) SXElement)
    cijs = lagrangeDerivCoeffs (0 TV.<| taus)

    -- initial point at each stage
    x0s :: Vec n (x SXElement)
    x0s = fmap (\(CollStage x0' _) -> x0') stages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (x SXElement)
    xfs = TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead stages)
    g = CollOcpConstraints
        { coStages = CollTrajConstraints stageConstraints
        , coPathC = TV.tvzipWith3 mkPathConstraints stages outputs times
        , coBc = ocpBc ocp x0 xf
        , coSbc = (ocpSc ocp) p0 pF
        }
    pF = TV.tvlast ps

    -- Q
    covInjections :: Vec n (Cov s Double)
    covInjections = fill (ocpSq ocp)

    ps :: Vec n (Cov s SXElement)
    ps = tvscanl' propogateCovariance p0 (TV.tvzip4 stages dcs interpolatedX covInjections)
      where
        tvscanl' f acc0 vs = devectorize $ V.scanl' f acc0 (vectorize vs)

    stageConstraints :: Vec n (CollStageConstraints x deg r sh SXElement)
    stageConstraints = vzipWith3 CollStageConstraints
                       dcs integratorMatchingConstraints covPathConstraints

    covPathConstraints :: Vec n (sh SXElement)
    covPathConstraints = TV.tvzipWith (ocpSh ocp) x0s (TV.tvshiftr p0 ps)

    integratorMatchingConstraints :: Vec n (x SXElement)
    integratorMatchingConstraints = vzipWith (vzipWith (-)) interpolatedX xfs

    dcs :: Vec n (CollDynConstraint deg r SXElement)
    outputs :: Vec n (Vec deg (o SXElement))
    interpolatedX :: Vec n (x SXElement)
    (dcs,outputs, interpolatedX) =
      TV.tvunzip3 $
      TV.tvzipWith (dynConstraints cijs (ocpDae ocp) taus h parm) times stages

    mkPathConstraints :: CollStage x z u deg SXElement -> Vec deg (o SXElement) ->
                         Vec deg SXElement -> Vec deg (h SXElement)
    mkPathConstraints (CollStage _ collPoints) = TV.tvzipWith3 mkPathC collPoints
      where
        mkPathC :: CollPoint x z u SXElement -> o SXElement -> SXElement -> h SXElement
        mkPathC (CollPoint x z u) = ocpPathC ocp x z u parm


makeCollNlp ::
  forall x z u p r o c h s sh sc deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize r, Vectorize o, Vectorize c, Vectorize s, Vectorize sh, Vectorize sc) =>
  OcpPhase x z u p r o c h s sh sc ->
  Nlp (CollTraj x z u p s n deg) None (CollOcpConstraints n deg x r c h sh sc)
makeCollNlp ocp = Nlp { nlpFG = getFg ocp
                      , nlpBX = bx
                      , nlpBG = bg
                      , nlpX0 = fmap (const 0) bx
                      , nlpP = None
                      }
  where
    bx = getBx ocp
    bg = getBg ocp

getBx :: (Dim n, Dim deg) => OcpPhase x z u p r o c h s sh sc ->
         CollTraj x z u p s n deg (Maybe Double, Maybe Double)
getBx ocp = ct
  where
    --ct :: CollTraj x z u p s n deg (Maybe Double, Maybe Double)
    ct = CollTraj tb sb pb (fill cs) xb

    --cs :: CollStage x z u deg (Maybe Double, Maybe Double)
    cs = CollStage xb (fill cp)

    --cp :: CollPoint x z u (Maybe Double, Maybe Double)
    cp = CollPoint xb zb ub

    xb = ocpXbnd ocp
    ub = ocpUbnd ocp
    zb = ocpZbnd ocp
    pb = ocpPbnd ocp
    tb = ocpTbnd ocp

    sb = ocpSbnd ocp

getBg :: forall x z u p r o c h s sh sc deg n .
  (Dim n, Dim deg, Vectorize x, Vectorize r, Vectorize c, Vectorize sh, Vectorize sc)
  => OcpPhase x z u p r o c h s sh sc ->
  CollOcpConstraints n deg x r c h sh sc (Maybe Double, Maybe Double)
getBg ocp =
  CollOcpConstraints
  { coStages = CollTrajConstraints (pure stageCon)
  , coPathC = fill (fill (ocpPathCBnds ocp))
  , coBc = fill (Just 0, Just 0)
  , coSbc = ocpScBnds ocp
  }
  where
    stageCon = CollStageConstraints (fill zz) (fill zz) (ocpShBnds ocp)
    zz = (Just 0, Just 0)

add :: (Vectorize x, Num a) => x a -> x a -> x a
add x y = devectorize $ V.zipWith (+) (vectorize x) (vectorize y)

dot :: forall x deg a. (Dim deg, Vectorize x, Num a) => Vec deg a -> Vec deg (x a) -> x a
dot cks xs = F.foldl' add (fill 0) $ TV.unSeq elemwise
  where
    elemwise :: Vec deg (x a)
    elemwise = TV.tvzipWith (\(Id c) x -> c *^ x) (fmap Id cks) xs

evaluateQuadratures ::
  forall x z u p o s n deg a .
  (Dim deg, Dim n, Fractional a) =>
  (x a -> z a -> u a -> p a -> o a -> a -> a) ->
  CollTraj x z u p s n deg a -> Vec n (Vec deg (o a)) -> a ->
  (forall b. Fractional b => Vec deg b) -> Vec n (Vec deg a) -> a
evaluateQuadratures f (CollTraj _ _ p stages _) outputs h taus times =
  (h *) $ V.sum $ TV.unVec $ TV.tvzipWith3 oneStage stages outputs times
  where
    oneStage :: CollStage x z u deg a -> Vec deg (o a) -> Vec deg a -> a
    oneStage (CollStage _ stage) output stageTimes = qnext
      where
        qdots :: Vec deg a
        qdots = TV.tvzipWith3 (\(CollPoint x z u) o t -> f x z u p o t) stage output stageTimes

        qs = cijInvFr !* qdots

        Id qnext = interpolate taus (Id 0) (fmap Id qs)

    cijs' :: Vec (Succ deg) (Vec (Succ deg) Double)
    cijs' = lagrangeDerivCoeffs (0 TV.<| taus)

    cijs :: Vec deg (Vec deg Double)
    cijs = TV.tvtail $ fmap TV.tvtail cijs'

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs

    cijInv' :: Mat.Matrix Double
    cijInv' = LA.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg a)
    cijInvFr = fmap (fmap realToFrac) cijInv

interpolateXDots' :: (Dim deg, Vectorize x, Num a) => Vec deg (Vec deg a) -> Vec deg (x a) -> Vec deg (x a)
interpolateXDots' cjks xs = fmap (`dot` xs) cjks

interpolateXDots ::
  (Dim deg, Vectorize x, Num a) =>
  Vec (Succ deg) (Vec (Succ deg) a)
  -> Vec (Succ deg) (x a)
  -> Vec deg (x a)
interpolateXDots cjks xs = TV.tvtail $ interpolateXDots' cjks xs

-- return dynamics constraints, outputs, and interpolated state
dynConstraints ::
  forall x z u p r o deg . (Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize r)
  => Vec (Succ deg) (Vec (Succ deg) SXElement) -> Dae x z u p r o SXElement ->
  Vec deg SXElement -> SXElement -> p SXElement -> Vec deg SXElement ->
  CollStage x z u deg SXElement ->
  (CollDynConstraint deg r SXElement, Vec deg (o SXElement), x SXElement)
dynConstraints cijs dae taus h p stageTimes (CollStage x0 cps) =
  (CollDynConstraint dynConstrs, outputs, xnext)
  where
    -- interpolated final state
    xnext :: x SXElement
    xnext = interpolate taus x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (r SXElement)
    outputs :: Vec deg (o SXElement)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots cps stageTimes

    applyDae :: x SXElement -> CollPoint x z u SXElement -> SXElement -> (r SXElement, o SXElement)
    applyDae x' (CollPoint x z u) = dae x' x z u p

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (x SXElement)
    xdots = fmap (fmap (/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (x SXElement)
    xs = fmap getX cps



propogateCovariance ::
  forall x z u s r deg . (Dim deg, Vectorize x, Vectorize z, Vectorize u, Vectorize r, Vectorize s)
  => Cov s SXElement
  -> (CollStage x z u deg SXElement, CollDynConstraint deg r SXElement, x SXElement, Cov s Double)
  -> Cov s SXElement
propogateCovariance p0' (CollStage x0 cps, CollDynConstraint dynConstrs, interpolatedX, q0') =
  if covN p0' == 0 then p0' else -- supress casadi zero size matrix error
--    (x0',
--     (ssize1 x0', ssize2 x0'),
--     (ssize1 df_dx0, ssize2 df_dx0),
--     (ssize1 df_dz, ssize2 df_dz),
--     (ssize1 dz_dx0, ssize2 dz_dx0),
--     (ssize1 dx1_dx0, ssize2 dx1_dx0),
--     (ssize1 p1, ssize2 p1)
--    )
--    `traceShow`
    fromMatrix p1
  where
    q0 = toMatrix (fmap realToFrac q0')
    p0 = toMatrix p0'

    f' :: SX
    f' = svector (V.concatMap vectorize (vectorize dynConstrs))

    z' :: SX
    z' = svector (V.concatMap (\(CollPoint x z _) -> vectorize (Tuple x z)) (vectorize cps))

    x0' :: SX
    x0' = svector (vectorize x0)

    df_dx0 = sjacobian f' x0'
    df_dz = sjacobian f' z'
    dz_dx0 = - (ssolve df_dz df_dx0)

    g' = svector (vectorize interpolatedX)
    dg_dx0 = sjacobian g' x0'
    dg_dz = sjacobian g' z'
    dx1_dx0 = dg_dx0 + smm dg_dz dz_dx0

    p1 = dx1_dx0 `smm` p0 `smm` (strans dx1_dx0) + q0




interpolate :: (Dim deg, Fractional a, Vectorize x) => Vec deg a -> x a -> Vec deg (x a) -> x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : F.toList taus) 1) [0..deg]
    deg = TV.tvlength taus


-- | make an initial guess
makeGuess ::
  forall x z u p s deg n .
  (Dim n, Dim deg)
  => Double -> (Double -> x Double) -> (Double -> z Double) -> (Double -> u Double)
  -> Cov s Double -> p Double
  -> CollTraj x z u p s n deg Double
makeGuess tf guessX guessZ guessU cov' parm = CollTraj tf cov' parm guesses (guessX tf)
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

    mkGuess' :: (Double, Vec deg Double) -> CollStage x z u deg Double
    mkGuess' (t,ts) = CollStage (guessX t) $
                      fmap (\t' -> CollPoint (guessX t') (guessZ t') (guessU t')) ts

    guesses :: Vec n (CollStage x z u deg Double)
    guesses = fmap mkGuess' times

    -- the collocation points
    --taus :: forall b. Fractional b => Vec deg b
    taus = mkTaus deg

    deg = vlength (undefined :: Vec deg ())
