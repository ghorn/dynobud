{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Hascm.DirectCollocation.Formulate
       ( makeCollNlp
       , mkTaus
       , interpolate
       ) where

import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Vector
import Linear.Matrix
import Linear.V

import JacobiRoots ( shiftedLegendreRoots )

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

getFg :: forall z x u p r o c h a n deg .
         (Dim deg, Vectorize x, Floating a, Dim n) =>
         OcpPhase x z u p r o c h -> NlpInputs (CollTraj x z u p n deg) None a ->
         NlpFun (CollOcpConstraints n deg x r c h) a
getFg ocp (NlpInputs collTraj@(CollTraj tf p stages xf) _) = NlpFun obj g
  where
    obj = objLagrange + objMayer

    objMayer = ocpMayer ocp xf tf
    objLagrange = evaluateQuadratures (ocpLagrange ocp) collTraj outputs h taus times

    -- timestep
    h = tf / (fromIntegral n)
    n = ctN collTraj

    -- initial time at each collocation stage
    t0s :: Vec n a
    t0s = TV.mkVec' $ take n [h*(fromIntegral k) | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Vec deg a)
    times = fmap (\t0 -> fmap (\tau -> t0 + tau*h) taus) t0s

    -- the collocation points
    taus :: forall b. Fractional b => Vec deg b
    taus = mkTaus deg

    deg = ctDeg collTraj

    -- coefficients for getting xdot by lagrange interpolating polynomials
    cijs :: Vec (Succ deg) (Vec (Succ deg) a)
    cijs = lagrangeDerivCoeffs (0 TV.<| taus)

    -- initial point at each stage
    x0s :: Vec n (x a)
    x0s = fmap (\(CollStage x0' _) -> x0') stages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (x a)
    xfs = TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead stages)
    g = CollOcpConstraints
        { coDynamics = CollTrajConstraints dcs
        , coPathC = TV.tvzipWith3 mkPathConstraints stages outputs times
        , coBc = (ocpBc ocp) x0 xf
        }
    dcs :: Vec n (CollStageConstraints x deg r a)
    outputs :: Vec n (Vec deg (o a))
    (dcs,outputs) =
      TV.tvunzip $
      TV.tvzipWith3 (dynConstraints cijs (ocpDae ocp) taus h p) times stages xfs

    mkPathConstraints :: CollStage x z u deg a -> Vec deg (o a) -> Vec deg a -> Vec deg (h a)
    mkPathConstraints (CollStage _ collPoints) outputs' stageTimes =
      TV.tvzipWith3 mkPathC collPoints outputs' stageTimes

    mkPathC :: CollPoint x z u a -> o a -> a -> h a
    mkPathC (CollPoint x z u) o t = ocpPathC ocp x z u p o t


makeCollNlp ::
  forall x z u p r o c h deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z,
   Vectorize r, Vectorize o, Vectorize c) =>
  OcpPhase x z u p r o c h ->
  Nlp (CollTraj x z u p n deg) None (CollOcpConstraints n deg x r c h)
makeCollNlp ocp = Nlp { nlpFG = getFg ocp
                      , nlpBX = bx
                      , nlpBG = bg
                      , nlpX0 = fmap (const 0) bx
                      , nlpP = None
                      }
  where
    bx = getBx ocp
    bg = getBg ocp

getBx :: (Dim n, Dim deg) => OcpPhase x z u p r o c h ->
         CollTraj x z u p n deg (Maybe Double, Maybe Double)
getBx ocp = ct
  where
    --ct :: CollTraj x z u p n deg (Maybe Double, Maybe Double)
    ct = CollTraj tb pb (fill cs) xb

    --cs :: CollStage x z u deg (Maybe Double, Maybe Double)
    cs = CollStage xb (fill cp)

    --cp :: CollPoint x z u (Maybe Double, Maybe Double)
    cp = CollPoint xb zb ub

    xb = ocpXbnd ocp
    ub = ocpUbnd ocp
    zb = ocpZbnd ocp
    pb = ocpPbnd ocp
    tb = ocpTbnd ocp

getBg ::
  (Dim n, Dim deg, Vectorize x, Vectorize r, Vectorize c)
  => OcpPhase x z u p r o c h ->
  CollOcpConstraints n deg x r c h (Maybe Double, Maybe Double)
getBg ocp =
  CollOcpConstraints
  { coDynamics = fill (Just 0, Just 0)
  , coPathC = fill (fill (ocpPathCBnds ocp))
  , coBc = fill (Just 0, Just 0)
  }

add :: (Vectorize x, Num a) => x a -> x a -> x a
add x y = devectorize $ V.zipWith (+) (vectorize x) (vectorize y)

dot :: forall x deg a. (Dim deg, Vectorize x, Num a) => Vec deg a -> Vec deg (x a) -> x a
dot cks xs = F.foldl' add (fill 0) $ TV.unSeq elemwise
  where
    elemwise :: Vec deg (x a)
    elemwise = TV.tvzipWith (\(Id c) x -> c *^ x) (fmap Id cks) xs

evaluateQuadratures ::
  forall x z u p o n deg a .
  (Dim deg, Dim n, Fractional a) =>
  (x a -> z a -> u a -> p a -> o a -> a -> a) ->
  CollTraj x z u p n deg a -> Vec n (Vec deg (o a)) -> a ->
  (forall b. Fractional b => Vec deg b) -> Vec n (Vec deg a) -> a
evaluateQuadratures f (CollTraj _ p stages _) outputs h taus times =
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

dynConstraints ::
  forall x z u p r o a deg . (Dim deg, Fractional a, Vectorize x)
  => Vec (Succ deg) (Vec (Succ deg) a) -> Dae x z u p r o a -> Vec deg a -> a -> p a -> Vec deg a ->
  CollStage x z u deg a -> x a -> (CollStageConstraints x deg r a, Vec deg (o a))
dynConstraints cijs dae taus h p stageTimes (CollStage x0 cps) xnext =
  (CollStageConstraints (CollDynConstraint dynConstrs) integratorC, outputs)
  where
    -- integration matching constraint
    integratorC :: x a
    integratorC = vzipWith (-) xnext' xnext

    -- interpolated final state
    xnext' :: x a
    xnext' = interpolate taus x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (r a)
    outputs :: Vec deg (o a)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots cps stageTimes

    applyDae :: x a -> CollPoint x z u a -> a -> (r a, o a)
    applyDae x' (CollPoint x z u) t = dae x' x z u p t

    xdots :: Vec deg (x a)
    xdots = fmap (fmap (/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (x a)
    xs = fmap getX cps

interpolate :: (Dim deg, Fractional a, Vectorize x) => Vec deg a -> x a -> Vec deg (x a) -> x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : (F.toList taus)) 1) [0..deg]
    deg = TV.tvlength taus

