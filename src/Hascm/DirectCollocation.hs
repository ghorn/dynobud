{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}

module Hascm.DirectCollocation
       ( CollTraj(..)
       , CollStage(..)
       , CollPoint(..)
       , CollTrajConstraints(..)
       , CollOcpConstraints(..)
       , makeCollNlp
       , PlotPoints(..)
       , PlotPointsL(..)
       , plotPoints
       , plotPointLists
       , toPlotTree
       , reifyCollPoint
       , reifyCollStage
       , reifyCollTraj
       ) where

import Data.Tree ( Tree(..) )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Packed.Matrix as Mat
import GHC.Generics
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Vector
import Linear.Matrix
import Linear.V
import Data.Proxy

import Plotter ( AccessorTree(..), Lookup(..), accessors )
import JacobiRoots

import Hascm.Vectorize
import qualified Hascm.TypeVecs as TV
import Hascm.TypeVecs ( Vec, Succ )
import Hascm.LagrangePolynomials

import Hascm.Nlp
import Hascm.Ocp
--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)

reifyCollPoint
  :: forall a r .
     CollPoint V.Vector V.Vector V.Vector a ->
     (forall x z u . (Vectorize x, Vectorize z, Vectorize u) => CollPoint x z u a -> r) ->
     r
reifyCollPoint (CollPoint x z u) f =
  TV.reifyDim (V.length x) $ \(Proxy :: Proxy nx) ->
  TV.reifyDim (V.length z) $ \(Proxy :: Proxy nz) ->
  TV.reifyDim (V.length u) $ \(Proxy :: Proxy nu) ->
  f (CollPoint (devectorize x :: Vec nx a) (devectorize z :: Vec nz a) (devectorize u :: Vec nu a))

reifyCollStage
  :: forall a r .
     CollStage V.Vector V.Vector V.Vector Int a ->
     (forall x z u deg . (Vectorize x, Vectorize z, Vectorize u, Dim deg) => CollStage x z u deg a -> r) ->
     r
reifyCollStage (CollStage x0 points') f =
  TV.reifyDim (V.length x1) $ \(Proxy :: Proxy nx) ->
  TV.reifyDim (V.length z1) $ \(Proxy :: Proxy nz) ->
  TV.reifyDim (V.length u1) $ \(Proxy :: Proxy nu) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f (CollStage
     (devectorize x0 :: Vec nx a)
     (devectorize (V.map (\(CollPoint x z u) -> (CollPoint (devectorize x) (devectorize z) (devectorize u))) points) :: Vec deg (CollPoint (Vec nx) (Vec nz) (Vec nu) a))
    )
  where
    points = TV.unVec points'
    deg = V.length points
    CollPoint x1 z1 u1 = V.head points

reifyCollTraj
  :: forall a r.
     CollTraj V.Vector V.Vector V.Vector V.Vector Int Int a
  -> (forall x z u p n deg . (Vectorize x,Vectorize z,Vectorize u,Vectorize p, Dim n, Dim deg) =>
      CollTraj x z u p n deg a -> r)
  -> r
reifyCollTraj (CollTraj endTime params stages' xf) f =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f (CollTraj
     endTime
     (devectorize params :: Vec np a)
     (devectorize
      (V.map (\(CollStage x0 points'') ->
               CollStage
               (devectorize x0)
               (devectorize (V.map (\(CollPoint x z u) ->
                                     CollPoint
                                     (devectorize x)
                                     (devectorize z)
                                     (devectorize u)
                                   ) (TV.unVec points'')
                            )
               )
             ) stages) :: Vec n (CollStage (Vec nx) (Vec nz) (Vec nu) deg a))
     (devectorize xf :: Vec nx a)
    )
  where
    nx = V.length x1
    nz = V.length z1
    nu = V.length u1
    np = V.length params
    n = V.length stages
    deg = V.length points

    stages = TV.unVec stages' :: V.Vector (CollStage V.Vector V.Vector V.Vector Int a)
    points = TV.unVec points' :: V.Vector (CollPoint V.Vector V.Vector V.Vector a)

    CollStage _ points' = V.head stages :: CollStage V.Vector V.Vector V.Vector Int a
    CollPoint x1 z1 u1 = V.head points :: CollPoint V.Vector V.Vector V.Vector a


data CollPoint x z u a = CollPoint (x a) (z a) (u a) deriving (Eq, Functor, Generic1)
data CollStage x z u deg a = CollStage (x a) (Vec deg (CollPoint x z u a)) deriving (Eq, Functor, Generic1)
data CollTraj x z u p n deg a = CollTraj a (p a) (Vec n (CollStage x z u deg a)) (x a) deriving (Eq, Functor, Generic1) -- endtime, params, coll stages, xf

instance (Vectorize x, Vectorize z, Vectorize u) => Vectorize (CollPoint x z u)
instance (Vectorize x, Vectorize z, Vectorize u, Dim deg) => Vectorize (CollStage x z u deg)
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg) =>
         Vectorize (CollTraj x z u p n deg)

data CollDynConstraint deg r a =
  CollDynConstraint (Vec deg (r a))
  deriving (Eq, Functor, Generic1)
instance (Vectorize r, Dim deg) =>
         Vectorize (CollDynConstraint deg r)

data CollStageConstraints x deg r a =
  CollStageConstraints (CollDynConstraint deg r a) (x a)
  deriving (Eq, Functor, Generic1)
instance (Vectorize x, Vectorize r, Dim deg) =>
         Vectorize (CollStageConstraints x deg r)

data CollTrajConstraints n x deg r a =
  CollTrajConstraints (Vec n (CollStageConstraints x deg r a))
  deriving (Eq, Functor, Generic1)
instance (Vectorize x, Vectorize r, Dim n, Dim deg) =>
         Vectorize (CollTrajConstraints n x deg r)

data CollOcpConstraints n deg x r c h a =
  CollOcpConstraints
  { coDynamics :: CollTrajConstraints n x deg r a
  , coPathC :: Vec n (Vec deg (h a))
  , coBc :: c a
  } deriving (Eq, Functor, Generic1)
instance (Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c, Vectorize h) =>
         Vectorize (CollOcpConstraints n deg x r c h)

ctN :: forall x z u p n deg a . Dim n => CollTraj x z u p n deg a -> Int
ctN _ = reflectDim (Proxy :: Proxy n)

ctDeg :: forall x z u p n deg a . Dim deg => CollTraj x z u p n deg a -> Int
ctDeg _ = reflectDim (Proxy :: Proxy deg)

mkTaus :: Fractional a => Int -> Vec deg a
mkTaus deg = case shiftedLegendreRoots deg of
  Just taus -> TV.mkVec $ V.map (fromRational . toRational) taus
  Nothing -> error "makeTaus: too high degree"

getFg :: forall z x u p r c h a n deg .
         (Dim deg, Vectorize x, Floating a, Dim n) =>
         OcpPhase x z u p r c h -> NlpInputs (CollTraj x z u p n deg) None a ->
         NlpFun (CollOcpConstraints n deg x r c h) a
getFg ocp (NlpInputs collTraj@(CollTraj tf p stages xf) _) = NlpFun obj g
  where
    obj = objLagrange + objMayer

    objMayer = ocpMayer ocp xf tf
    objLagrange = evaluateQuadratures (ocpLagrange ocp) collTraj h taus times

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
    xfs = TV.mkSeq $ TV.unSeq $ TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead stages)
    g = CollOcpConstraints
        { coDynamics =
             CollTrajConstraints $
             TV.tvzipWith3 (dynConstraints cijs (ocpDae ocp) taus h p) times stages xfs
        , coPathC = TV.tvzipWith mkPathConstraints stages times
        , coBc = (ocpBc ocp) x0 xf
        }

    mkPathConstraints :: CollStage x z u deg a -> Vec deg a -> Vec deg (h a)
    mkPathConstraints (CollStage _ collPoints) stageTimes =
      TV.tvzipWith mkPathC collPoints stageTimes

    mkPathC :: CollPoint x z u a -> a -> h a
    mkPathC (CollPoint x z u) t = ocpPathC ocp x z u p t


makeCollNlp ::
  forall x z u p r c h deg n .
  (Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z, Vectorize r, Vectorize c) =>
  OcpPhase x z u p r c h ->
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

getBx :: (Dim n, Dim deg) => OcpPhase x z u p r c h -> CollTraj x z u p n deg (Maybe Double, Maybe Double)
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
  => OcpPhase x z u p r c h ->
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
  forall x z u p n deg a .
  (Dim deg, Dim n, Fractional a) =>
  (x a -> z a -> u a -> p a -> a -> a) ->
  CollTraj x z u p n deg a -> a -> (forall b. Fractional b => Vec deg b) -> Vec n (Vec deg a) -> a
evaluateQuadratures f (CollTraj _ p stages _) h taus times =
  (h *) $ V.sum $ TV.unVec $ TV.tvzipWith oneStage stages times
  where
    oneStage :: CollStage x z u deg a -> Vec deg a -> a
    oneStage (CollStage _ stage) stageTimes = qnext
      where
        qdots :: Vec deg a
        qdots = TV.tvzipWith (\(CollPoint x z u) t -> f x z u p t) stage stageTimes

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
  forall x z u p r a deg . (Dim deg, Fractional a, Vectorize x)
  => Vec (Succ deg) (Vec (Succ deg) a) -> Dae x z u p r a -> Vec deg a -> a -> p a -> Vec deg a ->
  CollStage x z u deg a -> x a -> CollStageConstraints x deg r a
dynConstraints cijs dae taus h p stageTimes (CollStage x0 cps) xnext =
  CollStageConstraints (CollDynConstraint dynConstrs) integratorC
  where
    -- integration matching constraint
    integratorC = vzipWith (-) xnext' xnext

    -- interpolated final state
    xnext' :: x a
    xnext' = interpolate taus x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (r a)
    dynConstrs = TV.tvzipWith3 applyDae xdots cps stageTimes

    applyDae :: x a -> CollPoint x z u a -> a -> r a
    applyDae x' (CollPoint x z u) t = dae x' x z u p t

    xdots :: Vec deg (x a)
    xdots = fmap (fmap (/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (x a)
    xs = fmap getX cps

getX :: CollPoint x z u a -> x a
getX (CollPoint x _ _) = x

interpolate :: (Dim deg, Fractional a, Vectorize x) => Vec deg a -> x a -> Vec deg (x a) -> x a
interpolate taus x0 xs = dot (TV.mkVec' xis) (x0 TV.<| xs)
  where
    xis = map (lagrangeXis (0 : (F.toList taus)) 1) [0..deg]
    deg = TV.tvlength taus

data PlotPoints n deg x z u a =
  PlotPoints (Vec n ((a, x a), Vec deg (a, x a, z a, u a), (a, x a))) (a, x a)

data PlotPointsL x z u a =
  PlotPointsL [[(a, x a)]] [[(a, z a)]] [[(a, u a)]]

plotPoints ::
  forall x z u p n deg a .
  (Dim n, Dim deg, Fractional a, Vectorize x)
  => CollTraj x z u p n deg a ->
  PlotPoints n deg x z u a
plotPoints ct@(CollTraj tf _ stages xf) = PlotPoints ret (tf', xf)
  where
    (tf', ret) = T.mapAccumL f 0 stages
    nStages = TV.tvlength stages
    h = tf / (fromIntegral nStages)
    taus = mkTaus (ctDeg ct)

    f :: a -> CollStage x z u deg a -> (a, ((a, x a), Vec deg (a, x a, z a, u a), (a, x a)))
    f t0 (CollStage x0 xs) = (tnext, stage)
      where
        tnext = t0 + h
        stage = ( (t0, x0)
                , TV.tvzipWith (\(CollPoint x z u) tau -> (t0 + h*tau, x, z, u)) xs taus
                , (tnext, interpolate taus x0 (fmap getX xs))
                )

plotPointLists :: forall n deg x z u a .
                  (RealFrac a, Read a, Vectorize x, Vectorize z, Vectorize u) =>
                  PlotPoints n deg x z u a ->
                  PlotPointsL x z u a
plotPointLists (PlotPoints vec txf) =
  PlotPointsL (xs' ++ [[txf]]) zs' us'
  where
    (xs', zs', us') = unzip3 $ map f (F.toList vec)

    f :: ((a, x a), Vec deg (a, x a, z a, u a), (a, x a)) -> ([(a, x a)], [(a, z a)], [(a, u a)])
    f (x0, stage, xf) = (x0 : xs ++ [xf], zs, us)
      where
        (xs,zs,us) = unzip3 $ map (\(t, x, z, u) -> ((t,x), (t,z), (t,u))) (F.toList stage)

toPlotTree :: forall x z u .
              (Lookup (x Double), Lookup (z Double), Lookup (u Double),
               Vectorize x, Vectorize z, Vectorize u) =>
              Tree (String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
toPlotTree = Node ("trajectory", "trajectory", Nothing) [xtree, ztree, utree]
  where
    xtree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
    xtree = toGetterTree (\(PlotPointsL x _ _) -> x) "differential states" $ accessors (fill 0)

    ztree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
    ztree = toGetterTree (\(PlotPointsL _ z _) -> z) "algebraic variables" $ accessors (fill 0)

    utree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
    utree = toGetterTree (\(PlotPointsL _ _ u) -> u) "controls" $ accessors (fill 0)

toGetterTree ::
  (b -> [[(Double, x Double)]]) -> String -> AccessorTree (x Double)
  -> Tree (String, String, Maybe (b -> [[(Double,Double)]]))
toGetterTree toXs name (Getter f) = Node (name, name, Just g) []
  where
    g = map (map (\(t,x) -> (t,f x))) . toXs
toGetterTree toXs name (Data (_,name') children) =
  Node (name, name', Nothing) $ map (\(n,t) -> toGetterTree toXs n t) children
