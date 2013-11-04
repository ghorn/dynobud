{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language ScopedTypeVariables #-}
--{-# Language TypeFamilies #-}

module DirectCollocation
       ( CollTraj(..)
       , CollStage(..)
       , CollPoint(..)
       , CollTrajConstraints(..)
       , makeCollNlp
       , solveCollNlp
       , PlotPoints(..)
       , plotPoints
       , plotPointLists
       ) where

import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import JacobiRoots

import Vectorize
import TypeVecs
import TypeNats
import LagrangePolynomials

import Nlp
import Ocp
--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)

data CollPoint x z u a = CollPoint (x a) (z a) (u a) deriving (Functor, Generic1)
data CollStage x z u deg a = CollStage (x a) (Vec deg (CollPoint x z u a)) deriving (Functor, Generic1)
data CollTraj x z u p n deg a = CollTraj a (p a) (Vec n (CollStage x z u deg a)) (x a) deriving (Functor, Generic1) -- endtime, params, coll stages, xf

instance (Vectorize x, Vectorize z, Vectorize u) => Vectorize (CollPoint x z u)
instance (Vectorize x, Vectorize z, Vectorize u, NaturalT deg) => Vectorize (CollStage x z u deg)
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p, NaturalT n, NaturalT deg) =>
         Vectorize (CollTraj x z u p n deg)

data CollDynConstraint deg r a =
  CollDynConstraint (Vec deg (r a))
  deriving (Functor, Generic1)
instance (Vectorize r, NaturalT deg) =>
         Vectorize (CollDynConstraint deg r)

data CollStageConstraints x deg r a =
  CollStageConstraints (CollDynConstraint deg r a) (x a)
  deriving (Functor, Generic1)
instance (Vectorize x, Vectorize r, NaturalT deg) =>
         Vectorize (CollStageConstraints x deg r)

data CollTrajConstraints n x deg r a =
  CollTrajConstraints (Vec n (CollStageConstraints x deg r a))
  deriving (Functor, Generic1)
instance (Vectorize x, Vectorize r, NaturalT n, NaturalT deg) =>
         Vectorize (CollTrajConstraints n x deg r)

data CollOcpConstraints n deg x r bc pc a =
  CollOcpConstraints
  { coDynamics :: CollTrajConstraints n x deg r a
  , coPathC :: Vec n (Vec deg (pc a))
  , coBc :: bc a
  } deriving (Functor, Generic1)
instance (Vectorize x, Vectorize r, NaturalT n, NaturalT deg, Vectorize bc, Vectorize pc) =>
         Vectorize (CollOcpConstraints n deg x r bc pc)

ctDegT :: CollTraj x z u p n deg a -> deg
ctDegT _ = undefined

ctNT :: CollTraj x z u p n deg a -> n
ctNT _ = undefined

ctN :: IntegerT n => CollTraj x z u p n deg a -> Int
ctN = fromIntegerT . ctNT

ctDeg :: IntegerT deg => CollTraj x z u p n deg a -> Int
ctDeg = fromIntegerT . ctDegT

mkTaus :: Fractional a => Int -> Vec deg a
mkTaus deg = case shiftedLegendreRoots deg of
  Just taus -> mkVec $ V.map (fromRational . toRational) taus
  Nothing -> error "makeTaus: too high degree"

getFg :: (PositiveT n, NaturalT deg, NaturalT (Succ deg), Vectorize x, Fractional a, NaturalT n, Num a) =>
         OcpPhase x z u p r bc pc a -> CollTraj x z u p n deg a ->
         NlpFun (CollOcpConstraints n deg x r bc pc) a
getFg ocp collTraj@(CollTraj _ p css xf) = NlpFun obj g
  where
    obj = ocpMeyer ocp xf

    x0 = (\(CollStage x0' _) -> x0') (tvhead css)
    g = CollOcpConstraints
        { coDynamics = collConstraints (ocpDae ocp) taus collTraj
        , coPathC = fmap (\(CollStage _ collPoints) -> fmap mkPathC collPoints) css
        , coBc = (ocpBc ocp) x0 xf
        }
    taus = mkTaus deg
    deg = ctDeg collTraj

    --mkPathC :: CollPoint x z u a -> pc a
    mkPathC (CollPoint x z u) = ocpPathC ocp x z u p


makeCollNlp ::
  (PositiveT n, Vectorize x, Vectorize r, NaturalT deg, NaturalT n, NaturalT (Succ deg), Vectorize bc) =>
  (forall a. Floating a => OcpPhase x z u p r bc pc a) ->
  Nlp (CollTraj x z u p n deg) (CollOcpConstraints n deg x r bc pc)
makeCollNlp ocp = Nlp (getFg ocp) (getBx ocp) (getBg ocp)

solveCollNlp ::
  (PositiveT n, Vectorize x, Vectorize r, NaturalT deg, NaturalT n, NaturalT (Succ deg), Vectorize bc, Vectorize pc, Vectorize p, Vectorize u, Vectorize z) =>
  (forall a. Floating a => OcpPhase x z u p r bc pc a) ->
  Maybe (CollTraj x z u p n deg Double -> IO Bool) ->
  IO (CollTraj x z u p n deg Double)
solveCollNlp ocp callback = solveNlp (makeCollNlp ocp) callback

getBx ::
  (NaturalT n, NaturalT deg)
  => OcpPhase x z u p r bc pc Double -> CollTraj x z u p n deg (Maybe Double, Maybe Double)
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
  (NaturalT n, NaturalT deg, Vectorize x, Vectorize r, Vectorize bc)
  => OcpPhase x z u p r bc pc Double ->
  CollOcpConstraints n deg x r bc pc (Maybe Double, Maybe Double)
getBg ocp =
  CollOcpConstraints
  { coDynamics = fill (Just 0, Just 0)
  , coPathC = fill (fill (ocpPathCBnds ocp))
  , coBc = fill (Just 0, Just 0)
  }

add :: (Vectorize x, Num a) => x a -> x a -> x a
add x y = devectorize $ V.zipWith (+) (vectorize x) (vectorize y)

dot :: forall x deg a. (NaturalT deg, Vectorize x, Num a) => Vec deg a -> Vec deg (x a) -> x a
dot cks xs = F.foldl' (add) (fill 0) $ unSeq elemwise
  where
    elemwise :: Vec deg (x a)
    elemwise = tvzipWith (\(Id c) x -> fmap (c*) x) (fmap Id cks) xs

collConstraints ::
  forall x z u p r a deg n .
  (NaturalT n, NaturalT deg, NaturalT (Succ deg), Vectorize x, Fractional a)
  => Dae x z u p r a -> Vec deg a ->
  CollTraj x z u p n deg a -> CollTrajConstraints n x deg r a
collConstraints dae taus collTraj@(CollTraj tf p stages xf) =
  CollTrajConstraints $ tvzipWith (dynConstraints dae taus h p) stages xfs
  where
    x0s :: Vec n (x a)
    x0s = fmap (\(CollStage x0 _) -> x0) stages

    xfs :: Vec n (x a)
    xfs = mkSeq $ unSeq $ tvtail x0s |> xf

    h = tf / (fromIntegral n)
    n = ctN collTraj

interpolateXDots :: (NaturalT deg, Vectorize x, Num a) => Vec deg (Vec deg a) -> Vec deg (x a) -> Vec deg (x a)
interpolateXDots cjks xs = fmap (`dot` xs) cjks

dynConstraints ::
  forall x z u p r a deg . (NaturalT deg, NaturalT (Succ deg), Fractional a, Vectorize x)
  => Dae x z u p r a -> Vec deg a -> a -> p a ->
  CollStage x z u deg a -> x a -> CollStageConstraints x deg r a
dynConstraints dae taus h p (CollStage x0 cps) xnext = CollStageConstraints dynC integratorC
  where
    dynC :: CollDynConstraint deg r a
    dynC = CollDynConstraint dynConstrs

    integratorC = vzipWith (-) xnext' xnext

    dynConstrs :: Vec deg (r a)
    dynConstrs = tvzipWith applyDae xdots cps

    applyDae :: x a -> CollPoint x z u a -> r a
    applyDae x' (CollPoint x z u) = dae x' x z u p

    xdots :: Vec deg (x a)
    xdots = mkSeq $ unSeq $ tvtail xdots'

    xdots' :: Vec (Succ deg) (x a)
    xdots' = fmap (fmap (/h)) $ interpolateXDots cijs (x0 <| xs)

    xs :: Vec deg (x a)
    xs = fmap getX cps

    cijs :: Vec (Succ deg) (Vec (Succ deg) a)
    cijs = lagrangeDerivCoeffs (0 <| taus)

    xnext' :: x a
    xnext' = interpolate taus x0 xs

getX :: CollPoint x z u a -> x a
getX (CollPoint x _ _) = x


interpolate :: (NaturalT deg, NaturalT (Succ deg), Fractional a, Vectorize x)
               => Vec deg a -> x a -> Vec deg (x a) -> x a
interpolate taus x0 xs = dot (mkVec' xis) (x0 <| xs)
  where
    xis = map (lagrangeXis (0 : (F.toList taus)) 1) [0..deg]
    deg = tvlength taus

data PlotPoints n deg x z u a =
  PlotPoints (Vec n ((a, x a), Vec deg (a, x a, z a, u a), (a, x a))) (a, x a)

plotPoints ::
  forall x z u p n deg a .
  (NaturalT n, NaturalT deg, NaturalT (Succ deg), Fractional a, Vectorize x)
  => CollTraj x z u p n deg a ->
  PlotPoints n deg x z u a
plotPoints ct@(CollTraj tf _ stages xf) = PlotPoints ret (tf', xf)
  where
    (tf', ret) = T.mapAccumL f 0 stages
    nStages = tvlength stages
    h = tf / (fromIntegral nStages)
    taus = mkTaus (ctDeg ct)

    f :: a -> CollStage x z u deg a -> (a, ((a, x a), Vec deg (a, x a, z a, u a), (a, x a)))
    f t0 (CollStage x0 xs) = (tnext, stage)
      where
        tnext = t0 + h
        stage = ( (t0, x0)
                , tvzipWith (\(CollPoint x z u) tau -> (t0 + h*tau, x, z, u)) xs taus
                , (tnext, interpolate taus x0 (fmap getX xs))
                )

plotPointLists :: forall n deg x z u a .
                  PlotPoints n deg x z u a ->
                  ([[(a, x a)]], [[(a, z a)]], [[(a, u a)]])
plotPointLists (PlotPoints vec txf) = (xs' ++ [[txf]], zs', us')
  where
    (xs', zs', us') = unzip3 $ map f (F.toList vec)

    f :: ((a, x a), Vec deg (a, x a, z a, u a), (a, x a)) -> ([(a, x a)], [(a, z a)], [(a, u a)])
    f (x0, stage, xf) = (x0 : xs ++ [xf], zs, us)
      where
        (xs,zs,us) = unzip3 $ map (\(t, x, z, u) -> ((t,x), (t,z), (t,u))) (F.toList stage)
