{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}

module Ocp ( OcpPhase(..), ExplicitOde, ExplEulerMsDvs(..), ExplEulerMsConstraints(..), makeNlp ) where

import qualified Data.Foldable as F
import qualified Data.Vector as V

import TypeVecs
import qualified TypeVecs as TV
import Vectorize
import Nlp
import TypeNats

type ExplicitOde x u a = (x a -> u a -> x a)
--data ImplicitOde x u a = ImplicitOde (x a -> u a -> Vec nn a)

data OcpPhase x u bc pc a =
  OcpPhase { ocpMeyer :: x a -> a
           , ocpLagrange :: x a -> u a -> a
           , ocpDae :: ExplicitOde x u a
           , ocpBc :: x a -> x a -> bc a
           , ocpPathC :: x a -> u a -> pc a
           , ocpPathCBnds :: pc (Maybe Double, Maybe Double)
           , ocpXbnd :: x (Maybe Double, Maybe Double)
           , ocpUbnd :: u (Maybe Double, Maybe Double)
           }

data ExplEulerMsDvs x u nu a =
  ExplEulerMsDvs
    { eeX :: Vec (Succ nu) (x a)
    , eeU :: Vec nu (u a)
    } deriving (Functor, Generic1, Show)
instance (Vectorize x, Vectorize u, NaturalT nu, NaturalT (Succ nu)) => Vectorize (ExplEulerMsDvs x u nu)

data ExplEulerMsConstraints x nu bc pc a =
  ExplEulerMsConstraints
  { ecBc :: bc a
  , ecPathc :: Vec nu (pc a)
  , ecDynamics :: Vec nu (x a)
  } deriving (Functor, Generic1, Show)

instance (Vectorize x, Vectorize pc, Vectorize bc, NaturalT nu) =>
         Vectorize (ExplEulerMsConstraints x nu bc pc)

getDvBnds :: forall x u nbc pc nu . (NaturalT nu, NaturalT (Succ nu)) =>
  OcpPhase x u nbc pc Double -> -- Double here supresses warning in makeNlp, Double is never used
  ExplEulerMsDvs x u nu (Maybe Double, Maybe Double)
getDvBnds ocp = ExplEulerMsDvs x u
  where
    x = fill (ocpXbnd ocp)
    u = fill (ocpUbnd ocp)

getGBnds :: (Vectorize x, Vectorize bc, NaturalT nu) =>
            OcpPhase x u bc pc Double ->  -- Double here supresses warning in makeNlp, Double is never used
            ExplEulerMsConstraints x nu bc pc (Maybe Double, Maybe Double)
getGBnds ocp =
  ExplEulerMsConstraints
  { ecBc = fill (Just 0, Just 0)
  , ecPathc = fill (ocpPathCBnds ocp)
  , ecDynamics = fill (fill (Just 0, Just 0))
  }

getFg :: (Floating a, Vectorize x, NaturalT nu, PositiveT (Succ nu)) =>
         OcpPhase x u bc pc a -> ExplEulerMsDvs x u nu a ->
         NlpFun (ExplEulerMsConstraints x nu bc pc) a
getFg ocp (ExplEulerMsDvs xs us) = NlpFun objective constraints
  where
    initxs = TV.tvinit xs
    constraints =
      ExplEulerMsConstraints
      { ecBc = (ocpBc ocp) (TV.tvhead xs) (TV.tvlast xs)
      , ecPathc = TV.tvzipWith (ocpPathC ocp) initxs us
      , ecDynamics = TV.tvzipWith (zipWith' (-)) x0s x1s
      }
      where
        x0s = TV.tvzipWith (ocpDae ocp) initxs us
        x1s = TV.tvtail xs

    zipWith' :: Vectorize f => (a -> b -> c) -> f a -> f b -> f c
    zipWith' f x y = devectorize (V.zipWith f (vectorize x) (vectorize y))

    objective =
      (ocpMeyer ocp) (TV.tvlast xs) +
      (ssum (TV.tvzipWith (ocpLagrange ocp) initxs us)) / fromIntegral (TV.tvlength us)

ssum :: Num a => Vec n a -> a
ssum = F.foldl' (+) 0

makeNlp :: (Vectorize x, PositiveT (Succ nu), NaturalT nu, NaturalT (Succ nu), Vectorize bc) =>
           (forall a. Floating a => OcpPhase x u bc pc a) ->
           Nlp (ExplEulerMsDvs x u nu) (ExplEulerMsConstraints x nu bc pc)
makeNlp ocp = Nlp (getFg ocp) (getDvBnds ocp) (getGBnds ocp)
