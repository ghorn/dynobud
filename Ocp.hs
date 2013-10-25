{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Ocp where

import qualified Data.Vector as V
import Data.TypeLevel.Num.Ops ( Succ )
import Data.TypeLevel.Num.Sets ( Nat, Pos )
import qualified Data.Foldable as F
import GHC.Generics

import TypeVecs ( Vec )
import qualified TypeVecs as TV
import Vectorize
import Nlp

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

data ExplEulerMsDvs x u nx nu a =
  ExplEulerMsDvs
    { eeX :: Vec nx (x a)
    , eeU :: Vec nu (u a)
    } deriving (Functor, Generic1, Show)
instance (Vectorize x, Vectorize u, Nat nx, Nat nu) => Vectorize (ExplEulerMsDvs x u nx nu)

data ExplEulerMsConstraints x n bc pc a =
  ExplEulerMsConstraints
  { ecBc :: bc a
  , ecPathc :: Vec n (pc a)
  , ecDynamics :: Vec n (x a)
  } deriving (Functor, Generic1, Show)
instance (Vectorize x, Vectorize bc, Vectorize pc, Nat n) => Vectorize (ExplEulerMsConstraints x n bc pc)

getDvBnds :: forall x u nbc pc nx nu .
  (Succ nu nx) =>
  OcpPhase x u nbc pc Double -> -- Double here supresses warning in makeNlp, Double is never used
  ExplEulerMsDvs x u nx nu (Maybe Double, Maybe Double)
getDvBnds ocp = ExplEulerMsDvs x u
  where
    x = fill (ocpXbnd ocp)
    u = fill (ocpUbnd ocp)

getGBnds :: (Vectorize x, Vectorize bc, Nat n) =>
            OcpPhase x u bc pc Double ->  -- Double here supresses warning in makeNlp, Double is never used
            ExplEulerMsConstraints x n bc pc (Maybe Double, Maybe Double)
getGBnds ocp =
  ExplEulerMsConstraints
  { ecBc = fill (Just 0, Just 0)
  , ecPathc = fill (ocpPathCBnds ocp)
  , ecDynamics = fill (fill (Just 0, Just 0))
  }

getFg :: (Floating a, Succ nu nx, Vectorize x, Pos nu) =>
         OcpPhase x u bc pc a -> ExplEulerMsDvs x u nx nu a ->
         NlpFun (ExplEulerMsConstraints x nu bc pc) a
getFg ocp (ExplEulerMsDvs xs us) = NlpFun objective constraints
  where
    initxs = TV.vinit xs
    constraints =
      ExplEulerMsConstraints
      { ecBc = (ocpBc ocp) (TV.vhead xs) (TV.vlast xs)
      , ecPathc = TV.vzipWith (ocpPathC ocp) initxs us
      , ecDynamics = TV.vzipWith (zipWith' (-)) x0s x1s
      }
      where
        x0s = TV.vzipWith (ocpDae ocp) initxs us
        x1s = TV.vtail xs

    zipWith' :: Vectorize f => (a -> b -> c) -> f a -> f b -> f c
    zipWith' f x y = devectorize (V.zipWith f (vectorize x) (vectorize y))

    objective =
      (ocpMeyer ocp) (TV.vlast xs) +
      (ssum (TV.vzipWith (ocpLagrange ocp) initxs us)) / fromIntegral (TV.vlength us)

ssum :: Num a => Vec n a -> a
ssum = F.foldl' (+) 0

makeNlp :: (Vectorize x, Pos nu, Succ nu nx, Vectorize bc) =>
           (forall a. Floating a => OcpPhase x u bc pc a) ->
           Nlp (ExplEulerMsDvs x u nx nu) (ExplEulerMsConstraints x nu bc pc)
makeNlp ocp = Nlp (getFg ocp) (getDvBnds ocp) (getGBnds ocp)
