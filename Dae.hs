{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Dae where

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

-- instead of Vec nbc a, let user pass Vectorize type
data OcpPhase x u nbc npc a =
  OcpPhase { ocpMeyer :: x a -> a
           , ocpLagrange :: x a -> u a -> a
           , ocpDae :: ExplicitOde x u a
           , ocpBc :: x a -> x a -> Vec nbc a
           , ocpPathC :: x a -> u a -> Vec npc a
           , ocpPathCBnds :: Vec npc (Maybe Double, Maybe Double)
           , ocpXbnd :: x (Maybe Double, Maybe Double)
           , ocpUbnd :: u (Maybe Double, Maybe Double)
           }

data ExplEulerMsDvs x u nx nu a =
  ExplEulerMsDvs
    { eeX :: Vec nx (x a)
    , eeU :: Vec nu (u a)
    } deriving (Functor, Generic1, Show)
instance (Vectorize x, Vectorize u, Nat nx, Nat nu) => Vectorize (ExplEulerMsDvs x u nx nu)

data ExplEulerMsConstraints x n nbc npc a =
  ExplEulerMsConstraints
  { ecBc :: Vec nbc a
  , ecPathc :: Vec n (Vec npc a)
  , ecDynamics :: Vec n (x a)
  } deriving (Functor, Generic1, Show)
instance (Vectorize x, Nat n, Nat nbc, Nat npc) => Vectorize (ExplEulerMsConstraints x n nbc npc)

getDvBnds :: forall x u nbc npc nx nu .
  (Succ nu nx) =>
  OcpPhase x u nbc npc Double -> -- Double here supresses warning in makeNlp, Double is never used
  ExplEulerMsDvs x u nx nu (Maybe Double, Maybe Double)
getDvBnds ocp = ExplEulerMsDvs x u
  where
    x = fill (ocpXbnd ocp)
    u = fill (ocpUbnd ocp)
  
getGBnds :: (Vectorize x, Nat n, Nat nbc) =>
            OcpPhase x u nbc npc Double ->  -- Double here supresses warning in makeNlp, Double is never used
            ExplEulerMsConstraints x n nbc npc (Maybe Double, Maybe Double)
getGBnds ocp =
  ExplEulerMsConstraints
  { ecBc = fill (Just 0, Just 0)
  , ecPathc = fill (ocpPathCBnds ocp)
  , ecDynamics = fill (fill (Just 0, Just 0))
  }

getFg :: (Floating a, Succ nu nx, Vectorize x, Vectorize u, Pos nu) =>
         OcpPhase x u nbc npc a -> ExplEulerMsDvs x u nx nu a -> 
         NlpFun (ExplEulerMsConstraints x nu nbc npc) a
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

makeNlp :: (Vectorize u, Vectorize x, Pos nu, Succ nu nx, Nat nbc) =>
           (forall a. Floating a => OcpPhase x u nbc npc a) ->
           Nlp (ExplEulerMsDvs x u nx nu) (ExplEulerMsConstraints x nu nbc npc)
makeNlp ocp = Nlp (getFg ocp) (getDvBnds ocp) (getGBnds ocp)
