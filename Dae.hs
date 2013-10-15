{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language DeriveFunctor #-}

module Dae where

import qualified Data.Vector as V
import Data.TypeLevel.Num.Ops ( Succ )
import Data.TypeLevel.Num.Sets ( Nat )

import TypeVecs ( Vec(..) )
import qualified TypeVecs as TV
import Vectorize

type ExplicitOde x u a = (x a -> u a -> x a)
--data ImplicitOde x u a = ImplicitOde (x a -> u a -> Vec nn a)

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
    } deriving Functor

data ExplEulerMsConstraints x n nbc npc a =
  ExplEulerMsConstraints
  { ecBc :: Vec nbc a
  , ecPathc :: Vec n (Vec npc a)
  , ecDynamics :: Vec n (x a)
  } deriving Functor

getDvBnds ::
  Succ nu nx =>
  OcpPhase x u nbc npc a ->
  ExplEulerMsDvs x u nx nu (Maybe Double, Maybe Double)
getDvBnds ocp = ExplEulerMsDvs x u
  where
    x = fill (ocpXbnd ocp)
    u = fill (ocpUbnd ocp)
  
getGBnds :: (Vectorize x nx, Nat n, Nat nbc) =>
            OcpPhase x u nbc npc a ->
            ExplEulerMsConstraints x n nbc npc (Maybe Double, Maybe Double)
getGBnds ocp =
  ExplEulerMsConstraints
  { ecBc = fill (Just 0, Just 0)
  , ecPathc = fill (ocpPathCBnds ocp)
  , ecDynamics = fill (fill (Just 0, Just 0))
  }

getFg :: (Succ nu nx, Fractional a, Vectorize x nx', Vectorize u nu') =>
         OcpPhase x u nbc npc a -> ExplEulerMsDvs x u nx nu a -> 
         ( (a, ExplEulerMsConstraints x nu nbc npc a) )
getFg ocp (ExplEulerMsDvs xs us) = (objective, constraints)
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
    
    zipWith' :: Vectorize f n => (a -> b -> c) -> f a -> f b -> f c
    zipWith' f x y = devectorize (TV.vzipWith f (vectorize x) (vectorize y))
    
    objective =
      (ocpMeyer ocp) (TV.vlast xs) +
      (V.sum (unVec (TV.vzipWith (ocpLagrange ocp) initxs us))) / fromIntegral (TV.vlength us)


makeNlp :: (Fractional a, Vectorize u nu, Vectorize x nx, Nat nu, Succ nu nx, Nat nbc) =>
           OcpPhase x u nbc npc a ->
           Nlp (ExplEulerMsDvs x u nx nu) (ExplEulerMsConstraints x nu nbc npc) a
makeNlp ocp = Nlp (getFg ocp) (getDvBnds ocp) (getGBnds ocp)

data Nlp dv g a =
  Nlp { nlpFG :: dv a -> (a, g a)
      , nlpBX :: dv (Maybe Double, Maybe Double)
      , nlpBG :: g (Maybe Double, Maybe Double)
      }
