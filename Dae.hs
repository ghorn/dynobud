{-# OPTIONS_GHC -Wall #-}

module Dae ( Dae
           , ExplicitOde
           , ImplicitOde
           , SemiExplicitDae
           , Integrator
           , forwardEuler
           , rk4
           ) where

import Vectorize

--type Dae x z u p r a = x a -> x a -> z a -> u a -> p a -> a -> r a
type Dae x z u p r a = x a -> z a -> u a -> p a -> r a

type ExplicitOde x u p a = Dae x None u p x a
type ImplicitOde x u p r a = Dae x None u p r a
type SemiExplicitDae x z u p r a = Dae x z u p (Tuple x r) a

type Integrator x z u p r a = Dae x z u p r a -> a -> x a -> u a -> p a -> x a

-- x0 + dx - xf == 0
forwardEuler :: (Vectorize x, Num a) => Integrator x None u p x a
forwardEuler f ts x0 u p = vzipWith (+) x0 deltaX
  where
    deltaX = fmap (*ts) xdot
    xdot = f x0 None u p

rk4 :: (Vectorize x, Fractional a) => Integrator x None u p x a
rk4 f h x0 u p = vzipWith (+) x0 deltaX
  where
    deltaX = mul (h/6) (k1  `add` (mul 2 k2) `add` (mul 2 k3) `add` k4)
    k1 = f x0 None u p
    k2 = f (x0 `add` mul (h/2) k1) None u p
    k3 = f (x0 `add` mul (h/2) k2) None u p
    k4 = f (x0 `add` mul h     k3) None u p

    mul y v = fmap (y*) v
    add = vzipWith (+)
