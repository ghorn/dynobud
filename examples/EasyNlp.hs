-- | Minimize the Rosenbrock function (plus a trivial constraint) using
-- the most basic NLP interface.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic1 )

import Dyno.View.Vectorize ( Vectorize, vpure, vapply )
import Dyno.Nlp ( Bounds )
import Dyno.NlpUtils ( solveNlpV )
import Dyno.Solvers ( Solver, ipoptSolver )

data X a = X a a deriving (Functor, Generic1, Show)
data G a = G a deriving (Functor, Generic1, Show)

instance Applicative X where {pure = vpure; (<*>) = vapply}
instance Applicative G where {pure = vpure; (<*>) = vapply}

instance Vectorize X
instance Vectorize G

x0 :: X Double
x0 = X (-8) (-8)

bx :: X Bounds
bx = X (Just (-21), Just 0.5)
       (Just (-2), Just 2)

bg :: G Bounds
bg = G (Just (-10), Just 10)

fg :: Floating a => X a -> (a, G a)
fg (X x y) = (f, g)
  where
    f = (1-x)**2 + 100*(y - x**2)**2
    g = G x

solver :: Solver
solver = ipoptSolver
--solver = snoptSolver

main :: IO ()
main = do
  opt <- solveNlpV solver fg bx bg x0 Nothing
  print opt
