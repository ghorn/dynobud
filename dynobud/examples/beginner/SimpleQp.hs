{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic, Generic1 )

import Dyno.Vectorize
import Dyno.NlpUtils
import Dyno.Solvers

-- minimize:   x0**2 + 2*x1**2
-- subject to:    1 <= 2*x0 + x1 <= inf
--             -inf <=   x0      <= 10
--              -20 <=        x1 <= inf

-- design variable
data X a = X a a deriving (Functor, Generic, Generic1, Show)
instance Applicative X where
  pure = vpure
  (<*>) = vapply
instance Vectorize X

-- constraint
data G a = G a deriving (Functor, Generic, Generic1, Show)
instance Applicative G where
  pure = vpure
  (<*>) = vapply
instance Vectorize G

-- objective/constraint function
fg :: Num a => X a -> (a, G a)
fg (X x0 x1) = (f, g)
  where
    f = x0*x0 + 2*x1*x1
    g = G (2*x0 + x1)

-- design variable bounds
bx :: X (Maybe Double, Maybe Double)
bx = X (Nothing, Just 10) (Just (-20), Nothing)

-- constraint bounds
bg :: G (Maybe Double, Maybe Double)
bg = G (Just 1, Nothing)

-- initial guess
xguess :: X Double
xguess = X 2 3

main :: IO ()
main = do
  opt <- solveNlpV ipoptSolver fg bx bg xguess Nothing
  print opt
