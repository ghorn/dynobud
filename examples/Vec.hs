-- | How to use type-indexed Vectors

{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic1 )

import qualified Data.Foldable as F
import qualified Data.Vector as V

import Dyno.Vectorize
import Dyno.TypeVecs
import Dyno.Nats

data Params a = Params a a deriving (Functor, Generic1, Show)
data X n a = X (Vec n (Params a)) a deriving (Functor, Generic1, Show)

instance Vectorize Params
instance Dim n => Vectorize (X n)

-- some random function
obj :: forall n a . (Num a, Dim n) => X n a -> a
obj (X vec' b) = b*b + sum lol
  where
    lol :: [a]
    lol = map (\(Params x y) -> x*x + y*y) vec

    vec :: [Params a]
    vec = F.toList vec'

-- you don't know the length at compile time
unknownLength :: (Num a, Show a) => V.Vector (Params a)
unknownLength = V.fromList [Params 1 2, Params 3 4, Params 5 6, Params 7 8]

-- you do know the length at compile time
knownLength :: (Num a, Show a) => Vec D4 (Params a)
knownLength = mkVec unknownLength

-- do something on type-safe vec data
doSomething :: (Dim n, Num a) => Vec n (Params a) -> a
doSomething vec = obj (X vec 5)

-- apply the type-safe operation on a vector of unknown length
doSomethingAtRuntime :: Num a => V.Vector (Params a) -> a
doSomethingAtRuntime vec = reifyVector vec doSomething

main :: IO ()
main = do
  print (unknownLength :: V.Vector (Params Double))
  print (knownLength :: Vec D4 (Params Double))
  print (doSomething knownLength :: Double)
  print (doSomethingAtRuntime unknownLength :: Double)
