{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module TypeVecs
       ( Vec(..)
       , vlength
       , vlengthT
       , (<++>)
--       , (|>)
--       , (<|)
       , unsafeVec
       , vsplit
       , vhead
       , vzipWith
       , vinit
       , vtail
       , vlast
       , vreplicate
       )
       where

import Data.TypeLevel.Num.Ops
import Data.TypeLevel.Num.Sets

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import qualified Data.Vector as V

-- length-indexed vectors using phantom types
newtype Vec s a = Vec {unVec :: V.Vector a} deriving (Eq, Functor, Foldable, Traversable)

--infixr 5 <|
--infixl 5 |>
--(<|) :: Succ n np1 => a -> Vec n a -> Vec np1 a
--(<|) x (Vec xs) = Vec (V.cons x xs)
--
--(|>) :: Succ n np1 => Vec n a -> a -> Vec np1 a
--(|>) (Vec xs) x = Vec (V.snoc xs x)

-- construct one with a runtime check
unsafeVec :: Nat s => s -> V.Vector a -> Vec s a
unsafeVec l xs
  | toInt l /= V.length xs =
    error "unsafeVec: dynamic/static lenght mismatch"
  | otherwise = Vec xs

vlength :: Nat s => Vec s a -> Int
vlength = toInt . (undefined `asLengthOf`)
  where
    asLengthOf :: s -> Vec s a -> s
    asLengthOf x _ = x

vlengthT :: Vec s a -> s
vlengthT = (undefined `asLengthOf`)
  where
    asLengthOf :: s -> Vec s a -> s
    asLengthOf x _ = x

-- split into two
--vsplit :: (Nat i, i :<=: s, Sub s i si) => i -> Vec s a -> (Vec i a, Vec si a)
vsplit :: (Nat i, Sub s i si) => i -> Vec s a -> (Vec i a, Vec si a)
vsplit i v = (Vec x, Vec y)
  where
    (x,y) = V.splitAt (toInt i) (unVec v)

vhead :: Pos s => Vec s a -> a
vhead (Vec vs) = V.head vs

vzipWith :: (a -> b -> c) -> Vec s a -> Vec s b -> Vec s c
vzipWith f (Vec x) (Vec y) = Vec (V.zipWith f x y)

vinit :: (Succ sm1 s) => Vec s a -> Vec sm1 a
vinit (Vec vs) = Vec (V.init vs)

vtail :: (Succ sm1 s) => Vec s a -> Vec sm1 a
vtail (Vec vs) = Vec (V.tail vs)

vlast :: Pos s => Vec s a -> a
vlast (Vec vs) = V.last vs

vreplicate :: Nat n => n -> a -> Vec n a
vreplicate n x = Vec $ V.replicate (toInt n) x

-- concatenate two vectors
infixr 5 <++>
(<++>) :: (Nat s1, Nat s2, Add s1 s2 s3) => Vec s1 a -> Vec s2 a -> Vec s3 a
(<++>) (Vec x) (Vec y) = Vec (x V.++ y)
