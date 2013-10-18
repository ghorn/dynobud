{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module TypeVecs
       ( Vec ( unVec )
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

import Data.TypeLevel.Num.Ops ( Add, Sub, Succ )
import Data.TypeLevel.Num.Sets

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import qualified Data.Vector as V

-- length-indexed vectors using phantom types
newtype Vec s a = Vec' {unVec :: V.Vector a} deriving (Eq, Functor, Foldable, Traversable)

--infixr 5 <|
--infixl 5 |>
--(<|) :: Succ n np1 => a -> Vec n a -> Vec np1 a
--(<|) x (Vec xs) = Vec (V.cons x xs)
--
--(|>) :: Succ n np1 => Vec n a -> a -> Vec np1 a
--(|>) (Vec xs) x = Vec (V.snoc xs x)

-- create a Vec with a runtime check
unsafeVec :: Nat s => V.Vector a -> Vec s a
unsafeVec xs = case Vec' xs of
  ret ->
    if vlength ret == V.length xs
    then ret
    else error "unsafeVec: dynamic/static length mismatch"

mkVec :: Nat s => V.Vector a -> Vec s a
--mkVec = Vec'
mkVec = unsafeVec -- lets just run the check every time for now

vlength :: Nat s => Vec s a -> Int
vlength = toInt . (undefined `asLengthOf`)

vlengthT :: Vec s a -> s
vlengthT = (undefined `asLengthOf`)

asLengthOf :: s -> Vec s a -> s
asLengthOf x _ = x

-- split into two
--vsplit :: (Nat i, i :<=: s, Sub s i si) => i -> Vec s a -> (Vec i a, Vec si a)
vsplit :: (Nat i, Nat si, Sub s i si) => i -> Vec s a -> (Vec i a, Vec si a)
vsplit i v = (mkVec x, mkVec y)
  where
    (x,y) = V.splitAt (toInt i) (unVec v)

vhead :: Pos s => Vec s a -> a
vhead = V.head . unVec

vzipWith :: Nat s => (a -> b -> c) -> Vec s a -> Vec s b -> Vec s c
vzipWith f x y = mkVec (V.zipWith f (unVec x) (unVec y))

vinit :: (Succ sm1 s) => Vec s a -> Vec sm1 a
vinit = mkVec . V.init . unVec

vtail :: (Succ sm1 s) => Vec s a -> Vec sm1 a
vtail = mkVec . V.tail . unVec

vlast :: Pos s => Vec s a -> a
vlast = V.last . unVec

vreplicate :: Nat n => n -> a -> Vec n a
vreplicate n = mkVec . (V.replicate (toInt n))

-- concatenate two vectors
infixr 5 <++>
(<++>) :: (Nat s1, Nat s2, Add s1 s2 s3) => Vec s1 a -> Vec s2 a -> Vec s3 a
(<++>) x y = mkVec $ (unVec x) V.++ (unVec y)

instance Show a => Show (Vec s a) where
  showsPrec _ = showV . V.toList . unVec
    where
      showV []      = showString "<>"
      showV (x:xs)  = showChar '<' . shows x . showl xs
        where
          showl []      = showChar '>'
          showl (y:ys)  = showChar ',' . shows y . showl ys
