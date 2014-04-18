{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyno.TypeVecs
       ( Vec
       , Succ
       , unSeq
       , mkSeq
       , mkUnit
       , unVec
       , mkVec
       , mkVec'
       , unsafeVec
       , tvlength
       , (|>)
       , (<|)
       , tvtranspose
       , tvzip
       , tvzip3
       , tvzip4
       , tvzipWith
       , tvzipWith3
       , tvzipWith4
       , tvunzip
       , tvunzip3
       , tvunzip4
       , tvunzip5
       , tvhead
       , tvtail
       , tvlast
       , tvshiftl
       , tvshiftr
       , reifyVector
       , reifyDim
       , Dim
       )
       where

import Control.Applicative
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Serialize ( Serialize )
import Linear.Vector
import Linear.V ( Dim(..) )
import Data.Proxy
import Data.Reflection as R
import GHC.Generics ( Generic )

import Dyno.Vectorize

-- length-indexed vectors using phantom types
newtype Vec n a = MkVec {unSeq :: S.Seq a} deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Monad)
instance Serialize a => Serialize (Vec n a)

data Succ n
instance Dim n => Dim (Succ n) where
  reflectDim _ = 1 + reflectDim (Proxy :: Proxy n)

instance Dim n => Dim (Vec n a) where
  reflectDim _ = reflectDim (Proxy :: Proxy n)

instance Dim n => Applicative (Vec n) where
  pure x = ret
    where
      ret = MkVec $ S.replicate (tvlength ret) x
  MkVec xs <*> MkVec ys = MkVec $ S.zipWith id xs ys

instance Dim n => Additive (Vec n) where
  zero = pure 0
  MkVec xs ^+^ MkVec ys = MkVec (S.zipWith (+) xs ys)
  MkVec xs ^-^ MkVec ys = MkVec (S.zipWith (-) xs ys)

instance Dim n => Vectorize (Vec n) where
  vectorize = unVec
  devectorize = mkVec
  empty = pure ()

tvtranspose :: Vec n (Vec m a) -> Vec m (Vec n a)
tvtranspose vec = mkVec $ fmap mkVec $ T.sequence (unVec (fmap unVec vec))

unVec :: Vec n a -> V.Vector a
unVec = V.fromList . F.toList . unSeq

infixr 5 <|
infixl 5 |>
(<|) :: a -> Vec n a -> Vec (Succ n) a
(<|) x xs = MkVec $ x S.<| unSeq xs

(|>) :: Vec n a -> a -> Vec (Succ n) a
(|>) xs x = MkVec $ unSeq xs S.|> x

-- create a Vec with a runtime check
unsafeVec :: Dim n => V.Vector a -> Vec n a
unsafeVec = unsafeSeq . S.fromList . V.toList

unsafeSeq :: Dim n => S.Seq a -> Vec n a
unsafeSeq xs = case MkVec xs of
  ret -> let staticLen = tvlength ret
             dynLen = S.length xs
         in if staticLen == dynLen
            then ret
            else error $ "unsafeVec: static/dynamic length mismatch: " ++
                 "static: " ++ show staticLen ++ ", dynamic: " ++ show  dynLen

mkUnit :: Vec n a -> Vec () a
mkUnit (MkVec v) = MkVec v

mkVec :: V.Vector a -> Vec n a
mkVec = MkVec . S.fromList . V.toList

mkVec' :: Dim n => [a] -> Vec n a
mkVec' = MkVec . S.fromList

mkSeq :: S.Seq a -> Vec n a
mkSeq = MkVec

-- --mkVec :: (IntegerT n) => V.Vector a -> Vec n a
-- --mkVec = unsafeVec -- lets just run the check every time for now
--
-- --mkSeq :: (IntegerT n) => S.Seq a -> Vec n a
-- --mkSeq = unsafeSeq -- lets just run the check every time for now

tvlength :: forall n a. Dim n => Vec n a -> Int
tvlength _ = reflectDim (Proxy :: Proxy n)

tvzip :: Vec n a -> Vec n b -> Vec n (a,b)
tvzip x y = mkSeq (S.zip (unSeq x) (unSeq y))

tvzip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a,b,c)
tvzip3 x y z = mkSeq (S.zip3 (unSeq x) (unSeq y) (unSeq z))

tvzip4 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n (a,b,c,d)
tvzip4 x y z w = mkSeq (S.zip4 (unSeq x) (unSeq y) (unSeq z) (unSeq w))

tvzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
tvzipWith f x y = mkSeq (S.zipWith f (unSeq x) (unSeq y))

tvzipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
tvzipWith3 f x y z = mkSeq (S.zipWith3 f (unSeq x) (unSeq y) (unSeq z))

tvzipWith4 :: (a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
tvzipWith4 f x y z u = mkSeq (S.zipWith4 f (unSeq x) (unSeq y) (unSeq z) (unSeq u))

tvunzip :: Vec n (a,b) -> (Vec n a, Vec n b)
tvunzip v = (mkVec v1, mkVec v2)
  where
    (v1,v2) = V.unzip (unVec v)

tvunzip3 :: Vec n (a,b,c) -> (Vec n a, Vec n b, Vec n c)
tvunzip3 v = (mkVec v1, mkVec v2, mkVec v3)
  where
    (v1,v2,v3) = V.unzip3 (unVec v)

tvunzip4 :: Vec n (a,b,c,d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
tvunzip4 v = (mkVec v1, mkVec v2, mkVec v3, mkVec v4)
  where
    (v1,v2,v3,v4) = V.unzip4 (unVec v)

tvunzip5 :: Vec n (a,b,c,d,e) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
tvunzip5 v = (mkVec v1, mkVec v2, mkVec v3, mkVec v4, mkVec v5)
  where
    (v1,v2,v3,v4,v5) = V.unzip5 (unVec v)

tvhead :: Vec n a -> a
tvhead x = case S.viewl (unSeq x) of
  y S.:< _ -> y
  S.EmptyL -> error "vhead: empty"

tvtail :: Dim n => Vec (Succ n) a -> Vec n a
tvtail x = case S.viewl (unSeq x) of
  _ S.:< ys -> mkSeq ys
  S.EmptyL -> error "vtail: empty"

tvlast :: Vec n a -> a
tvlast x = case S.viewr (unSeq x) of
  _ S.:> y -> y
  S.EmptyR -> error "vlast: empty"

tvshiftl :: Dim n => Vec n a -> a -> Vec n a
tvshiftl xs x = case S.viewl (unSeq xs) of
  _ S.:< ys -> mkSeq (ys S.|> x)
  S.EmptyL -> error "tvshiftl: EmptyL"

tvshiftr :: Dim n => a -> Vec n a -> Vec n a
tvshiftr x xs = case S.viewr (unSeq xs) of
  ys S.:> _ -> mkSeq (x S.<| ys)
  S.EmptyR -> error "tvshiftr: EmptyR"

instance Show a => Show (Vec n a) where
  showsPrec _ = showV . F.toList . unSeq
    where
      showV []      = showString "<>"
      showV (x:xs)  = showChar '<' . shows x . showl xs
        where
          showl []      = showChar '>'
          showl (y:ys)  = showChar ',' . shows y . showl ys


data ReifiedDim (s :: *)

retagDim :: (Proxy s -> a) -> proxy (ReifiedDim s) -> a
retagDim f _ = f Proxy
{-# INLINE retagDim #-}

instance Reifies s Int => Dim (ReifiedDim s) where
  reflectDim = retagDim reflect
  {-# INLINE reflectDim #-}

reifyDim :: Int -> (forall (n :: *). Dim n => Proxy n -> r) -> r
reifyDim i f = R.reify i (go f) where
  go :: Reifies n Int => (Proxy (ReifiedDim n) -> a) -> proxy n -> a
  go g _ = g Proxy
{-# INLINE reifyDim #-}

reifyVector :: forall a r. V.Vector a -> (forall (n :: *). Dim n => Vec n a -> r) -> r
reifyVector v f = reifyDim (V.length v) $ \(Proxy :: Proxy n) -> f (MkVec (S.fromList (V.toList v)) :: Vec n a)
{-# INLINE reifyVector #-}
