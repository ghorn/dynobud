{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
-- these last nasty ones are for instance Vectorize f => Applicative/Additive/Metric/etc f
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

module Dyno.Vectorize
       ( Vectorize(..)
       , devectorize
       , None(..)
       , Id(..)
       , Tuple(..)
       , Triple(..)
       , vlength
       , vzipWith
       , vzipWith3
       , vzipWith4
       , GVectorize(..)
       ) where

import GHC.Generics

import Control.Applicative
import Data.Either ( partitionEithers )
import Data.Serialize ( Serialize )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Proxy ( Proxy(..) )
import qualified Linear
import Text.Printf ( printf )
import Prelude -- BBP workaround

import SpatialMath ( Euler )
import SpatialMathT ( V3T, Rot )

import Accessors ( Lookup )


-- | a length-0 vectorizable type
data None a = None
            deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance Vectorize None
instance Applicative None where
  pure = const None
  (<*>) = const (const None)
instance Linear.Additive None where
instance Serialize (None a)

-- | a length-1 vectorizable type
newtype Id a = Id { unId :: a }
             deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance Vectorize Id
instance Applicative Id where
  pure = Id
  Id fx <*> Id x = Id (fx x)
instance Linear.Additive Id where
instance Serialize a => Serialize (Id a)


-- | a length-2 vectorizable type
data Tuple f g a = Tuple (f a) (g a)
                 deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g) => Vectorize (Tuple f g)
instance (Applicative f, Applicative g) => Applicative (Tuple f g) where
  pure x = Tuple (pure x) (pure x)
  Tuple fx fy <*> Tuple x y = Tuple (fx <*> x) (fy <*> y)
instance (Vectorize f, Vectorize g, Applicative f, Applicative g) => Linear.Additive (Tuple f g) where
  zero = Tuple (fill 0) (fill 0)


-- | a length-3 vectorizable type
data Triple f g h a = Triple (f a) (g a) (h a)
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g, Vectorize h) => Vectorize (Triple f g h)
instance (Applicative f, Applicative g, Applicative h) => Applicative (Triple f g h) where
  pure x = Triple (pure x) (pure x) (pure x)
  Triple fx fy fz <*> Triple x y z = Triple (fx <*> x) (fy <*> y) (fz <*> z)
instance (Vectorize f, Vectorize g, Vectorize h,
          Applicative f, Applicative g, Applicative h)
         => Linear.Additive (Triple f g h) where
  zero = Triple (fill 0) (fill 0) (fill 0)


instance Lookup (None a)
instance (Lookup a, Generic a) => Lookup (Id a)
instance (Lookup (f a), Generic (f a),
          Lookup (g a), Generic (g a)) => Lookup (Tuple f g a)
instance (Lookup (f a), Generic (f a),
          Lookup (g a), Generic (g a),
          Lookup (h a), Generic (h a)) => Lookup (Triple f g h a)
instance Vectorize Linear.V0
instance Vectorize Linear.V1
instance Vectorize Linear.V2
instance Vectorize Linear.V3
instance Vectorize Linear.V4
instance Vectorize Linear.Quaternion
instance Vectorize Euler
instance Vectorize (V3T f)
instance Vectorize (Rot f1 f2)

-- | partial version of 'devectorize\'' which throws an error
-- if the vector length doesn' match the type length
devectorize :: Vectorize f => V.Vector a -> f a
devectorize x = case devectorize' x of
  Right y -> y
  Left msg -> error msg

vzipWith :: Vectorize f => (a -> b -> c) -> f a -> f b -> f c
vzipWith f x y = devectorize $ V.zipWith f (vectorize x) (vectorize y)

vzipWith3 :: Vectorize f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
vzipWith3 f x y z = devectorize $ V.zipWith3 f (vectorize x) (vectorize y) (vectorize z)

vzipWith4 :: Vectorize f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
vzipWith4 f x y z w =
  devectorize $ V.zipWith4 f (vectorize x) (vectorize y) (vectorize z) (vectorize w)

-- this could me more efficient as a class method, but this is safer
vlength :: Vectorize f => Proxy f -> Int
vlength = V.length . vectorize . (fill () `asFunctorOf`)
  where
    asFunctorOf :: f a -> Proxy f -> f a
    asFunctorOf x _ = x

-- | fmap f == devectorize . (V.map f) . vectorize
class Functor f => Vectorize (f :: * -> *) where
  vectorize :: f a -> V.Vector a
  devectorize' :: V.Vector a -> Either String (f a)
  fill :: a -> f a

  default vectorize :: (Generic1 f, GVectorize (Rep1 f)) => f a -> V.Vector a
  vectorize f = gvectorize (from1 f)

  default devectorize' :: (Generic1 f, GVectorize (Rep1 f)) => V.Vector a -> Either String (f a)
  devectorize' f = fmap to1 (gdevectorize f)

  default fill :: (Generic1 f, GVectorize (Rep1 f)) => a -> f a
  fill = to1 . gfill

-- undecidable, overlapping, orphan instances to get rid of boilerplate

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance Vectorize f => Applicative f where
#else
instance {-# OVERLAPPABLE #-} Vectorize f => Applicative f where
#endif
  pure = fill
  x0 <*> x1 = devectorize (V.zipWith id (vectorize x0) (vectorize x1))

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance Vectorize f => Linear.Additive f where
#else
instance {-# OVERLAPPABLE #-} Vectorize f => Linear.Additive f where
#endif
  zero = fill 0

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance Vectorize f => Linear.Metric f where
#else
instance {-# OVERLAPPABLE #-} Vectorize f => Linear.Metric f where
#endif
  dot x0 x1 = V.sum $ V.zipWith (*) (vectorize x0) (vectorize x1)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance (Vectorize f, Eq a) => Eq (f a) where
#else
instance {-# OVERLAPPABLE #-} (Vectorize f, Eq a) => Eq (f a) where
#endif
  x == y = (vectorize x) == (vectorize y)
  x /= y = (vectorize x) /= (vectorize y)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance (Vectorize f, Ord a) => Ord (f a) where
#else
instance {-# OVERLAPPABLE #-} (Vectorize f, Ord a) => Ord (f a) where
#endif
  compare x y = compare (vectorize x) (vectorize y)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance Vectorize f => F.Foldable f where
#else
instance {-# OVERLAPPABLE #-} Vectorize f => F.Foldable f where
#endif
  foldMap f x = F.foldMap f (vectorize x)
  foldr f acc0 x = F.foldr f acc0 (vectorize x)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 708
instance Vectorize f => T.Traversable f where
#else
instance {-# OVERLAPPABLE #-} Vectorize f => T.Traversable f where
#endif
  traverse f x = devectorize <$> T.traverse f (vectorize x)

class GVectorize (f :: * -> *) where
  gvectorize :: f a -> V.Vector a
  gdevectorize :: V.Vector a -> Either String (f a)
  gfill :: a -> f a
  gvlength :: Proxy f -> Int

-- product type (concatination)
instance (GVectorize f, GVectorize g) => GVectorize (f :*: g) where
  gvectorize (f :*: g) = gvectorize f V.++ gvectorize g
  gdevectorize v0s
    | V.length v0s < n0 =
      Left $ "gdevectorize (f :*: g): V.length v0s < vlength f0  (" ++
             show (V.length v0s) ++ " < " ++ show n0 ++ ")"
    | V.length v1 /= n1 =
      Left $ "gdevectorize (f :*: g): V.length v1 /= vlength f1  (" ++
             show (V.length v1) ++ " /= " ++ show n1 ++ ")"
    | otherwise = case (ef0, ef1) of
      (Left msg0, Left msg1) ->
        Left $ "gdevectorize (f :*: g): errored on both sides: {" ++ msg0 ++ ", " ++ msg1 ++ "}"
      (Left msg0, Right   _) ->
        Left $ "gdevectorize (f :*: g): errored on left side: " ++ msg0
      (Right   _, Left msg1) ->
        Left $ "gdevectorize (f :*: g): errored on right side: " ++ msg1
      (Right f0, Right f1) -> Right (f0 :*: f1)
    where
      ef0 = gdevectorize v0
      ef1 = gdevectorize v1

      n0 = gvlength (Proxy :: Proxy f)
      n1 = gvlength (Proxy :: Proxy g)

      (v0,v1) = V.splitAt n0 v0s

  gfill x = gfill x :*: gfill x
  gvlength = const (nf + ng)
    where
      nf = gvlength (Proxy :: Proxy f)
      ng = gvlength (Proxy :: Proxy g)

-- Metadata (constructor name, etc)
instance GVectorize f => GVectorize (M1 i c f) where
  gvectorize = gvectorize . unM1
  gdevectorize = fmap M1 . gdevectorize
  gfill = M1 . gfill
  gvlength = gvlength . proxy
    where
      proxy :: Proxy (M1 i c f) -> Proxy f
      proxy = const Proxy

-- singleton
instance GVectorize Par1 where
  gvectorize = V.singleton . unPar1
  gdevectorize v = case V.toList v of
    [] -> Left "gdevectorize Par1: got empty list"
    [x] -> Right (Par1 x)
    xs -> Left $ "gdevectorize Par1: got non-1 length: " ++ show (length xs)
  gfill = Par1
  gvlength = const 1

-- data with no fields
instance GVectorize U1 where
  gvectorize = const V.empty
  gdevectorize v
    | V.null v = Right U1
    | otherwise = Left $ "gdevectorize U1: got non-null vector, length: " ++ show (V.length v)
  gfill = const U1
  gvlength = const 0

-- Constants, additional parameters, and rank-1 recursion
instance Vectorize f => GVectorize (Rec1 f) where
  gvectorize = vectorize . unRec1
  gdevectorize = fmap Rec1 . devectorize'
  gfill = Rec1 . fill
  gvlength = vlength . proxy
    where
      proxy :: Proxy (Rec1 f) -> Proxy f
      proxy = const Proxy

-- composition
instance (Vectorize f, GVectorize g) => GVectorize (f :.: g) where
  gfill = Comp1 . devectorize'' . V.replicate k . gfill
    where
      devectorize'' x = case devectorize' x of
        Right y -> y
        Left msg -> error $ "gfill (f :.: g) devectorize error: " ++ msg
      k = vlength (Proxy :: Proxy f)

  gvectorize = V.concatMap gvectorize . vectorize . unComp1
  gdevectorize v = case partitionEithers (V.toList evs) of
    ([], vs) -> fmap Comp1 (devectorize' (V.fromList vs))
    (bad, good) -> Left $ printf "gdevectorize (f :.: g): got %d failures and %d successes"
                          (length bad) (length good)
    where
      kf = vlength (Proxy :: Proxy f)
      kg = gvlength (Proxy :: Proxy g)

      --evs :: V.Vector (Either String (g a))
      evs = fmap gdevectorize (splitsAt kg kf v {-:: Vec nf (Vec ng a)-} )

  gvlength = const (nf * ng)
    where
      nf = vlength (Proxy :: Proxy f)
      ng = gvlength (Proxy :: Proxy g)

-- break a vector jOuter vectors, each of length kInner
splitsAt' :: Int -> Int -> V.Vector a -> [V.Vector a]
splitsAt' 0 jOuter v
  | V.null v = replicate jOuter V.empty
  | otherwise = error $ "splitsAt 0 " ++ show jOuter ++ ": got non-zero vector"
splitsAt' kInner 0 v
  | V.null v = []
  | otherwise = error $ "splitsAt " ++ show kInner ++ " 0: leftover vector of length: " ++ show (V.length v)
splitsAt' kInner jOuter v
  | kv0 < kInner =
    error $ "splitsAt " ++ show kInner ++ " " ++ show jOuter ++ ": " ++ "ran out of vector input"
  | otherwise = v0 : splitsAt' kInner (jOuter - 1) v1
  where
    kv0 = V.length v0
    (v0,v1) = V.splitAt kInner v

-- break a vector jOuter vectors, each of length kInner
splitsAt :: Int -> Int -> V.Vector a -> V.Vector (V.Vector a)
splitsAt k j = V.fromList . splitsAt' k j
