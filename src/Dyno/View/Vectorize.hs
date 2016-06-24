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

module Dyno.View.Vectorize
       ( Vectorize(..)
       , devectorize
       , (:.)(..), unO
       , None(..)
       , Id(..), unId
       , Tuple(..)
       , Triple(..)
       , Quad(..)
       , vapply
       , vzipWith
       , vzipWith3
       , vzipWith4
       , vdiag
       , vdiag'
       , vnames
       , vnames'
       , GVectorize(..)
       ) where

import GHC.Generics

import Accessors ( GATip, Lookup(..), accessors, flatten, flatten' )
import Control.Applicative
import Control.Compose ( (:.)(..), Id(..), unO, unId )
import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Binary ( Binary(..) )
import Data.Either ( partitionEithers )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Proxy ( Proxy(..) )
import qualified Linear
import SpatialMath ( Euler )
import SpatialMathT ( V3T, Rot )
import Text.Printf ( printf )
import Prelude -- BBP workaround



-- | a length-0 vectorizable type
data None a = None
            deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance Vectorize None
instance Applicative None where
  pure = const None
  (<*>) = const (const None)
instance Linear.Additive None where
instance Binary (None a)
instance FromJSON a => FromJSON (None a)
instance ToJSON a => ToJSON (None a)

instance Vectorize Id


-- | a length-2 vectorizable type
data Tuple f g a = Tuple { unFst :: f a, unSnd :: g a }
                 deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g) => Vectorize (Tuple f g)
instance (Applicative f, Applicative g) => Applicative (Tuple f g) where
  pure x = Tuple (pure x) (pure x)
  Tuple fx fy <*> Tuple x y = Tuple (fx <*> x) (fy <*> y)
instance (Vectorize f, Vectorize g, Applicative f, Applicative g) => Linear.Additive (Tuple f g) where
  zero = Tuple (fill 0) (fill 0)
instance (FromJSON a, FromJSON (f a), FromJSON (g a))
         => FromJSON (Tuple f g a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a))
         => ToJSON (Tuple f g a)
instance (Binary (f a), Binary (g a)) => Binary (Tuple f g a)


-- | a length-3 vectorizable type
data Triple f g h a = Triple { unFst3 :: f a, unSnd3 :: g a, unThd3 :: h a }
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g, Vectorize h) => Vectorize (Triple f g h)
instance (Applicative f, Applicative g, Applicative h) => Applicative (Triple f g h) where
  pure x = Triple (pure x) (pure x) (pure x)
  Triple fx fy fz <*> Triple x y z = Triple (fx <*> x) (fy <*> y) (fz <*> z)
instance (Vectorize f, Vectorize g, Vectorize h,
          Applicative f, Applicative g, Applicative h)
         => Linear.Additive (Triple f g h) where
  zero = Triple (fill 0) (fill 0) (fill 0)
instance (FromJSON a, FromJSON (f a), FromJSON (g a), FromJSON (h a))
         => FromJSON (Triple f g h a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a), ToJSON (h a))
         => ToJSON (Triple f g h a)
instance (Binary (f a), Binary (g a), Binary (h a)) => Binary (Triple f g h a)


-- | a length-4 vectorizable type
data Quad f g h i a = Quad { unFst4 :: f a, unSnd4 :: g a, unThd4 :: h a, unFth4 :: i a }
                    deriving (Eq, Ord, Generic, Generic1, Functor, F.Foldable, T.Traversable, Show)
instance (Vectorize f, Vectorize g, Vectorize h, Vectorize i) => Vectorize (Quad f g h i)
instance (Applicative f, Applicative g, Applicative h, Applicative i) => Applicative (Quad f g h i) where
  pure x = Quad (pure x) (pure x) (pure x) (pure x)
  Quad fx fy fz fw <*> Quad x y z w = Quad (fx <*> x) (fy <*> y) (fz <*> z) (fw <*> w)
instance (Vectorize f, Vectorize g, Vectorize h, Vectorize i,
          Applicative f, Applicative g, Applicative h, Applicative i)
         => Linear.Additive (Quad f g h i) where
  zero = Quad (fill 0) (fill 0) (fill 0) (fill 0)
instance (FromJSON a, FromJSON (f a), FromJSON (g a), FromJSON (h a), FromJSON (i a))
         => FromJSON (Quad f g h i a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a), ToJSON (h a), ToJSON (i a))
         => ToJSON (Quad f g h i a)
instance (Binary (f a), Binary (g a), Binary (h a), Binary (i a)) => Binary (Quad f g h i a)

instance (Vectorize g, Vectorize f) => Vectorize (g :. f)

instance Lookup (None a)
instance (Lookup (f a), Lookup (g a)) => Lookup (Tuple f g a)
instance (Lookup (f a), Lookup (g a), Lookup (h a)) => Lookup (Triple f g h a)
instance (Lookup (f a), Lookup (g a), Lookup (h a), Lookup (i a)) => Lookup (Quad f g h i a)
instance Vectorize Linear.V0
instance Vectorize Linear.V1
instance Vectorize Linear.V2
instance Vectorize Linear.V3
instance Vectorize Linear.V4
instance Vectorize Linear.Quaternion
instance Vectorize Euler
instance Vectorize (V3T f)
instance Vectorize g => Vectorize (Rot f1 f2 g)

-- | partial version of 'devectorize\'' which throws an error
-- if the vector length doesn' match the type length
devectorize :: Vectorize f => V.Vector a -> f a
devectorize x = case devectorize' x of
  Right y -> y
  Left msg -> error msg

-- | define Applicative in terms of Vectorize
vapply :: Vectorize f => f (a -> b) -> f a -> f b
vapply f x = devectorize (V.zipWith id (vectorize f) (vectorize x))

vzipWith :: Vectorize f => (a -> b -> c) -> f a -> f b -> f c
vzipWith f x y = devectorize $ V.zipWith f (vectorize x) (vectorize y)

vzipWith3 :: Vectorize f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
vzipWith3 f x y z = devectorize $ V.zipWith3 f (vectorize x) (vectorize y) (vectorize z)

vzipWith4 :: Vectorize f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
vzipWith4 f x y z w =
  devectorize $ V.zipWith4 f (vectorize x) (vectorize y) (vectorize z) (vectorize w)

-- | Make a diagonal "matrix" from a "vector".
-- Off-diagonal elements will be 0, thus the Num constraint.
vdiag :: forall f a . (Vectorize f, Num a) => f a -> f (f a)
vdiag = flip vdiag' 0

-- | Make a diagonal "matrix" from a "vector" with a given off-diagonal value.
vdiag' :: forall f a . Vectorize f => f a -> a -> f (f a)
vdiag' v0 offDiag =
  devectorize $ V.generate n (\k -> devectorize (V.generate n (\j -> gen j k)))
  where
    v = vectorize v0
    n = vlength (Proxy :: Proxy f)
    gen j k
      | j /= k = offDiag
      | otherwise = v V.! k

-- | fmap f == devectorize . (V.map f) . vectorize
class Functor f => Vectorize (f :: * -> *) where
  vectorize :: f a -> V.Vector a
  devectorize' :: V.Vector a -> Either String (f a)
  fill :: a -> f a
  vlength :: Proxy f -> Int

  default vectorize :: (Generic1 f, GVectorize (Rep1 f)) => f a -> V.Vector a
  vectorize f = gvectorize (from1 f)

  default devectorize' :: (Generic1 f, GVectorize (Rep1 f)) => V.Vector a -> Either String (f a)
  devectorize' f = fmap to1 (gdevectorize f)

  default fill :: (Generic1 f, GVectorize (Rep1 f)) => a -> f a
  fill = to1 . gfill

  default vlength :: (Generic1 f, GVectorize (Rep1 f)) => Proxy f -> Int
  vlength = const $ gvlength (Proxy :: Proxy (Rep1 f))


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

-- | fill a vectorizable thing with its field names
vnames :: forall f . (Vectorize f, Lookup (f ())) => f String
vnames = case mr of
  Left msg -> error $ "vnames devectorize error: " ++ msg
  Right r -> r
  where
    mr = devectorize' $ V.fromList $
         fmap fst (flatten accessors :: [(String, GATip (f ()))])

-- | fill a vectorizable thing with its field name heirarchy
vnames' :: forall f . (Vectorize f, Lookup (f ())) => f [String]
vnames' = case mr of
  Left msg -> error $ "vnames' devectorize error: " ++ msg
  Right r -> fmap (map (maybe "()" id)) r
  where
    mr = devectorize' $ V.fromList $
         fmap fst (flatten' accessors :: [([Maybe String], GATip (f ()))])
