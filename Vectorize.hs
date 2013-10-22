{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Vectorize ( Vectorize(..)
                 , GVectorize(..)
                 , vlength
                 , fill
                 ) where


import qualified Data.Vector as V
import GHC.Generics

fill :: Vectorize f => a -> f a
fill x = fmap (const x) empty

-- | fmap f == devectorize . (V.map f) . vectorize
class Functor f =>  Vectorize f where
  vectorize :: f a -> V.Vector a
  devectorize :: V.Vector a -> f a
  empty :: f ()

  default vectorize :: (Generic1 f, GVectorize (Rep1 f)) => f a -> V.Vector a
  vectorize f = gvectorize (from1 f)

  default devectorize :: (Generic1 f, GVectorize (Rep1 f)) => V.Vector a -> f a
  devectorize f = to1 (gdevectorize f)

  default empty :: (Generic1 f, GVectorize (Rep1 f)) => f ()
  empty = to1 gempty

class GVectorize f where
  gvectorize :: f a -> V.Vector a
  gdevectorize :: V.Vector a -> f a
  gempty :: f ()

vlength :: Vectorize f => f a -> Int
vlength = V.length . vectorize . (empty `asFunctorOf`)
  where
    asFunctorOf :: f a -> f b -> f a
    asFunctorOf x _ = x

gvlength :: GVectorize f => f a -> Int
gvlength = V.length . gvectorize . (gempty `asFunctorOf`)
  where
    asFunctorOf :: f a -> f b -> f a
    asFunctorOf x _ = x

instance (GVectorize f, GVectorize g) => GVectorize (f :*: g) where
  gvectorize (f :*: g) = gvectorize f V.++ gvectorize g
  gdevectorize v0s = f0 :*: f1
    where
      f0 = gdevectorize v0
      f1 = gdevectorize v1

      -- unsafe!!
      (v0,v1s) = V.splitAt (gvlength f0) v0s
      (v1,_)   = V.splitAt (gvlength f1) v1s
  gempty = gempty :*: gempty

-- Metadata (constructor name, etc)
instance GVectorize f => GVectorize (M1 i c f) where
  gvectorize = gvectorize . unM1
  gdevectorize = M1 . gdevectorize
  gempty = M1 gempty

instance GVectorize Par1 where
  gvectorize = V.singleton . unPar1
  -- unsafe!!
  gdevectorize = Par1 . V.head
  gempty = Par1 ()

instance GVectorize U1 where
  gvectorize = const V.empty
  gdevectorize = const U1
  gempty = U1

-- Constants, additional parameters, and rank-1 recursion
instance Vectorize f => GVectorize (Rec1 f) where
  gvectorize = vectorize . unRec1
  gdevectorize = Rec1 . devectorize
  gempty = Rec1 empty

instance (Vectorize f, GVectorize g) => GVectorize (f :.: g) where
  gempty = Comp1 ret
    where
      ret = devectorize $ V.replicate k gempty
      k = vlength ret
  gvectorize = V.concatMap gvectorize . (vectorize . unComp1)
  gdevectorize v = Comp1 (devectorize vs)
    where
      kg = gvlength (V.head vs)
      vs = V.map gdevectorize (splitsAtV kg v)

-- break a vector into a bunch of length k vectors
splitsAt :: Int -> V.Vector a -> [V.Vector a]
splitsAt k v
  | ks == 0 = [v0]
  | ks < k = error "splitsAt: uneven leftover vector"
  | otherwise = v0 : splitsAt k v1
  where
    ks = V.length v1
    (v0,v1) = V.splitAt k v

-- break a vector into a bunch of length k vectors
splitsAtV :: Int -> V.Vector a -> V.Vector (V.Vector a)
splitsAtV k = V.fromList . splitsAt k
