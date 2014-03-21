{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Dyno.Casadi.V
       ( V, View(..), Symbolic(..), Vec(..), VV(..), S(..), sym
       ) where

import GHC.Generics hiding ( S )

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import qualified Data.Vector as V

import Dyno.Casadi.SX ( SX, ssymV )
import Dyno.Casadi.MX ( MX, symV )

import Dyno.Casadi.Viewable ( Viewable(..) )
import Dyno.Vectorize

data V (a :: *) (f :: * -> *) = V { unV :: a } -- deriving Generic
instance Show a => Show (V a f) where
  showsPrec p (V x) = showsPrec p x


class Symbolic a where
  vsym :: String -> Int -> IO a
instance Symbolic SX where vsym = ssymV
instance Symbolic MX where vsym = symV

-- | creating symbolics
sym :: forall f a . (View f, Viewable a, Symbolic a) => String -> IO (V a f)
sym name = ret
  where
    ret :: IO (V a f)
    ret = fmap V (vsym name n)
    n = size (Proxy :: Proxy f)

-- | vectors in View
data Vec n f a = Vec { unVec :: V.Vector (V a f) } deriving ( Show )
instance (Dim n, View f) => View (Vec n f) where
  cat = V . vveccat . fmap unV . unVec
  split = Vec . fmap V . flip vvecsplit ks . unV
    where
      ks = V.fromList (take n [0,m..])
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)
  size = const (n * m)
    where
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)


-- | MX view into a scalar, for convenience
data S a = S a deriving ( Generic, Generic1, Show, Functor, Foldable, Traversable )
-- just use a Vectorize instance
instance Vectorize S where
instance View S where
  cat = V . unV . cat . VV
  size = const (size (Proxy :: Proxy (VV S)))
  split = unVV . split . V . unV


-- | Use something in class Vectorize as a View
newtype VV f a = VV { unVV :: f a }
instance Vectorize f => View (VV f) where
  cat (VV xs) = V (vveccat (vectorize xs))
  size = const $ vlength (empty :: f ())
  split (V x) = VV (devectorize (vvecsplit x ks))
    where
      ks = V.fromList (take n [0..])
      n = size (Proxy :: Proxy (VV f))

-- | Type-save "views" into vectors, which can access subvectors
--   without splitting then concatenating everything.
class View f where
  cat :: Viewable a => f a -> V a f
  default cat :: (GCat (Rep (f a)) a, Generic (f a), Viewable a) => f a -> V a f
  cat = V . gcat vveccat . from

  size :: Proxy f -> Int
  default size :: (GSize (Rep (f ())), Generic (f ())) => Proxy f -> Int
  size = gsize . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  split :: Viewable a => V a f -> f a
  default split :: (GSplit (Rep (f a)) a, Generic (f a), Viewable a) => V a f -> f a
  split = to . gsplit vvecsplit . unV

------------------------------------ SIZE ------------------------------
class GSize f where
  gsize :: Proxy (f p) -> Int

instance (GSize f, GSize g) => GSize (f :*: g) where
  gsize pxy = gsize px + gsize py
    where
      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
instance GSize f => GSize (M1 i d f) where
  gsize = gsize . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy _ = Proxy

instance View f => GSize (Rec0 (V a f)) where
  gsize = size . reproxy
    where
      reproxy :: Proxy (Rec0 (V a f) p) -> Proxy f
      reproxy _ = Proxy

----------------------------- CAT -------------------------------
class GCat f a where
  gcat :: (V.Vector a -> a) -> f p -> a

-- concatenate fields recursively
instance (GCat f a, GCat g a) => GCat (f :*: g) a where
  gcat veccat (x :*: y) = veccat (V.fromList [x',y'])
    where
      x' = gcat veccat x
      y' = gcat veccat y
-- discard the metadata
instance GCat f a => GCat (M1 i d f) a where
  gcat veccat = gcat veccat . unM1

-- any field should just hold a view, no recursion here
instance GCat (Rec0 (V a f)) a where
  gcat _ (K1 (V x)) = x


------------------------------- SPLIT -------------------------------
class GSplit f a where
  gsplit :: (a -> V.Vector Int -> V.Vector a) -> a -> f p

-- split fields recursively
instance (GSplit f a, GSplit g a, GSize f, GSize g) => GSplit (f :*: g) a where
  gsplit vecsplit mxy = ret
    where
      ret = x :*: y
      
      nx = gsize (proxy x)
      --ny = gsize (proxy y)

      proxy :: h -> Proxy h
      proxy = const Proxy

      mx :: a
      my :: a
      (mx,my) = case V.toList (vecsplit mxy (V.fromList [0,nx])) of
        [mx',my'] -> (mx',my')
--        [mx',my'] -> if nx == size1 mx' && ny == size1 my'
--                          then (mx',my')
--                          else error "size mismatch in vertsplit"
        bad -> error $ "vecsplit error, wanted 2 but got: " ++ show (length bad)
      
      x = gsplit vecsplit mx -- :: (V f)
      y = gsplit vecsplit my -- :: g p -- (V xy)
-- discard the metadata
instance GSplit f a => GSplit (M1 i d f) a where
  gsplit vecsplit = M1 . gsplit vecsplit
-- any field should just hold a view, no recursion here
instance GSplit (Rec0 (V a f)) a where
  gsplit _ x = K1 (V x)
