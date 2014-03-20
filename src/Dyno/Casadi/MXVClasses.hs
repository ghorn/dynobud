{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.Casadi.MXVClasses ( Cat(..), Split(..), Size(..), Sc(..) ) where

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import Data.Proxy
import GHC.Generics
import qualified Data.Vector as V
import Dyno.Casadi.MXVClassesInternal
import Dyno.Casadi.MXVData

data Sc a = Sc a deriving ( Generic, Show, Functor, Foldable, Traversable )

-------------------------------- SIZE ------------------------------
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

instance Size (f MXV) => GSize (Rec0 (MXV f)) where
  gsize = size . reproxy
    where
      reproxy :: Proxy (Rec0 (MXV f) p) -> Proxy (f MXV)
      reproxy _ = Proxy

instance GSize (Rec0 (MXV Sc)) where
  gsize = const 1

----------------------------- CAT -------------------------------
-- concatenate fields recursively
instance (GCat f, GCat g) => GCat (f :*: g) where
  gcat (x :*: y) = veccat (V.fromList [x',y'])
    where
      x' = gcat x
      y' = gcat y
-- discard the metadata
instance GCat f => GCat (M1 i d f) where
  gcat = gcat . unM1
-- any field should just hold a view, no recursion here
instance GCat (Rec0 (MXV f)) where
  gcat (K1 (MXV x)) = x


----------------------------- SPLIT -------------------------------
-- split fields recursively
instance (GSplit f, GSplit g, GSize f, GSize g) => GSplit (f :*: g) where
  gsplit mxy = ret
    where
      ret = x :*: y
      
      nx = gsize (proxy x)
      ny = gsize (proxy y)

      proxy :: h -> Proxy h
      proxy = const Proxy

      mx :: MX
      my :: MX
      (mx,my) = case vertsplit' mxy [0,nx] of
        [mx',my'] -> if nx == size1 mx' && ny == size1 my'
                          then (mx',my')
                          else error "size mismatch in vertsplit"
        bad -> error $ "vecsplit error, wanted 2 but got: " ++ show (length bad)
      
      x = gsplit mx -- :: (MXV f)
      y = gsplit my -- :: g p -- (MXV xy)
-- discard the metadata
instance GSplit f => GSplit (M1 i d f) where
  gsplit = M1 . gsplit
-- any field should just hold a view, no recursion here
instance GSplit (Rec0 (MXV f)) where
  gsplit x = K1 (MXV x)



vertsplit' :: MX -> [Int] -> [MX]
vertsplit' x ks = V.toList (vertsplit x (V.fromList ks))
