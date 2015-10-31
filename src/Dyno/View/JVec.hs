{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.View.JVec
       ( JVec(..)
       , jreplicate, jreplicate'
       , reifyJVec
       ) where

import qualified Data.Sequence as Seq
import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Casadi.Viewable ( Viewable(..) )

import Dyno.View.Unsafe ( mkM, unM )

import Dyno.TypeVecs ( Vec, unVec, reifyVector )
import Dyno.View.View ( View(..), J )
import Dyno.Vectorize ( devectorize )

-- | vectors in View
newtype JVec (n :: k) f a = JVec { unJVec :: Vec n (J f a) } deriving ( Show )
instance (Dim n, View f) => View (JVec n f) where
  cat = mkM . vveccat . fmap unM . unVec . unJVec
  split = JVec . fmap mkM . devectorize . flip vvertsplit ks . unM
    where
      ks = V.fromList (take (n+1) [0,m..])
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)
  size = const (n * m)
    where
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)
  sizes = const . Seq.iterateN n (+m) . (+ m)
    where
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)

jreplicate' :: forall a n f . (Dim n, View f) => J f a -> JVec n f a
jreplicate' el =  ret
  where
    ret = JVec (devectorize (V.replicate nvec el))
    nvec = reflectDim (Proxy :: Proxy n)

jreplicate :: forall a n f . (Dim n, View f, Viewable a) => J f a -> J (JVec n f) a
jreplicate = cat . jreplicate'


reifyJVec :: forall a f r . Vector (J f a) -> (forall (n :: *). Dim n => JVec n f a -> r) -> r
reifyJVec v f = reifyVector v $ \(v' :: Vec n (J f a)) -> f (JVec v' :: JVec n f a)
{-# INLINE reifyJVec #-}

