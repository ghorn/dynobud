{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Dyno.View.JVec
       ( JVec(..)
       , jreplicate, jreplicate'
       , reifyJVec
       ) where

import GHC.TypeLits ( KnownNat, natVal )

import qualified Data.Sequence as Seq
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Casadi.Viewable ( Viewable(..) )

import Dyno.View.Unsafe ( mkM, unM )

import Dyno.TypeVecs ( Vec, unVec, reifyVector )
import Dyno.View.Vectorize ( devectorize )
import Dyno.View.View ( View(..), J )

-- | vectors in View
newtype JVec n f a = JVec { unJVec :: Vec n (J f a) } deriving ( Show )
instance (KnownNat n, View f) => View (JVec n f) where
  cat = mkM . vvertcat . fmap unM . unVec . unJVec
  split = JVec . fmap mkM . devectorize . flip vvertsplit ks . unM
    where
      ks = V.fromList (take (n+1) [0,m..])
      n = fromIntegral (natVal (Proxy :: Proxy n))
      m = size (Proxy :: Proxy f)
  size = const (n * m)
    where
      n = fromIntegral (natVal (Proxy :: Proxy n))
      m = size (Proxy :: Proxy f)
  sizes = const . Seq.iterateN n (+m) . (+ m)
    where
      n = fromIntegral (natVal (Proxy :: Proxy n))
      m = size (Proxy :: Proxy f)

jreplicate' :: forall a n f . KnownNat n => J f a -> JVec n f a
jreplicate' el =  ret
  where
    ret = JVec (devectorize (V.replicate nvec el))
    nvec = fromIntegral (natVal (Proxy :: Proxy n))

jreplicate :: forall a n f . (KnownNat n, View f, Viewable a) => J f a -> J (JVec n f) a
jreplicate = cat . jreplicate'


reifyJVec :: forall a f r . Vector (J f a) -> (forall n . KnownNat n => JVec n f a -> r) -> r
reifyJVec v f = reifyVector v $ \(v' :: Vec n (J f a)) -> f (JVec v' :: JVec n f a)
{-# INLINE reifyJVec #-}
