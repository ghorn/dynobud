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

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Serialize ( Serialize(..) )

import Dyno.View.Internal.View ( mkJ, unJ )

import Dyno.TypeVecs ( Vec(..), unVec, mkVec, mkVec', reifyVector )
import Dyno.View.Viewable ( Viewable(..) )
import Dyno.View.View ( View(..), J )


-- | vectors in View
newtype JVec n f a = JVec { unJVec :: Vec n (J f a) } deriving ( Show, Eq )
instance (Dim n, View f) => View (JVec n f) where
  cat = mkJ . vveccat . fmap unJ . unVec . unJVec
  split = JVec . fmap mkJ . mkVec . flip vvertsplit ks . unJ
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
instance (Dim n, Serialize (J f a)) => Serialize (JVec n f a) where
  get = fmap (JVec . mkVec') get
  put = put . F.toList . unJVec

jreplicate' :: forall a n f . (Dim n, View f) => J f a -> JVec n f a
jreplicate' el =  ret
  where
    ret = JVec (mkVec (V.replicate nvec el))
    nvec = reflectDim (Proxy :: Proxy n)

jreplicate :: forall a n f . (Dim n, View f, Viewable a) => J f a -> J (JVec n f) a
jreplicate = cat . jreplicate'


reifyJVec :: forall a f r . Vector (J f a) -> (forall (n :: *). Dim n => JVec n f a -> r) -> r
reifyJVec v f = reifyVector v $ \(v' :: Vec n (J f a)) -> f (JVec v' :: JVec n f a)
{-# INLINE reifyJVec #-}

