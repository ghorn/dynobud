{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Dyno.View.JV
       ( JV
       , splitJV
       , catJV
       , splitJV'
       , catJV'
       ) where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Sequence as Seq
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Accessors ( Lookup(..) )

import Dyno.View.Unsafe.View ( mkJ, unJ )

import Dyno.View.View ( View(..), J )
import Dyno.View.Viewable ( Viewable(..) )
import Dyno.Vectorize ( Vectorize(..), Id, vlength )

-- | views into Vectorizable things
newtype JV f a = JV { unJV :: f a } deriving (Functor, Generic, Generic1)

instance Vectorize f => View (JV f) where
  cat :: forall a . Viewable a => JV f a -> J (JV f) a
  cat = mkJ . vveccat . vectorize . unJV
  size = const $ vlength (Proxy :: Proxy f)
  sizes = const . Seq.singleton . (vlength (Proxy :: Proxy f) +)
  split :: forall a . Viewable a => J (JV f) a -> JV f a
  split = JV . devectorize . flip vvertsplit ks. unJ
    where
      ks = V.fromList (take (n+1) [0..])
      n = size (Proxy :: Proxy (JV f))

-- todo: pretty sure this is not needed anymore
instance (Vectorize f, Lookup (f a)) => Lookup (J (JV f) (Vector a)) where
  toAccessorTree x g = toAccessorTree (devectorize (unJ x) :: f a) (devectorize . unJ . g)

splitJV :: Vectorize f => J (JV f) (Vector a) -> f a
splitJV = devectorize . unJ

catJV :: Vectorize f => f a -> J (JV f) (Vector a)
catJV = mkJ . vectorize

splitJV' :: (Vectorize f, Viewable a) => J (JV f) a -> f (J (JV Id) a)
splitJV' = fmap mkJ . unJV . split

catJV' :: (Vectorize f, Viewable a) => f (J (JV Id) a) -> J (JV f) a
catJV' = cat . JV . fmap unJ
