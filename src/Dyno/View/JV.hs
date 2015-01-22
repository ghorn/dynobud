{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Dyno.View.JV
       ( JV(..)
       , splitJV
       , catJV
       , splitJV'
       , catJV'
       , sxSplitJV
       , sxCatJV
       ) where

import GHC.Generics hiding ( S )

import qualified Data.Sequence as Seq
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Casadi.SX ( SX )

import Dyno.SXElement
import Dyno.View.Viewable ( Viewable(..) )
import Dyno.View.View
import Dyno.Vectorize ( Vectorize(..), Id, vlength )
import Dyno.Server.Accessors ( Lookup(..) )

-- | views into Vectorizable things
newtype JV f a = JV { unJV :: f a } deriving (Functor, Generic)

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

sxSplitJV :: Vectorize f => J (JV f) SX -> f SXElement
sxSplitJV v = fmap f (splitJV' v)
  where
    f :: J (JV Id) SX -> SXElement
    f (UnsafeJ x) = sxToSXElement x

sxCatJV :: Vectorize f => f SXElement -> J (JV f) SX
sxCatJV v = catJV' (fmap f v)
  where
    f :: SXElement -> J (JV Id) SX
    f x = mkJ (sxElementToSX x)
