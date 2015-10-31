{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.Vector ( Vector )
import Casadi.Viewable ( Viewable )

import Dyno.View.Unsafe ( JV(..), mkJ, unJ )
import Dyno.View.View ( View(..), J )
import Dyno.Vectorize ( Vectorize(..), Id, devectorize )

splitJV :: Vectorize f => J (JV f) (Vector a) -> f a
splitJV = devectorize . unJ

catJV :: Vectorize f => f a -> J (JV f) (Vector a)
catJV = mkJ . vectorize

splitJV' :: (Vectorize f, Viewable a) => J (JV f) a -> f (J (JV Id) a)
splitJV' = fmap mkJ . unJV . split

catJV' :: (Vectorize f, Viewable a) => f (J (JV Id) a) -> J (JV f) a
catJV' = cat . JV . fmap unJ
