{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Dyno.View.JV
       ( JV
       , splitJV
       , catJV
       ) where

import Data.Vector ( Vector )

import Dyno.View.Unsafe ( JV(..), mkM, unM )
import Dyno.View.View ( J )
import Dyno.Vectorize ( Vectorize(..), devectorize )

splitJV :: Vectorize f => J (JV f) (Vector a) -> f a
splitJV = devectorize . unM

catJV :: Vectorize f => f a -> J (JV f) (Vector a)
catJV = mkM . vectorize
