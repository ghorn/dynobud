{-# OPTIONS_GHC -Wall #-}

-- | This is it's own module as a work-around to https://ghc.haskell.org/trac/ghc/ticket/8913

module Dyno.Casadi.MXVInternal
       ( GCat(..), GSize(..), GSplit(..)
       ) where

import Data.Proxy
import Dyno.Casadi.MXVData

class GCat f where
  gcat :: f p -> MX

class GSize f where
  gsize :: Proxy (f p) -> Int

class GSplit f where
  gsplit :: MX -> f p
