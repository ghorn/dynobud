{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Dyno.Casadi.MXVClassesInternal where

import GHC.Generics
import Data.Proxy

import Dyno.Casadi.MXVData

class Cat f where
  cat :: f -> MX
  default cat :: (GCat (Rep f), Generic f) => f -> MX
  cat x = gcat (from x)
class GCat f where
  gcat :: f p -> MX


class Size f where
  size :: Proxy f -> Int
  default size :: (GSize (Rep f), Generic f) => Proxy f -> Int
  size x = gsize (reproxy x)
    where
      reproxy :: Proxy g -> Proxy ((Rep g) p)
      reproxy = const Proxy
class GSize f where
  gsize :: Proxy (f p) -> Int


class Split f where
  split :: MX -> f
  default split :: (GSplit (Rep f), Generic f) => MX -> f
  split x = to (gsplit x)
class GSplit f where
  gsplit :: MX -> f p
