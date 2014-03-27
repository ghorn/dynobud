{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.View.NumInstances
       (
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dyno.View.Viewable ( CasadiMat(..) )
import Dyno.Casadi.MX ( MX )
import Dyno.Casadi.SX ( SX )
import Dyno.Casadi.DMatrix ( DMatrix )

import Dyno.View.View

--------------------------- SX ---------------------------
instance (View f) => Num (J f SX) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ (x + y)
  (UnsafeJ x) - (UnsafeJ y) = mkJ (x - y)
  (UnsafeJ x) * (UnsafeJ y) = mkJ (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = mkJ (fromInteger k * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Fractional (J f SX) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ (x / y)
  fromRational x = mkJ (fromRational x * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Floating (J f SX) where
  pi = mkJ (pi * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ (x ** y)
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

--------------------------- MX ---------------------------
instance (View f) => Num (J f MX) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ (x + y)
  (UnsafeJ x) - (UnsafeJ y) = mkJ (x - y)
  (UnsafeJ x) * (UnsafeJ y) = mkJ (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = mkJ (fromInteger k * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Fractional (J f MX) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ (x / y)
  fromRational x = mkJ (fromRational x * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Floating (J f MX) where
  pi = mkJ (pi * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ (x ** y)
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

---------------------------- DMatrix ----------------------------------
instance (View f) => Num (J f DMatrix) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ (x + y)
  (UnsafeJ x) - (UnsafeJ y) = mkJ (x - y)
  (UnsafeJ x) * (UnsafeJ y) = mkJ (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = mkJ (fromInteger k * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Fractional (J f DMatrix) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ (x / y)
  fromRational x = mkJ (fromRational x * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Floating (J f DMatrix) where
  pi = mkJ (pi * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ (x ** y)
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

---------------------- Vector a ------------------------
instance (View f, Num a) => Num (J f (Vector a)) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ $ V.zipWith (+) x y
  (UnsafeJ x) - (UnsafeJ y) = mkJ $ V.zipWith (-) x y
  (UnsafeJ x) * (UnsafeJ y) = mkJ $ V.zipWith (*) x y
  abs = fmap (fmap abs)
  signum = fmap (fmap signum)
  fromInteger k = mkJ (V.replicate n (fromInteger k))
    where
      n = size (Proxy :: Proxy f)

instance (View f, Fractional a) => Fractional (J f (Vector a)) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ $ V.zipWith (/) x y
  fromRational x = mkJ (V.replicate n (fromRational x))
    where
      n = size (Proxy :: Proxy f)

instance (View f, Floating a) => Floating (J f (Vector a)) where
  pi = mkJ (V.replicate n pi)
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ $ V.zipWith (**) x y
  exp   = fmap (fmap exp)
  log   = fmap (fmap log)
  sin   = fmap (fmap sin)
  cos   = fmap (fmap cos)
  tan   = fmap (fmap tan)
  asin  = fmap (fmap asin)
  atan  = fmap (fmap atan)
  acos  = fmap (fmap acos)
  sinh  = fmap (fmap sinh)
  cosh  = fmap (fmap cosh)
  tanh  = fmap (fmap tanh)
  asinh = fmap (fmap asinh)
  atanh = fmap (fmap atanh)
  acosh = fmap (fmap acosh)
