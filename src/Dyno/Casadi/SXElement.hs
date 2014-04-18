{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}
{-# Language Rank2Types #-}

module Dyno.Casadi.SXElement
       ( funToSX, funSXToSX, SXElement, sxElement_sym
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Symbolic.Classes.SXElement

import Dyno.Casadi.SX ( svector )
import Dyno.Vectorize

instance Show SXElement where
  show = show . svector . V.singleton

casadiSsyms :: String -> Int -> IO (V.Vector SXElement)
casadiSsyms name k = fmap V.fromList $ mapM (sxElement_sym . (name ++) . show) (take k [(0::Int)..])

-- call function directly using SXElement's unsafePerformIO Floating instances
funToSX :: (Vectorize f, Vectorize g) =>
           (forall a . Floating a => f a -> g a) -> IO (f SXElement, g SXElement)
funToSX f = do
  let asFunOf :: (f Double -> g Double) -> Proxy f
      asFunOf = const Proxy
      len = vlength (asFunOf f)
  inputsVec <- casadiSsyms "x" len
  let inputs = devectorize inputsVec
      outputs = f inputs
  return (inputs, outputs)

funSXToSX :: (Vectorize f, Vectorize g) =>
           (f SXElement -> g SXElement) -> IO (f SXElement, g SXElement)
funSXToSX f = do
  let asFunOf :: (f SXElement -> g SXElement) -> Proxy f
      asFunOf = const Proxy
      len = vlength (asFunOf f)
  inputsVec <- casadiSsyms "x" len
  let inputs = devectorize inputsVec
      outputs = f inputs
  return (inputs, outputs)


instance Num SXElement where
  (+) x y = unsafePerformIO (sxElement___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sxElement___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sxElement___mul__ x y)
  {-# NOINLINE (*) #-}
  abs x = unsafePerformIO (sxElement_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sxElement_sign x)
  {-# NOINLINE signum #-}
  fromInteger x = unsafePerformIO (sxElement__0 (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}

instance Fractional SXElement where
  (/) x y = unsafePerformIO (sxElement___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sxElement__0 (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating SXElement where
  pi = unsafePerformIO (sxElement__0 (pi :: Double))
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (sxElement___pow__ x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (sxElement_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (sxElement_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (sxElement_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (sxElement_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (sxElement_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (sxElement_arcsin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (sxElement_arctan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (sxElement_arccos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (sxElement_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (sxElement_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (sxElement_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (sxElement_arcsinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (sxElement_arctanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (sxElement_arccosh x)
  {-# NOINLINE acosh #-}
