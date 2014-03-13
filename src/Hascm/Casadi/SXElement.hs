{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}
{-# Language Rank2Types #-}
{-# Language FlexibleContexts #-}

module Hascm.Casadi.SXElement
       ( funToSX, funSXToSX, algToSX, funToSX', SXElement
       ) where

import Data.Vector.Generic ( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive ( PrimState, PrimMonad )
import GHC.Prim ( RealWorld )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.SXElement
import Hascm.Casadi.SX ( svector )

import Dvda.Algorithm.Construct (
  Node(..), AlgOp(..), Algorithm(..), InputIdx(..), OutputIdx(..)
  )
import Dvda.Expr
import Hascm.Vectorize
import Hascm.AlgorithmV

instance Show SXElement where
  show = show . svector . V.singleton

casadiSsyms :: String -> Int -> IO (V.Vector SXElement)
casadiSsyms name k = fmap V.fromList $ mapM (sxElement'' . (name ++) . show) (take k [(0::Int)..])

-- call function directly using SXElement's unsafePerformIO Floating instances
funToSX :: (Vectorize f, Vectorize g) =>
           (forall a . Floating a => f a -> g a) -> IO (f SXElement, g SXElement)
funToSX f = do
  let asFunOf :: (f Double -> g Double) -> f Double
      asFunOf _ = undefined
      len = vlength (asFunOf f)
  inputsVec <- casadiSsyms "x" len
  let inputs = devectorize inputsVec
      outputs = f inputs
  return (inputs, outputs)

funSXToSX :: (Vectorize f, Vectorize g) =>
           (f SXElement -> g SXElement) -> IO (f SXElement, g SXElement)
funSXToSX f = do
  let asFunOf :: (f SXElement -> g SXElement) -> f SXElement
      asFunOf _ = undefined
      len = vlength (asFunOf f)
  inputsVec <- casadiSsyms "x" len
  let inputs = devectorize inputsVec
      outputs = f inputs
  return (inputs, outputs)

-- call function by using Dvda to generate an Algorithm, then use SXElement's safe IO functions
funToSX' :: (Vectorize f, Vectorize g) =>
            (forall a . Floating a => f a -> g a) -> IO (f SXElement, g SXElement)
funToSX' f = constructAlgorithmV' f >>= algToSX

algToSX :: (Vectorize f, Vectorize g) => AlgorithmV f g Double -> IO (f SXElement, g SXElement)
algToSX (AlgorithmV alg) = do
  -- work vector
  workVec <- VM.new (algWorkSize alg)

  -- outputs vector
  outputMVec <- VM.new (algOutDims alg)

  -- inputs vector
  inputsSX <- casadiSsyms "x" (algInDims alg)

  mapM_ (op workVec inputsSX outputMVec) (algOps alg)

  outputVec <- V.freeze outputMVec
  return (devectorize inputsSX, devectorize outputVec)

op :: (G.Vector v1 SXElement, GM.MVector v SXElement, GM.MVector v2 SXElement) =>
      v RealWorld SXElement -> v1 SXElement -> v2 RealWorld SXElement -> AlgOp Double -> IO ()
op work input _ (InputOp (Node k) (InputIdx i)) = GM.write work k (input ! i)
op work _ output (OutputOp (Node k) (OutputIdx i)) =
  GM.read work k >>= GM.write output i
op work _ _ (NormalOp (Node k) (GConst c)) =
  sxElement' c >>= GM.write work k
op work input output (NormalOp node (GNum (FromInteger x))) =
  op work input output (NormalOp node (GConst (fromIntegral x)))
op work input output (NormalOp node (GFractional (FromRational x))) =
  op work input output (NormalOp node (GConst (fromRational x)))

op work _ _ (NormalOp k (GNum (Mul x y)))  = bin work k x y sxElement___mul__
op work _ _ (NormalOp k (GNum (Add x y)))  = bin work k x y sxElement___add__
op work _ _ (NormalOp k (GNum (Sub x y)))  = bin work k x y sxElement___sub__
op work _ _ (NormalOp (Node k) (GNum (Negate (Node kx)))) = do
  x <- GM.read work kx
  zero <- sxElement' (0 :: Double)
  z <- sxElement___sub__ zero x
  GM.write work k z
op work _ _ (NormalOp k (GFractional (Div x y)))   = bin work k x y sxElement___truediv__
op work _ _ (NormalOp k (GNum (Abs x)))            = un work k x sxElement_fabs
op work _ _ (NormalOp k (GNum (Signum x)))         = un work k x sxElement_sign
op work _ _ (NormalOp k (GFloating (Pow x y)))     = bin work k x y sxElement___pow__
op work _ _ (NormalOp (Node k) (GFloating (LogBase (Node kx) (Node ky)))) = do
  logx <- GM.read work kx >>= sxElement_log
  logy <- GM.read work ky >>= sxElement_log
  z <- sxElement___truediv__ logy logx
  GM.write work k z

op work _ _ (NormalOp k (GFloating (Exp x)))       = un work k x sxElement_exp
op work _ _ (NormalOp k (GFloating (Log x)))       = un work k x sxElement_log
op work _ _ (NormalOp k (GFloating (Sin x)))       = un work k x sxElement_sin
op work _ _ (NormalOp k (GFloating (Cos x)))       = un work k x sxElement_cos
op work _ _ (NormalOp k (GFloating (Tan x)))       = un work k x sxElement_tan
op work _ _ (NormalOp k (GFloating (ASin x)))      = un work k x sxElement_arcsin
op work _ _ (NormalOp k (GFloating (ATan x)))      = un work k x sxElement_arctan
op work _ _ (NormalOp k (GFloating (ACos x)))      = un work k x sxElement_arccos
op work _ _ (NormalOp k (GFloating (Sinh x)))      = un work k x sxElement_sinh
op work _ _ (NormalOp k (GFloating (Cosh x)))      = un work k x sxElement_cosh
op work _ _ (NormalOp k (GFloating (Tanh x)))      = un work k x sxElement_tanh
op work _ _ (NormalOp k (GFloating (ASinh x)))     = un work k x sxElement_arcsinh
op work _ _ (NormalOp k (GFloating (ATanh x)))     = un work k x sxElement_arctanh
op work _ _ (NormalOp k (GFloating (ACosh x)))     = un work k x sxElement_arccosh
op _ _ _ (NormalOp _ (GSym _)) = error "runAlg: there's symbol in my algorithm"

bin :: (PrimMonad m, GM.MVector v t) => v (PrimState m) t -> Node -> Node -> Node -> (t -> t -> m t) -> m ()
bin work (Node k) (Node kx) (Node ky) f = do
  x <- GM.read work kx
  y <- GM.read work ky
  z <- f x y
  GM.write work k z

un :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Node -> Node -> (a -> m a) -> m ()
un work (Node k) (Node kx) f = GM.read work kx >>= f >>= GM.write work k

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
  fromInteger x = unsafePerformIO (sxElement' (fromInteger x))
  {-# NOINLINE fromInteger #-}

instance Fractional SXElement where
  (/) x y = unsafePerformIO (sxElement___truediv__ x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sxElement' (fromRational x))
  {-# NOINLINE fromRational #-}

instance Floating SXElement where
  pi = unsafePerformIO (sxElement' pi)
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
