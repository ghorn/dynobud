{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}
{-# Language Rank2Types #-}
{-# Language FlexibleContexts #-}

module Hascm.Casadi.SX ( funToSX, algToSX, funToSX', SX ) where

import Data.Vector.Generic ( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive ( PrimState, PrimMonad )
import GHC.Prim ( RealWorld )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.SX

import Dvda.Algorithm.Construct (
  Node(..), AlgOp(..), Algorithm(..), InputIdx(..), OutputIdx(..)
  )
import Dvda.Expr
import Hascm.Vectorize
import Hascm.AlgorithmV

casadiSsyms :: String -> Int -> IO (V.Vector SX)
casadiSsyms name k = fmap V.fromList $ mapM (sx'' . (name ++) . show) (take k [(0::Int)..])

-- call function directly using SX's unsafePerformIO Floating instances
funToSX' :: (Vectorize f, Vectorize g) =>
            (forall a . Floating a => f a -> g a) -> IO (f SX, g SX)
funToSX' f = do
  let asFunOf :: (f Double -> g Double) -> f Double
      asFunOf _ = undefined
      len = vlength (asFunOf f)
  inputsVec <- casadiSsyms "x" len
  let inputs = devectorize inputsVec
      outputs = f inputs
  return (inputs, outputs)

-- call function by using Dvda to generate an Algorithm, then use SX's safe IO functions
funToSX :: (Vectorize f, Vectorize g) =>
           (forall a . Floating a => f a -> g a) -> IO (f SX, g SX)
funToSX f = constructAlgorithmV' f >>= algToSX

algToSX :: (Vectorize f, Vectorize g) => AlgorithmV f g Double -> IO (f SX, g SX)
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

op :: (G.Vector v1 SX, GM.MVector v SX, GM.MVector v2 SX) =>
      v RealWorld SX -> v1 SX -> v2 RealWorld SX -> AlgOp Double -> IO ()
op work input _ (InputOp (Node k) (InputIdx i)) = GM.write work k (input ! i)
op work _ output (OutputOp (Node k) (OutputIdx i)) =
  GM.read work k >>= GM.write output i
op work _ _ (NormalOp (Node k) (GConst c)) =
  sx' c >>= GM.write work k
op work input output (NormalOp node (GNum (FromInteger x))) =
  op work input output (NormalOp node (GConst (fromIntegral x)))
op work input output (NormalOp node (GFractional (FromRational x))) =
  op work input output (NormalOp node (GConst (fromRational x)))

op work _ _ (NormalOp k (GNum (Mul x y)))  = bin work k x y sx___mul__
op work _ _ (NormalOp k (GNum (Add x y)))  = bin work k x y sx___add__
op work _ _ (NormalOp k (GNum (Sub x y)))  = bin work k x y sx___sub__
op work _ _ (NormalOp (Node k) (GNum (Negate (Node kx)))) = do
  x <- GM.read work kx
  zero <- sx' (0 :: Double)
  z <- sx___sub__ zero x
  GM.write work k z
op work _ _ (NormalOp k (GFractional (Div x y)))   = bin work k x y sx___truediv__
op work _ _ (NormalOp k (GNum (Abs x)))            = un work k x sx_fabs
op work _ _ (NormalOp k (GNum (Signum x)))         = un work k x sx_sign
op work _ _ (NormalOp k (GFloating (Pow x y)))     = bin work k x y sx___pow__
op work _ _ (NormalOp (Node k) (GFloating (LogBase (Node kx) (Node ky)))) = do
  logx <- GM.read work kx >>= sx_log
  logy <- GM.read work ky >>= sx_log
  z <- sx___truediv__ logy logx
  GM.write work k z

op work _ _ (NormalOp k (GFloating (Exp x)))       = un work k x sx_exp
op work _ _ (NormalOp k (GFloating (Log x)))       = un work k x sx_log
op work _ _ (NormalOp k (GFloating (Sin x)))       = un work k x sx_sin
op work _ _ (NormalOp k (GFloating (Cos x)))       = un work k x sx_cos
op work _ _ (NormalOp k (GFloating (Tan x)))       = un work k x sx_tan
op work _ _ (NormalOp k (GFloating (ASin x)))      = un work k x sx_arcsin
op work _ _ (NormalOp k (GFloating (ATan x)))      = un work k x sx_arctan
op work _ _ (NormalOp k (GFloating (ACos x)))      = un work k x sx_arccos
op work _ _ (NormalOp k (GFloating (Sinh x)))      = un work k x sx_sinh
op work _ _ (NormalOp k (GFloating (Cosh x)))      = un work k x sx_cosh
op work _ _ (NormalOp k (GFloating (Tanh x)))      = un work k x sx_tanh
op work _ _ (NormalOp k (GFloating (ASinh x)))     = un work k x sx_arcsinh
op work _ _ (NormalOp k (GFloating (ATanh x)))     = un work k x sx_arctanh
op work _ _ (NormalOp k (GFloating (ACosh x)))     = un work k x sx_arccosh
op _ _ _ (NormalOp _ (GSym _)) = error "runAlg: there's symbol in my algorithm"

bin :: (PrimMonad m, GM.MVector v t) => v (PrimState m) t -> Node -> Node -> Node -> (t -> t -> m t) -> m ()
bin work (Node k) (Node kx) (Node ky) f = do
  x <- GM.read work kx
  y <- GM.read work ky
  z <- f x y
  GM.write work k z

un :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Node -> Node -> (a -> m a) -> m ()
un work (Node k) (Node kx) f = GM.read work kx >>= f >>= GM.write work k

instance Num SX where
  (+) x y = unsafePerformIO (sx___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sx___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sx___mul__ x y)
  {-# NOINLINE (*) #-}
  abs x = unsafePerformIO (sx_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sx_sign x)
  {-# NOINLINE signum #-}
  fromInteger x = unsafePerformIO (sx' (fromInteger x))
  {-# NOINLINE fromInteger #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (sx___truediv__ x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sx' (fromRational x))
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = unsafePerformIO (sx' pi)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (sx___pow__ x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (sx_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (sx_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (sx_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (sx_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (sx_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (sx_arcsin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (sx_arctan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (sx_arccos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (sx_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (sx_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (sx_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (sx_arcsinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (sx_arctanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (sx_arccosh x)
  {-# NOINLINE acosh #-}
