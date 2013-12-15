{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Hascm.Casadi.SXMatrix ( SXMatrix(), ssym, ssymV, ssymM, smm, strans
                             , sgradient, sjacobian, shessian, svector
                             , sdata, ssparse, sdensify
                             , scrs, svertcat ) where

import Control.Monad ( when )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.SX
import Casadi.Wrappers.Classes.SXMatrix
import Casadi.Wrappers.Classes.CRSSparsity
import qualified Casadi.Wrappers.Tools as C

ssym :: String -> IO SXMatrix
ssym = C.ssym''

ssymV :: String -> Int -> IO SXMatrix
ssymV = C.ssym'

ssymM :: String -> Int -> Int -> IO SXMatrix
ssymM = C.ssym

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sgradient :: SXMatrix -> SXMatrix -> SXMatrix
sgradient x y = unsafePerformIO (C.gradient x y)
{-# NOINLINE sgradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sjacobian :: SXMatrix -> SXMatrix -> SXMatrix
sjacobian x y = unsafePerformIO (C.jacobian x y)
{-# NOINLINE sjacobian #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
shessian :: SXMatrix -> SXMatrix -> SXMatrix
shessian x y = unsafePerformIO (C.hessian x y)
{-# NOINLINE shessian #-}

-- | matrix matrix product
smm :: SXMatrix -> SXMatrix -> SXMatrix
smm x y = unsafePerformIO (sxMatrix_mul' x y)
{-# NOINLINE smm #-}

-- | transpose
strans :: SXMatrix -> SXMatrix
strans x = unsafePerformIO (sxMatrix_trans x)
{-# NOINLINE strans #-}

sdensify :: SXMatrix -> SXMatrix
sdensify x = unsafePerformIO (C.densify'' x)
{-# NOINLINE sdensify #-}

scrs :: SXMatrix -> CRSSparsity
scrs x = unsafePerformIO (sxMatrix_sparsityRef x)
{-# NOINLINE scrs #-}

-- | from SX vector
svector :: V.Vector SX -> SXMatrix
svector x = unsafePerformIO (sxMatrix''''''''''' x)
{-# NOINLINE svector #-}

sdata :: SXMatrix -> V.Vector SX
sdata x = unsafePerformIO (sxMatrix_data x)
{-# NOINLINE sdata #-}

svertcat :: V.Vector SXMatrix -> SXMatrix
svertcat x = unsafePerformIO (C.vertcat'' x)
{-# NOINLINE svertcat #-}

ssparse :: SXMatrix -> V.Vector (Int,Int,SX)
ssparse sxm = unsafePerformIO $ do
  let crs = scrs sxm
  row <- crsSparsity_getRow crs
  col <- crsSparsity_colRef crs
  let sxs = sdata sxm
  when (V.length row /= V.length col) $ error "ssparse: row/col dimension mismatch"
  when (V.length row /= V.length sxs) $ error "ssparse: row/sxs dimension mismatch"
  return $ V.zip3 row col sxs
{-# NOINLINE ssparse #-}

instance Num SXMatrix where
  (+) x y = unsafePerformIO (sxMatrix___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sxMatrix___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sxMatrix___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (sxMatrix'''''''''' (fromInteger x))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (sxMatrix_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sxMatrix_sign x)
  {-# NOINLINE signum #-}

instance Fractional SXMatrix where
  (/) x y = unsafePerformIO (sxMatrix___truediv__ x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sxMatrix'''''''''' (fromRational x))
  {-# NOINLINE fromRational #-}

instance Floating SXMatrix where
  pi = unsafePerformIO (sxMatrix' pi)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (sxMatrix___pow__ x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (sxMatrix_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (sxMatrix_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (sxMatrix_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (sxMatrix_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (sxMatrix_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (sxMatrix_arcsin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (sxMatrix_arctan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (sxMatrix_arccos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (sxMatrix_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (sxMatrix_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (sxMatrix_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (sxMatrix_arcsinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (sxMatrix_arctanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (sxMatrix_arccosh x)
  {-# NOINLINE acosh #-}
