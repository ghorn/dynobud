{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Hascm.Casadi.DMatrix
       ( DMatrix(), dcrs, dmm, dvector, ddata
       , dfull, dtrans
       , dsize, dsize1, dsize2, dnumel
       , dvertcat, dhorzcat
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.Sparsity
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.GenDMatrix
import qualified Casadi.Wrappers.Tools as C

-- | matrix matrix product
dmm :: DMatrix -> DMatrix -> DMatrix
dmm x y = unsafePerformIO (dmatrix_mul' x y)
{-# NOINLINE dmm #-}

-- | transpose
dtrans :: DMatrix -> DMatrix
dtrans x = unsafePerformIO (dmatrix_trans x)
{-# NOINLINE dtrans #-}

dfull :: DMatrix -> DMatrix
dfull x = unsafePerformIO (C.full' x)
{-# NOINLINE dfull #-}

dcrs :: DMatrix -> Sparsity
dcrs x = unsafePerformIO (dmatrix_sparsityRef x)
{-# NOINLINE dcrs #-}

-- | from vector
dvector :: V.Vector Double -> DMatrix
dvector x = unsafePerformIO (dmatrix''''''''''' x)
{-# NOINLINE dvector #-}

ddata :: DMatrix -> V.Vector Double
ddata x = unsafePerformIO (dmatrix_data x)
{-# NOINLINE ddata #-}

dsize :: DMatrix -> Int
dsize x = unsafePerformIO (genDMatrix_size x)
{-# NOINLINE dsize #-}

dsize1 :: DMatrix -> Int
dsize1 x = unsafePerformIO (genDMatrix_size1 x)
{-# NOINLINE dsize1 #-}

dsize2 :: DMatrix -> Int
dsize2 x = unsafePerformIO (genDMatrix_size2 x)
{-# NOINLINE dsize2 #-}

dnumel :: DMatrix -> Int
dnumel x = unsafePerformIO (genDMatrix_numel x)
{-# NOINLINE dnumel #-}

dvertcat :: V.Vector DMatrix -> DMatrix
dvertcat x = unsafePerformIO (C.vertcat' x)
{-# NOINLINE dvertcat #-}

dhorzcat :: V.Vector DMatrix -> DMatrix
dhorzcat x = unsafePerformIO (C.horzcat' x)
{-# NOINLINE dhorzcat #-}


instance Num DMatrix where
  (+) x y = unsafePerformIO (dmatrix___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (dmatrix___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (dmatrix___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (dmatrix'''''''''' (fromInteger x))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (dmatrix_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (dmatrix_sign x)
  {-# NOINLINE signum #-}

instance Fractional DMatrix where
  (/) x y = unsafePerformIO (dmatrix___truediv__ x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (dmatrix'''''''''' (fromRational x))
  {-# NOINLINE fromRational #-}

instance Floating DMatrix where
  pi = unsafePerformIO (dmatrix' pi)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (dmatrix___pow__ x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (dmatrix_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (dmatrix_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (dmatrix_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (dmatrix_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (dmatrix_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (dmatrix_arcsin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (dmatrix_arctan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (dmatrix_arccos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (dmatrix_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (dmatrix_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (dmatrix_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (dmatrix_arcsinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (dmatrix_arctanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (dmatrix_arccosh x)
  {-# NOINLINE acosh #-}
