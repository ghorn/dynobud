{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.DMatrix
       ( DMatrix(), dcrs, dmm, dvector, ddata, ddiag
       , ddense, dsparse, dtrans, dtriu, dtril
       , dsize, dsize1, dsize2, dnumel
       , dvertcat, dhorzcat, dveccat, dvertsplit, dhorzsplit
       , dones, dzeros
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Core.Classes.Sparsity
import Casadi.Core.Classes.DMatrix
import qualified Casadi.Core.Tools as C

import Dyno.Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..) )

-- | matrix matrix product
dmm :: DMatrix -> DMatrix -> DMatrix
dmm x y = unsafePerformIO (dmatrix_mul__0 x y)
{-# NOINLINE dmm #-}

-- | transpose
dtrans :: DMatrix -> DMatrix
dtrans x = unsafePerformIO (dmatrix_trans x)
{-# NOINLINE dtrans #-}

ddense :: DMatrix -> DMatrix
ddense x = unsafePerformIO (C.dense__2 x)
{-# NOINLINE ddense #-}

dsparse :: DMatrix -> DMatrix
dsparse x = unsafePerformIO (C.sparse__2 x)
{-# NOINLINE dsparse #-}

dcrs :: DMatrix -> Sparsity
dcrs x = unsafePerformIO (dmatrix_sparsityRef__0 x)
{-# NOINLINE dcrs #-}

ddiag :: DMatrix -> DMatrix
ddiag x = unsafePerformIO (C.diag__2 x)
{-# NOINLINE ddiag #-}

-- | from vector
dvector :: V.Vector Double -> DMatrix
dvector x = unsafePerformIO (dmatrix__4 x)
{-# NOINLINE dvector #-}

ddata :: DMatrix -> V.Vector Double
ddata x = unsafePerformIO (dmatrix_data__0 x)
{-# NOINLINE ddata #-}

dsize :: DMatrix -> Int
dsize x = unsafePerformIO (dmatrix_size__1 x)
{-# NOINLINE dsize #-}

dsize1 :: DMatrix -> Int
dsize1 x = unsafePerformIO (dmatrix_size1 x)
{-# NOINLINE dsize1 #-}

dsize2 :: DMatrix -> Int
dsize2 x = unsafePerformIO (dmatrix_size2 x)
{-# NOINLINE dsize2 #-}

dnumel :: DMatrix -> Int
dnumel x = unsafePerformIO (dmatrix_numel x)
{-# NOINLINE dnumel #-}

dvertcat :: V.Vector DMatrix -> DMatrix
dvertcat x = unsafePerformIO (C.vertcat__3 x)
{-# NOINLINE dvertcat #-}

dveccat :: V.Vector DMatrix -> DMatrix
dveccat x = unsafePerformIO (C.veccat__2 x)
{-# NOINLINE dveccat #-}

dvertsplit :: DMatrix -> V.Vector Int -> V.Vector DMatrix
dvertsplit x ks = unsafePerformIO (C.vertsplit__9 x ks)
{-# NOINLINE dvertsplit #-}

dhorzsplit :: DMatrix -> V.Vector Int -> V.Vector DMatrix
dhorzsplit x ks = unsafePerformIO (C.horzsplit__9 x ks)
{-# NOINLINE dhorzsplit #-}

dhorzcat :: V.Vector DMatrix -> DMatrix
dhorzcat x = unsafePerformIO (C.horzcat__3 x)
{-# NOINLINE dhorzcat #-}

dtriu :: DMatrix -> DMatrix
dtriu x = unsafePerformIO (C.triu__4 (castDMatrix x))
{-# NOINLINE dtriu #-}

dtril :: DMatrix -> DMatrix
dtril x = unsafePerformIO (C.tril__4 (castDMatrix x))
{-# NOINLINE dtril #-}

dones :: (Int,Int) -> DMatrix
dones (r,c) = unsafePerformIO (dmatrix_ones__3 r c)
{-# NOINLINE dones #-}

dzeros :: (Int,Int) -> DMatrix
dzeros (r,c) = unsafePerformIO (dmatrix_zeros__3 r c)
{-# NOINLINE dzeros #-}

instance Num DMatrix where
  (+) x y = unsafePerformIO (dmatrix___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (dmatrix___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (dmatrix___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (dmatrix__5 (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (dmatrix_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (dmatrix_sign x)
  {-# NOINLINE signum #-}

instance Fractional DMatrix where
  (/) x y = unsafePerformIO (dmatrix___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (dmatrix__5 (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating DMatrix where
  pi = unsafePerformIO (dmatrix__5 (pi :: Double))
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

instance Fmod DMatrix where
  fmod x y = unsafePerformIO (dmatrix_fmod x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 DMatrix where
  arctan2 x y = unsafePerformIO (dmatrix_arctan2 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd DMatrix where
  x `leq` y = unsafePerformIO (dmatrix___le__ x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (dmatrix___ge__ x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (dmatrix___eq__ x y)
  {-# NOINLINE eq #-}
