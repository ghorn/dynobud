{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.SX
       ( SX(), ssym, ssymV, ssymM, smm, strans
       , sgradient, sjacobian, shessian, svector, sdiag
       , ssolve
       , sdata
       , striu
       , stril
       , sdense, ssparse
--       , d2s
       , ssize, ssize1, ssize2, snumel
       , scrs, svertcat, shorzcat, sveccat, svertsplit
       , sones, szeros
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.SXElement ( SXElement )
import Casadi.Wrappers.Classes.SX
import Casadi.Wrappers.Classes.GenSX
--import Casadi.Wrappers.Classes.DMatrix ( DMatrix )
import Casadi.Wrappers.Classes.Sparsity ( Sparsity )
import qualified Casadi.Wrappers.Tools as C

ssym :: String -> IO SX
ssym = genSX_sym''

ssymV :: String -> Int -> IO SX
ssymV = genSX_sym'

ssymM :: String -> Int -> Int -> IO SX
ssymM = genSX_sym

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sgradient :: SX -> SX -> SX
sgradient x y = unsafePerformIO (C.gradient x y)
{-# NOINLINE sgradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sjacobian :: SX -> SX -> SX
sjacobian x y = unsafePerformIO (C.jacobian x y)
{-# NOINLINE sjacobian #-}

-- | @hessian exp x@ is the hessian of exp w.r.t. x
shessian :: SX -> SX -> SX
shessian x y = unsafePerformIO (C.hessian x y)
{-# NOINLINE shessian #-}

-- | matrix matrix product
smm :: SX -> SX -> SX
smm x y = unsafePerformIO (sx_mul' x y)
{-# NOINLINE smm #-}

--d2s :: DMatrix -> SX
--d2s x = unsafePerformIO (C.ssym'''''''' x)
--{-# NOINLINE d2s #-}

sdiag :: SX -> SX
sdiag x = unsafePerformIO (C.diag'' x)
{-# NOINLINE sdiag #-}

-- | transpose
strans :: SX -> SX
strans x = unsafePerformIO (sx_trans x)
{-# NOINLINE strans #-}

sdense :: SX -> SX
sdense x = unsafePerformIO (C.dense'' x)
{-# NOINLINE sdense #-}

ssparse :: SX -> SX
ssparse x = unsafePerformIO (C.sparse''''' x)
{-# NOINLINE ssparse #-}

striu :: SX -> SX
striu x = unsafePerformIO (C.triu'' (castGenSX x))
{-# NOINLINE striu #-}

stril :: SX -> SX
stril x = unsafePerformIO (C.tril'' (castGenSX x))
{-# NOINLINE stril #-}

scrs :: SX -> Sparsity
scrs x = unsafePerformIO (sx_sparsityRef x)
{-# NOINLINE scrs #-}

-- | from SXElement vector
svector :: V.Vector SXElement -> SX
svector x = unsafePerformIO (sx''''''' x)
{-# NOINLINE svector #-}

sdata :: SX -> V.Vector SXElement
sdata x = unsafePerformIO (sx_data x)
{-# NOINLINE sdata #-}

ssize :: SX -> Int
ssize x = unsafePerformIO (genSX_size x)
{-# NOINLINE ssize #-}

ssize1 :: SX -> Int
ssize1 x = unsafePerformIO (genSX_size1 x)
{-# NOINLINE ssize1 #-}

ssize2 :: SX -> Int
ssize2 x = unsafePerformIO (genSX_size2 x)
{-# NOINLINE ssize2 #-}

snumel :: SX -> Int
snumel x = unsafePerformIO (genSX_numel x)
{-# NOINLINE snumel #-}

svertcat :: V.Vector SX -> SX
svertcat x = unsafePerformIO (C.vertcat'' x)
{-# NOINLINE svertcat #-}

shorzcat :: V.Vector SX -> SX
shorzcat x = unsafePerformIO (C.horzcat'' x)
{-# NOINLINE shorzcat #-}

sveccat :: V.Vector SX -> SX
sveccat x = unsafePerformIO (C.veccat'' x)
{-# NOINLINE sveccat #-}

svertsplit :: SX -> V.Vector Int -> V.Vector SX
svertsplit x ks = unsafePerformIO (C.vertsplit'''''' x ks)
{-# NOINLINE svertsplit #-}

ssolve :: SX -> SX -> SX
ssolve a b = unsafePerformIO (C.solve'' a b)
{-# NOINLINE ssolve #-}

sones :: (Int,Int) -> SX
sones (r,c) = unsafePerformIO (genSX_ones r c)
{-# NOINLINE sones #-}

szeros :: (Int,Int) -> SX
szeros (r,c) = unsafePerformIO (genSX_zeros r c)
{-# NOINLINE szeros #-}

instance Num SX where
  (+) x y = unsafePerformIO (sx___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sx___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sx___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (sx'''''' (fromInteger x))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (sx_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sx_sign x)
  {-# NOINLINE signum #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (sx___truediv__ x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sx'''''' (fromRational x))
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = unsafePerformIO (sx'''''' pi)
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
