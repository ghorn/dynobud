{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.MX
       ( MX(), sym, symV, symM, mm, trans, diag
       , gradient, jacobian -- , hessian
       , solve
       , expand
       , triu
       , tril
       , dense --, sparse
       , d2m
       , size, size1, size2, numel
       , crs, vertcat, horzcat, veccat, vertsplit
       , ones, zeros
       ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.MX
import Casadi.Wrappers.Classes.DMatrix ( DMatrix )
import Casadi.Wrappers.Classes.GenMX
import Casadi.Wrappers.Classes.Sparsity ( Sparsity )
import qualified Casadi.Wrappers.Tools as C

sym :: String -> IO MX
sym x = fmap castMX (genMX_sym'' x)

symV :: String -> Int -> IO MX
symV x y = fmap castMX (genMX_sym' x y)

symM :: String -> Int -> Int -> IO MX
symM x y z = fmap castMX (genMX_sym x y z)

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
gradient :: MX -> MX -> MX
gradient x y = unsafePerformIO (C.gradient' x y)
{-# NOINLINE gradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
jacobian :: MX -> MX -> MX
jacobian x y = unsafePerformIO (C.jacobian' x y)
{-# NOINLINE jacobian #-}

expand :: Vector MX -> Vector MX
expand x = unsafePerformIO (C.matrix_expand''' x)
{-# NOINLINE expand #-}

solve :: MX -> MX -> MX
solve a b = unsafePerformIO (C.solve''' a b)
{-# NOINLINE solve #-}

---- | @hessian exp x@ is the jacobian of exp w.r.t. x
--hessian :: MX -> MX -> MX
--hessian x y = unsafePerformIO (C.hessian x y)
--{-# NOINLINE hessian #-}

d2m :: DMatrix -> MX
d2m x = unsafePerformIO (mx'''''''' x)
{-# NOINLINE d2m #-}

-- | matrix matrix product
mm :: MX -> MX -> MX
mm x y = unsafePerformIO (mx_mul' x y)
{-# NOINLINE mm #-}

-- | transpose
trans :: MX -> MX
trans x = unsafePerformIO (mx_trans x)
{-# NOINLINE trans #-}

dense :: MX -> MX
dense x = unsafePerformIO (C.dense''' x)
{-# NOINLINE dense #-}

--sparse :: MX -> MX
--sparse x = unsafePerformIO (C.sparse x)
--{-# NOINLINE sparse #-}

triu :: MX -> MX
triu x = unsafePerformIO (C.triu''' (castGenMX x))
{-# NOINLINE triu #-}

tril :: MX -> MX
tril x = unsafePerformIO (C.tril''' (castGenMX x))
{-# NOINLINE tril #-}

diag :: MX -> MX
diag x = unsafePerformIO (C.diag''' x)
{-# NOINLINE diag #-}

crs :: MX -> Sparsity
crs x = unsafePerformIO (mx_sparsityRef x)
{-# NOINLINE crs #-}

-- | from MXElement vector
size :: MX -> Int
size x = unsafePerformIO (genMX_size x)
{-# NOINLINE size #-}

size1 :: MX -> Int
size1 x = unsafePerformIO (genMX_size1 x)
{-# NOINLINE size1 #-}

size2 :: MX -> Int
size2 x = unsafePerformIO (genMX_size2 x)
{-# NOINLINE size2 #-}

numel :: MX -> Int
numel x = unsafePerformIO (genMX_numel x)
{-# NOINLINE numel #-}

vertcat :: V.Vector MX -> MX
vertcat x = unsafePerformIO (C.vertcat'''' x)
{-# NOINLINE vertcat #-}

veccat :: V.Vector MX -> MX
veccat x = unsafePerformIO (C.veccat''' x)
{-# NOINLINE veccat #-}

vertsplit :: MX -> V.Vector Int -> V.Vector MX
vertsplit x ks = unsafePerformIO (C.vertsplit'''''''''' x ks)
{-# NOINLINE vertsplit #-}

horzcat :: V.Vector MX -> MX
horzcat x = unsafePerformIO (C.horzcat'''' x)
{-# NOINLINE horzcat #-}

ones :: (Int,Int) -> MX
ones (r,c) = unsafePerformIO (genMX_ones r c)
{-# NOINLINE ones #-}

zeros :: (Int,Int) -> MX
zeros (r,c) = unsafePerformIO (genMX_zeros r c)
{-# NOINLINE zeros #-}

instance Num MX where
  (+) x y = unsafePerformIO (mx___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (mx___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (mx___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (mx''''' (fromInteger x))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (mx_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (mx_sign x)
  {-# NOINLINE signum #-}

instance Fractional MX where
  (/) x y = unsafePerformIO (mx___truediv__ x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (mx''''' (fromRational x))
  {-# NOINLINE fromRational #-}

instance Floating MX where
  pi = unsafePerformIO (mx''''' pi)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (mx___pow__ x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (mx_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (mx_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (mx_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (mx_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (mx_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (mx_arcsin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (mx_arctan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (mx_arccos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (mx_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (mx_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (mx_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (mx_arcsinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (mx_arctanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (mx_arccosh x)
  {-# NOINLINE acosh #-}
