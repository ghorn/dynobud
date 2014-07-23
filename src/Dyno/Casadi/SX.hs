{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.SX
       ( SX(), ssym, ssymV, ssymM, smm, strans
       , sgradient, sjacobian, shessian, svector, sdiag
       , ssolve
       , sdata
       , striu
       , stril
       , sdense, ssparse
       , d2s
       , ssize, ssize1, ssize2, snumel
       , scrs, svertcat, shorzcat, sveccat, svertsplit, shorzsplit
       , sones, szeros
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.SXElement ( SXElement )
import Casadi.Core.Classes.SX
import Casadi.Core.Classes.DMatrix ( DMatrix )
import Casadi.Core.Classes.Sparsity ( Sparsity )
import qualified Casadi.Core.Tools as C

import Dyno.Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..) )

instance Conjugate SX where
  conjugate = id

ssym :: String -> IO SX
ssym = sx_sym__5

ssymV :: String -> Int -> IO SX
ssymV = sx_sym__6

ssymM :: String -> Int -> Int -> IO SX
ssymM = sx_sym__7

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sgradient :: SX -> SX -> SX
sgradient x y = unsafePerformIO (C.gradient__1 x y)
{-# NOINLINE sgradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sjacobian :: SX -> SX -> SX
sjacobian x y = unsafePerformIO (C.jacobian__1 x y)
{-# NOINLINE sjacobian #-}

-- | @hessian exp x@ is the hessian of exp w.r.t. x
shessian :: SX -> SX -> SX
shessian x y = unsafePerformIO (C.hessian__1 x y)
{-# NOINLINE shessian #-}

-- | matrix matrix product
smm :: SX -> SX -> SX
smm x y = unsafePerformIO (sx_mul__0 x y)
{-# NOINLINE smm #-}

d2s :: DMatrix -> SX
d2s x = unsafePerformIO (sx__2 x)
{-# NOINLINE d2s #-}

sdiag :: SX -> SX
sdiag x = unsafePerformIO (C.diag__1 x)
{-# NOINLINE sdiag #-}

-- | transpose
strans :: SX -> SX
strans x = unsafePerformIO (sx_trans x)
{-# NOINLINE strans #-}

sdense :: SX -> SX
sdense x = unsafePerformIO (C.dense__1 x)
{-# NOINLINE sdense #-}

ssparse :: SX -> SX
ssparse x = unsafePerformIO (C.sparse__0 x)
{-# NOINLINE ssparse #-}

striu :: SX -> SX
striu x = unsafePerformIO (C.triu__3 (castSX x))
{-# NOINLINE striu #-}

stril :: SX -> SX
stril x = unsafePerformIO (C.tril__3 (castSX x))
{-# NOINLINE stril #-}

scrs :: SX -> Sparsity
scrs x = unsafePerformIO (sx_sparsityRef__0 x)
{-# NOINLINE scrs #-}

-- | from SXElement vector
svector :: V.Vector SXElement -> SX
svector x = unsafePerformIO (sx__7 x)
{-# NOINLINE svector #-}

sdata :: SX -> V.Vector SXElement
sdata x = unsafePerformIO (sx_data__0 x)
{-# NOINLINE sdata #-}

ssize :: SX -> Int
ssize x = unsafePerformIO (sx_size__1 x)
{-# NOINLINE ssize #-}

ssize1 :: SX -> Int
ssize1 x = unsafePerformIO (sx_size1 x)
{-# NOINLINE ssize1 #-}

ssize2 :: SX -> Int
ssize2 x = unsafePerformIO (sx_size2 x)
{-# NOINLINE ssize2 #-}

snumel :: SX -> Int
snumel x = unsafePerformIO (sx_numel x)
{-# NOINLINE snumel #-}

svertcat :: V.Vector SX -> SX
svertcat x = unsafePerformIO (C.vertcat__2 x)
{-# NOINLINE svertcat #-}

shorzcat :: V.Vector SX -> SX
shorzcat x = unsafePerformIO (C.horzcat__2 x)
{-# NOINLINE shorzcat #-}

sveccat :: V.Vector SX -> SX
sveccat x = unsafePerformIO (C.veccat__1 x)
{-# NOINLINE sveccat #-}

svertsplit :: SX -> V.Vector Int -> V.Vector SX
svertsplit x ks = unsafePerformIO (C.vertsplit__6 x ks)
{-# NOINLINE svertsplit #-}

shorzsplit :: SX -> V.Vector Int -> V.Vector SX
shorzsplit x ks = unsafePerformIO (C.horzsplit__6 x ks)
{-# NOINLINE shorzsplit #-}

ssolve :: SX -> SX -> SX
ssolve a b = unsafePerformIO (C.solve__2 a b)
{-# NOINLINE ssolve #-}

sones :: (Int,Int) -> SX
sones (r,c) = unsafePerformIO (sx_ones__3 r c)
{-# NOINLINE sones #-}

szeros :: (Int,Int) -> SX
szeros (r,c) = unsafePerformIO (sx_zeros__3 r c)
{-# NOINLINE szeros #-}

instance Num SX where
  (+) x y = unsafePerformIO (sx___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sx___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sx___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (sx__8 (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (sx_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sx_sign x)
  {-# NOINLINE signum #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (sx___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sx__8 (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = unsafePerformIO (sx__8 (pi :: Double))
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

instance Fmod SX where
  fmod x y = unsafePerformIO (sx_fmod x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 SX where
  arctan2 x y = unsafePerformIO (sx_arctan2 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd SX where
  x `leq` y = unsafePerformIO (sx___le__ x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (sx___ge____0 x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (sx___eq__ x y)
  {-# NOINLINE eq #-}
