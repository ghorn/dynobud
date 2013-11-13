{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language Rank2Types #-}
{-# Language FlexibleContexts #-}

module Hascm.Casadi.SXMatrix ( SXMatrix(), ssym, ssymV, ssymM, mm, smul, sadd, ssub, strans
                             , sgradient, sjacobian, shessian, svector
                             , sdata, ssparse, sdensify
                             , scrs ) where

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

-- | matrix matrix product
mm :: SXMatrix -> SXMatrix -> IO SXMatrix
mm = sxMatrix_mul'

-- | elementwise multiplication
smul :: SXMatrix -> SXMatrix -> IO SXMatrix
smul = sxMatrix___mul__

-- | elementwise addition
sadd :: SXMatrix -> SXMatrix -> IO SXMatrix
sadd = sxMatrix___add__

-- | elementwise addition
ssub :: SXMatrix -> SXMatrix -> IO SXMatrix
ssub = sxMatrix___sub__

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sgradient :: SXMatrix -> SXMatrix -> IO SXMatrix
sgradient = C.gradient

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sjacobian :: SXMatrix -> SXMatrix -> IO SXMatrix
sjacobian = C.jacobian

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
shessian :: SXMatrix -> SXMatrix -> IO SXMatrix
shessian = C.hessian

-- | transpose
strans :: SXMatrix -> IO SXMatrix
strans = sxMatrix_trans

-- absolute value
sabs :: SXMatrix -> IO SXMatrix
sabs = sxMatrix_fabs

-- signum
ssignum :: SXMatrix -> IO SXMatrix
ssignum = sxMatrix_sign

sdensify :: SXMatrix -> IO SXMatrix
sdensify = C.densify''

scrs :: SXMatrix -> IO CRSSparsity
scrs = sxMatrix_sparsityRef

sscalar :: Double -> IO SXMatrix
sscalar = sxMatrix''''''''''

-- | from SX vector
svector :: V.Vector SX -> IO SXMatrix
svector = sxMatrix'''''''''''

sdata :: SXMatrix -> IO (V.Vector SX)
sdata = sxMatrix_data

ssparse :: SXMatrix -> IO (V.Vector (Int,Int,SX))
ssparse sxm = do
  crs <- scrs sxm
  row <- crsSparsity_getRow crs
  col <- crsSparsity_colRef crs
  sxs <- sdata sxm
  when (V.length row /= V.length col) $ error "ssparse: row/col dimension mismatch"
  when (V.length row /= V.length sxs) $ error "ssparse: row/sxs dimension mismatch"
  return $ V.zip3 row col sxs

instance Num SXMatrix where
  (+) x y = unsafePerformIO (sadd x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (ssub x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (smul x y)
  {-# NOINLINE (*) #-}
  fromInteger = unsafePerformIO . sscalar . fromInteger
  {-# NOINLINE fromInteger #-}
  abs = unsafePerformIO . sabs
  {-# NOINLINE abs #-}
  signum = unsafePerformIO . ssignum
  {-# NOINLINE signum #-}
