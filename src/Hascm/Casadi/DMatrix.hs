{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hascm.Casadi.DMatrix ( DMatrix(), dcrs, dsparse, dmm, dscalar, dvector, ddata
                            , ddensify, dtrans
                            ) where

import Control.Monad ( when )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Wrappers.Classes.CRSSparsity
import Casadi.Wrappers.Classes.DMatrix
import qualified Casadi.Wrappers.Tools as C

-- matrix matrix product
dmm :: DMatrix -> DMatrix -> DMatrix
dmm x = unsafePerformIO . dmatrix_mul' x
{-# NOINLINE dmm #-}

dscalar :: Double -> DMatrix
dscalar = unsafePerformIO . dmatrix''''''''''
{-# NOINLINE dscalar #-}

dvector :: V.Vector Double -> DMatrix
dvector = unsafePerformIO . dmatrix'''''''''''
{-# NOINLINE dvector #-}

ddensify :: DMatrix -> DMatrix
ddensify = unsafePerformIO . C.densify'
{-# NOINLINE ddensify #-}

dcrs :: DMatrix -> CRSSparsity
dcrs = unsafePerformIO . dmatrix_sparsityRef
{-# NOINLINE dcrs #-}

ddata :: DMatrix -> V.Vector Double
ddata = unsafePerformIO . dmatrix_data
{-# NOINLINE ddata #-}

-- | transpose
dtrans :: DMatrix -> DMatrix
dtrans = unsafePerformIO . dmatrix_trans
{-# NOINLINE dtrans #-}

dsparse :: DMatrix -> V.Vector (Int,Int,Double)
dsparse dmat = unsafePerformIO $ do
  let crs = dcrs dmat
  row <- crsSparsity_getRow crs
  col <- crsSparsity_colRef crs
  let vals = ddata dmat
  when (V.length row /= V.length col)  $ error "dsparse: row/col dimension mismatch"
  when (V.length row /= V.length vals) $ error "dsparse: row/vals dimension mismatch"
  return $ V.zip3 row col vals
{-# NOINLINE dsparse #-}

instance Num DMatrix where
  (+) x y = unsafePerformIO (dmatrix___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (dmatrix___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (dmatrix___mul__ x y)
  {-# NOINLINE (*) #-}
  fromInteger = dscalar . fromInteger
  {-# NOINLINE fromInteger #-}
  abs = unsafePerformIO . dmatrix_fabs
  {-# NOINLINE abs #-}
  signum = unsafePerformIO . dmatrix_sign
  {-# NOINLINE signum #-}
