{-# OPTIONS_GHC -Wall #-}

module Dyno.View.CasadiMat
       ( CasadiMat(..), MX.MX, SX.SX, DMatrix.DMatrix
       , vertslice, horzslice
       , fromDVector
       ) where

import qualified Data.Vector as V

import System.IO.Unsafe ( unsafePerformIO )
import Casadi.Overloading ( Fmod, ArcTan2, SymOrd, Erf )
import Casadi.Sparsity ( Sparsity )
import qualified Casadi.SX as SX
import qualified Casadi.MX as MX
import qualified Casadi.DMatrix as DMatrix
import Casadi.Slice ( Slice, slice )
import Casadi.Core.Tools as C

class (Eq a, Show a, Floating a, Fmod a, ArcTan2 a, SymOrd a, Erf a) => CasadiMat a where
  vertsplit :: a -> V.Vector Int -> V.Vector a
  vertcat :: V.Vector a -> a
  horzsplit :: a -> V.Vector Int -> V.Vector a
  horzcat :: V.Vector a -> a
  veccat :: V.Vector a -> a
  size1 :: a -> Int
  size2 :: a -> Int
  mm :: a -> a -> a
  innerProd :: a -> a -> a
  trans :: a -> a
  diag :: a -> a
  eye :: Int -> a
  ones :: (Int,Int) -> a
  zeros :: (Int,Int) -> a
  zerosSp :: Sparsity -> a
  solve :: a -> a -> a
  indexed :: a -> Slice -> Slice -> a
  sparsity :: a -> Sparsity
  getNZ :: a -> Slice -> a
  setNZ :: a -> a -> Slice -> IO ()
  triu :: a -> a
  tril :: a -> a
  triu2symm :: a -> a
  tril2symm :: a -> a
  copy :: a -> IO a
  dense :: a -> a
  fromDMatrix :: DMatrix.DMatrix -> a

instance CasadiMat SX.SX where
  veccat = SX.sveccat
--  vertsplit = vertslice
  vertsplit = SX.svertsplit
  vertcat = SX.svertcat
--  horzsplit = horzslice
  horzsplit = SX.shorzsplit
  horzcat = SX.shorzcat
  size1 = SX.ssize1
  size2 = SX.ssize2
  mm = SX.smm
  innerProd = SX.sinnerProd
  trans = SX.strans
  diag = SX.sdiag
  eye = SX.seye
  ones = SX.sones
  zeros = SX.szeros
  zerosSp = SX.szerosSp
  solve = SX.ssolve
  indexed = SX.sindexed
  sparsity = SX.scrs
  getNZ = SX.sgetNZ
  setNZ = SX.ssetNZ
  triu = SX.striu
  tril = SX.stril
  triu2symm = SX.striu2symm
  tril2symm = SX.stril2symm
  copy = SX.scopy
  dense = SX.sdense
  fromDMatrix = SX.d2s

instance CasadiMat MX.MX where
  veccat = MX.veccat
--  vertsplit = vertslice
  vertsplit = MX.vertsplit
  vertcat = MX.vertcat
--  horzsplit = horzslice
  horzsplit = MX.horzsplit
  horzcat = MX.horzcat
  size1 = MX.size1
  size2 = MX.size2
  mm = MX.mm
  innerProd = MX.innerProd
  trans = MX.trans
  diag = MX.diag
  eye = MX.eye
  ones = MX.ones
  zeros = MX.zeros
  zerosSp = MX.zerosSp
  solve = MX.solve
  indexed = MX.indexed
  sparsity = MX.crs
  getNZ = MX.getNZ
  setNZ = MX.setNZ
  triu = MX.triu
  tril = MX.tril
  triu2symm = MX.triu2symm
  tril2symm = MX.tril2symm
  copy = MX.copy
  dense = MX.dense
  fromDMatrix = MX.d2m

instance CasadiMat DMatrix.DMatrix where
  veccat = DMatrix.dveccat
--  vertsplit = vertslice
  vertsplit = DMatrix.dvertsplit
  vertcat = DMatrix.dvertcat
--  horzsplit = horzslice
  horzsplit = DMatrix.dhorzsplit
  horzcat = DMatrix.dhorzcat
  size1 = DMatrix.dsize1
  size2 = DMatrix.dsize2
  mm = DMatrix.dmm
  innerProd = DMatrix.dinnerProd
  trans = DMatrix.dtrans
  diag = DMatrix.ddiag
  eye = DMatrix.deye
  ones = DMatrix.dones
  zeros = DMatrix.dzeros
  zerosSp = DMatrix.dzerosSp
  solve x y = unsafePerformIO (C.solve__3 x y)
  indexed = DMatrix.dindexed
  sparsity = DMatrix.dcrs
  getNZ = DMatrix.dgetNZ
  setNZ = DMatrix.dsetNZ
  triu = DMatrix.dtriu
  tril = DMatrix.dtril
  triu2symm = DMatrix.dtriu2symm
  tril2symm = DMatrix.dtril2symm
  copy = DMatrix.dcopy
  dense = DMatrix.ddense
  fromDMatrix = id

fromDVector :: CasadiMat a => V.Vector Double -> a
fromDVector = fromDMatrix . DMatrix.dvector

vertslice :: CasadiMat a => a -> V.Vector Int -> V.Vector a
vertslice x vs = V.fromList (f (V.toList vs))
  where
    cols = size2 x
    hslice = slice 0 cols 1

    f (v0:v1:others) = indexed x (slice v0 v1 1) hslice : f (v1:others)
    f _ = []

horzslice :: CasadiMat a => a -> V.Vector Int -> V.Vector a
horzslice x vs = V.fromList (f (V.toList vs))
  where
    rows = size1 x
    vslice = slice 0 rows 1

    f (v0:v1:others) = indexed x vslice (slice v0 v1 1) : f (v1:others)
    f _ = []
