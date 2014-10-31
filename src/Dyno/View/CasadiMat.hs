{-# OPTIONS_GHC -Wall #-}

module Dyno.View.CasadiMat
       ( CasadiMat(..), MX.MX, SX.SX, DMatrix.DMatrix
       ) where

import qualified Data.Vector as V

import System.IO.Unsafe ( unsafePerformIO )
import Casadi.Overloading ( Fmod, ArcTan2, SymOrd )
import qualified Casadi.SX as SX
import qualified Casadi.MX as MX
import qualified Casadi.DMatrix as DMatrix
import Casadi.Core.Tools as C

class (Eq a, Show a, Floating a, Fmod a, ArcTan2 a, SymOrd a) => CasadiMat a where
  vertsplit :: a -> V.Vector Int -> V.Vector a
  vertcat :: V.Vector a -> a
  horzsplit :: a -> V.Vector Int -> V.Vector a
  horzcat :: V.Vector a -> a
  veccat :: V.Vector a -> a
  size1 :: a -> Int
  size2 :: a -> Int
  mm :: a -> a -> a
  trans :: a -> a
  diag :: a -> a
  ones :: (Int,Int) -> a
  zeros :: (Int,Int) -> a
  fromDVector :: V.Vector Double -> a
  solve :: a -> a -> a

instance CasadiMat SX.SX where
  veccat = SX.sveccat
  vertsplit = SX.svertsplit
  vertcat = SX.svertcat
  horzsplit = SX.shorzsplit
  horzcat = SX.shorzcat
  size1 = SX.ssize1
  size2 = SX.ssize2
  mm = SX.smm
  trans = SX.strans
  diag = SX.sdiag
  ones = SX.sones
  zeros = SX.szeros
  fromDVector = SX.d2s . fromDVector
  solve = SX.ssolve

instance CasadiMat MX.MX where
  veccat = MX.veccat
  vertsplit = MX.vertsplit
  vertcat = MX.vertcat
  horzsplit = MX.horzsplit
  horzcat = MX.horzcat
  size1 = MX.size1
  size2 = MX.size2
  mm = MX.mm
  trans = MX.trans
  diag = MX.diag
  ones = MX.ones
  zeros = MX.zeros
  fromDVector = MX.d2m . fromDVector
  solve = MX.solve

instance CasadiMat DMatrix.DMatrix where
  veccat = DMatrix.dveccat
  vertsplit = DMatrix.dvertsplit
  vertcat = DMatrix.dvertcat
  horzsplit = DMatrix.dhorzsplit
  horzcat = DMatrix.dhorzcat
  size1 = DMatrix.dsize1
  size2 = DMatrix.dsize2
  mm = DMatrix.dmm
  trans = DMatrix.dtrans
  diag = DMatrix.ddiag
  ones = DMatrix.dones
  zeros = DMatrix.dzeros
  fromDVector = DMatrix.dvector
  solve x y = unsafePerformIO (C.solve__3 x y)
