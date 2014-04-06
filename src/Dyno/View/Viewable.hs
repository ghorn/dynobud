{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}

module Dyno.View.Viewable
       ( Viewable(..), MX.MX, SX.SX, DMatrix.DMatrix, CasadiMat(..)
       ) where

import qualified Data.Vector as V

import qualified Dyno.Casadi.SX as SX
import qualified Dyno.Casadi.MX as MX
import qualified Dyno.Casadi.DMatrix as DMatrix

class Show a => Viewable a where
  vvertsplit :: a -> V.Vector Int -> V.Vector a
  vhorzsplit :: a -> V.Vector Int -> V.Vector a
  vveccat :: V.Vector a -> a
  vsize1 :: a -> Int

instance Viewable SX.SX where
  vveccat = SX.sveccat
  vvertsplit = SX.svertsplit
  vhorzsplit = SX.shorzsplit
  vsize1 = SX.ssize1

instance Viewable MX.MX where
  vveccat = MX.veccat
  vvertsplit = MX.vertsplit
  vhorzsplit = MX.horzsplit
  vsize1 = MX.size1

instance Viewable DMatrix.DMatrix where
  vveccat = DMatrix.dveccat
  vvertsplit = DMatrix.dvertsplit
  vhorzsplit = DMatrix.dhorzsplit
  vsize1 = DMatrix.dsize1

class (Viewable a) => CasadiMat a where
--  zeros :: (Int,Int) -> a
  ones :: (Int,Int) -> a
instance CasadiMat DMatrix.DMatrix where ones = DMatrix.dones
instance CasadiMat SX.SX where ones = SX.sones
instance CasadiMat MX.MX where ones = MX.ones


instance Show a => Viewable (V.Vector a) where
  vsize1 = V.length
  vveccat = V.concat . V.toList
  vvertsplit x ks = V.fromList (split x (V.toList ks))
  vhorzsplit _ _ = error "vhorzsplit not defined for Vector"

split :: V.Vector a -> [Int] -> [V.Vector a]
split v xs@(0:_) = split' v xs
split _ _ = error "split: first index must be 0"

split' :: V.Vector a -> [Int] -> [V.Vector a]
split' _ [] = error "can't split with no input"
split' x [kf]
  | V.length x == kf = []
  | otherwise = error "split: last index must be length of vector"
split' x (k0:k1:ks) = V.slice k0 (k1 - k0) x : split' x (k1:ks)

