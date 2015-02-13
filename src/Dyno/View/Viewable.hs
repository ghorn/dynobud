{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}

module Dyno.View.Viewable
       ( Viewable(..)
       ) where

import qualified Data.Vector as V

import qualified Casadi.SX as SX
import qualified Casadi.MX as MX
import qualified Casadi.DMatrix as DMatrix
import qualified Casadi.CMatrix as CM

class Viewable a where
  vvertsplit :: a -> V.Vector Int -> V.Vector a
  vhorzsplit :: a -> V.Vector Int -> V.Vector a
  vveccat :: V.Vector a -> a
  vsize1 :: a -> Int
  vsize2 :: a -> Int
  vrecoverDimension :: a -> Int -> a

instance Viewable SX.SX where
  vveccat = CM.veccat
  vvertsplit = CM.vertsplit
  vhorzsplit = CM.horzsplit
  vsize1 = CM.size1
  vsize2 = CM.size2
  vrecoverDimension _ k = CM.zeros (k,1)

instance Viewable MX.MX where
  vveccat = CM.veccat
  vvertsplit = CM.vertsplit
  vhorzsplit = CM.horzsplit
  vsize1 = CM.size1
  vsize2 = CM.size2
  vrecoverDimension _ k = CM.zeros (k,1)

instance Viewable DMatrix.DMatrix where
  vveccat = CM.veccat
  vvertsplit = CM.vertsplit
  vhorzsplit = CM.horzsplit
  vsize1 = CM.size1
  vsize2 = CM.size2
  vrecoverDimension _ k = CM.zeros (k,1)

--instance CM.CasadiMat a => Viewable a where
--  vveccat = CM.veccat
--  vvertsplit = CM.vertsplit
--  vhorzsplit = CM.horzsplit
--  vsize1 x
--    | CM.size2 x == 1 = CM.size1 x
--    | otherwise = error "Dyno.View.Viewable(vsize1): not a column!!"

instance Viewable (V.Vector a) where
  vsize1 = V.length
  vsize2 = const 1
  vveccat = V.concat . V.toList
  vvertsplit x ks = V.fromList (split x (V.toList ks))
  vhorzsplit _ _ = error "vhorzsplit not defined for Vector"
  vrecoverDimension x _ = x

split :: V.Vector a -> [Int] -> [V.Vector a]
split v xs@(0:_) = split' v xs
split _ _ = error "split: first index must be 0"

split' :: V.Vector a -> [Int] -> [V.Vector a]
split' _ [] = error "can't split with no input"
split' x [kf]
  | V.length x == kf = []
  | otherwise = error "split: last index must be length of vector"
split' x (k0:k1:ks) = V.slice k0 (k1 - k0) x : split' x (k1:ks)

