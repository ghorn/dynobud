{-# OPTIONS_GHC -Wall #-}

module Dyno.Casadi.Viewable ( Viewable(..), MX, SX, DMatrix ) where

import qualified Data.Vector as V

import Dyno.Casadi.SX ( SX, sveccat, svertsplit, ssize1 )
import Dyno.Casadi.MX ( MX, veccat, vertsplit, size1 )
import Dyno.Casadi.DMatrix ( DMatrix, dveccat, dvertsplit, dsize1 )

class Viewable a where
  vvecsplit :: a -> V.Vector Int -> V.Vector a
  vveccat :: V.Vector a -> a
  vsize1 :: a -> Int

instance Viewable SX where
  vveccat = sveccat
  vvecsplit = svertsplit
  vsize1 = ssize1

instance Viewable MX where
  vveccat = veccat
  vvecsplit = vertsplit
  vsize1 = size1

instance Viewable DMatrix where
  vveccat = dveccat
  vvecsplit = dvertsplit
  vsize1 = dsize1

instance Viewable (V.Vector a) where
  vsize1 = V.length
  vveccat = V.concat . V.toList
  vvecsplit x ks = V.fromList (split x (V.toList ks))

split :: V.Vector a -> [Int] -> [V.Vector a]
split _ [] = error "can't split with no input"
split x [k0] = [V.slice k0 ((V.length x) - k0) x]
split x (k0:k1:ks) = V.slice k0 (k1 - k0) x : split x (k1:ks)
