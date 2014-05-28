{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}

module Dyno.Casadi.Overloading
       ( Fmod(..)
       , ArcTan2(..)
       , SymOrd(..)
       ) where

import Data.Fixed ( mod' )

-- | doesn't require Real, used for overloading symbolics
class Fmod a where
  fmod :: a -> a -> a

instance Real a => Fmod a where
  fmod = mod'

-- | doesn't require RealFloat, used for overloading symbolics
class ArcTan2 a where
  arctan2 :: a -> a -> a

instance RealFloat a => ArcTan2 a where
  arctan2 = atan2

-- | Ord, but returns a 1 or a 0 instead of True or False
class SymOrd a where
  leq :: a -> a -> a
  geq :: a -> a -> a
  eq :: a -> a -> a

instance (Num a, Ord a) => SymOrd a where
  x `leq` y = if x <= y then 1 else 0
  x `geq` y = if x >= y then 1 else 0
  x  `eq` y = if x == y then 1 else 0
