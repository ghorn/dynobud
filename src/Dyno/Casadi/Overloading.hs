{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}

module Dyno.Casadi.Overloading
       ( Fmod(..)
       , ArcTan2(..)
       ) where

import Data.Fixed ( mod' )

class Fmod a where
  fmod :: a -> a -> a

instance Real a => Fmod a where
  fmod = mod'

class ArcTan2 a where
  arctan2 :: a -> a -> a

instance RealFloat a => ArcTan2 a where
  arctan2 = atan2
