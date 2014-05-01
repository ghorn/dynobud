{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

module Dyno.Casadi.Option
       ( Opt(..)
       , Option
       , setOption
       ) where

import Data.Vector ( Vector )
import Casadi.Core.Classes.Function ( Function )
import Casadi.Core.Classes.GenericType
import Casadi.Core.Classes.OptionsFunctionality
  ( OptionsFunctionalityClass, optionsFunctionality_setOption )

data Opt where
  Opt :: Option a => a -> Opt

setOption :: (OptionsFunctionalityClass a, Option b) => a -> String -> b -> IO ()
setOption f name val = do
  gval <- mkGeneric val
  optionsFunctionality_setOption f name gval

class Option a where
  mkGeneric :: a -> IO GenericType

instance Option Bool where
  mkGeneric = genericType__11
instance Option Int where
  mkGeneric = genericType__10
instance Option Double where
  mkGeneric = genericType__9
instance Option String where
  mkGeneric = genericType__8
instance Option (Vector Bool) where
  mkGeneric = genericType__7
instance Option (Vector Int) where
  mkGeneric = genericType__6
instance Option (Vector Double) where
  mkGeneric = genericType__5
instance Option (Vector String) where
  mkGeneric = genericType__4
instance Option GenericType where
  mkGeneric = return
instance Option Function where
  mkGeneric = genericType__3
