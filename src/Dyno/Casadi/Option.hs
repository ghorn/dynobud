{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

module Dyno.Casadi.Option
       ( Opt(..)
       , Option
       , setOption
       ) where

import Data.Vector ( Vector )
import Casadi.Wrappers.Classes.GenericType
import Casadi.Wrappers.Classes.OptionsFunctionality
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
  mkGeneric = genericType'
instance Option Int where
  mkGeneric = genericType''
instance Option Double where
  mkGeneric = genericType'''
instance Option String where
  mkGeneric = genericType''''
instance Option (Vector Bool) where
  mkGeneric = genericType'''''
instance Option (Vector Int) where
  mkGeneric = genericType''''''
instance Option (Vector Double) where
  mkGeneric = genericType'''''''
instance Option (Vector String) where
  mkGeneric = genericType''''''''
instance Option GenericType where
  mkGeneric = return
