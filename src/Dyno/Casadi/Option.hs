{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

module Dyno.Casadi.Option
       ( Opt(..)
       , Option
       , setOption
       , getOption
       ) where

import Data.Vector ( Vector )
import Casadi.Core.Classes.Function ( Function )
import Casadi.Core.Classes.GenericType
import Casadi.Core.Classes.OptionsFunctionality
  ( OptionsFunctionalityClass, optionsFunctionality_setOption,
    optionsFunctionality_hasOption, optionsFunctionality_getOption)

data Opt where
  Opt :: Option a => a -> Opt

setOption :: (OptionsFunctionalityClass a, Option b) => a -> String -> b -> IO ()
setOption f name val = do
  gval <- mkGeneric val
  optionsFunctionality_setOption f name gval

getOption :: (OptionsFunctionalityClass a, Option b) => a -> String -> IO (Maybe b)
getOption f name = do
  has <- optionsFunctionality_hasOption f name
  if has
    then optionsFunctionality_getOption f name >>= fromGeneric
    else return Nothing

class Option a where
  mkGeneric :: a -> IO GenericType
  fromGeneric :: GenericType -> IO (Maybe a)

instance Option Bool where
  mkGeneric = genericType__11
  fromGeneric = ifThenGet genericType_isBool genericType_toBool
instance Option Int where
  mkGeneric = genericType__10
  fromGeneric = ifThenGet genericType_isInt genericType_toInt
instance Option Double where
  mkGeneric = genericType__9
  fromGeneric = ifThenGet genericType_isDouble genericType_toDouble
instance Option String where
  mkGeneric = genericType__8
  fromGeneric = ifThenGet genericType_isString genericType_toString
instance Option (Vector Bool) where
  mkGeneric = genericType__7
  fromGeneric = const (return Nothing)
instance Option (Vector Int) where
  mkGeneric = genericType__6
  fromGeneric = ifThenGet genericType_isIntVector genericType_toIntVector
instance Option (Vector Double) where
  mkGeneric = genericType__5
  fromGeneric = ifThenGet genericType_isDoubleVector genericType_toDoubleVector
instance Option (Vector String) where
  mkGeneric = genericType__4
  fromGeneric = ifThenGet genericType_isStringVector genericType_toStringVector
instance Option GenericType where
  mkGeneric = return
  fromGeneric = return . Just
instance Option Function where
  mkGeneric = genericType__3
  fromGeneric = ifThenGet genericType_isFunction genericType_toFunction

ifThenGet :: (a -> IO Bool) -> (a -> IO b) -> a -> IO (Maybe b)
ifThenGet isOpt getOpt g = do
  isopt <- isOpt g
  if isopt
    then fmap Just (getOpt g)
    else return Nothing
