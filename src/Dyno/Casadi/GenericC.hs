{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module Dyno.Casadi.GenericC
       ( GenericC(..)
       ) where

import Data.Vector ( Vector )
import Casadi.Core.Classes.Function ( Function )
import Casadi.Core.Classes.GenericType

class GenericC a where
  mkGeneric :: a -> IO GenericType
  fromGeneric :: GenericType -> IO (Maybe a)

instance GenericC Bool where
  mkGeneric = genericType__11
  fromGeneric = ifThenGet genericType_isBool genericType_toBool
instance GenericC Int where
  mkGeneric = genericType__10
  fromGeneric = ifThenGet genericType_isInt genericType_toInt
instance GenericC Double where
  mkGeneric = genericType__9
  fromGeneric = ifThenGet genericType_isDouble genericType_toDouble
instance GenericC String where
  mkGeneric = genericType__8
  fromGeneric = ifThenGet genericType_isString genericType_toString
instance GenericC (Vector Bool) where
  mkGeneric = genericType__7
  fromGeneric = const (return Nothing)
instance GenericC (Vector Int) where
  mkGeneric = genericType__6
  fromGeneric = ifThenGet genericType_isIntVector genericType_toIntVector
instance GenericC (Vector Double) where
  mkGeneric = genericType__5
  fromGeneric = ifThenGet genericType_isDoubleVector genericType_toDoubleVector
instance GenericC (Vector String) where
  mkGeneric = genericType__4
  fromGeneric = ifThenGet genericType_isStringVector genericType_toStringVector
instance GenericC GenericType where
  mkGeneric = return
  fromGeneric = return . Just
instance GenericC Function where
  mkGeneric = genericType__3
  fromGeneric = ifThenGet genericType_isFunction genericType_toFunction

ifThenGet :: (a -> IO Bool) -> (a -> IO b) -> a -> IO (Maybe b)
ifThenGet isOpt getOpt g = do
  isopt <- isOpt g
  if isopt
    then fmap Just (getOpt g)
    else return Nothing
