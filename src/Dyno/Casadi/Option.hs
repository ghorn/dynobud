{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Dyno.Casadi.Option
       ( Opt(..)
       , GenericC(..)
       , setOption
       , getOption
       ) where

import Casadi.Core.Classes.OptionsFunctionality
  ( OptionsFunctionalityClass, optionsFunctionality_setOption,
    optionsFunctionality_hasOption, optionsFunctionality_getOption)
import Dyno.Casadi.GenericC

data Opt where
  Opt :: GenericC a => a -> Opt

setOption :: (OptionsFunctionalityClass a, GenericC b) => a -> String -> b -> IO ()
setOption f name val = do
  gval <- mkGeneric val
  optionsFunctionality_setOption f name gval

getOption :: (OptionsFunctionalityClass a, GenericC b) => a -> String -> IO (Maybe b)
getOption f name = do
  has <- optionsFunctionality_hasOption f name
  if has
    then optionsFunctionality_getOption f name >>= fromGeneric
    else return Nothing

