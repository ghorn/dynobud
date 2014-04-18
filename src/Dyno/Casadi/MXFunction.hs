{-# OPTIONS_GHC -Wall #-}

module Dyno.Casadi.MXFunction
       ( C.MXFunction, mxFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Symbolic.Classes.MXFunction as C
import Dyno.Casadi.MX ( MX )

mxFunction :: Vector MX -> Vector MX -> IO C.MXFunction
mxFunction = C.mxFunction__0
