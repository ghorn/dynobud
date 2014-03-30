{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.MXFunction
       ( C.MXFunction, mxFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Wrappers.Classes.MXFunction as C
import Dyno.Casadi.MX ( MX )

mxFunction :: Vector MX -> Vector MX -> IO C.MXFunction
mxFunction inputs outputs = C.mxFunction'' inputs outputs
{-# NOINLINE mxFunction #-}
