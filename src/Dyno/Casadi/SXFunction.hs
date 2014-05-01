{-# OPTIONS_GHC -Wall #-}

module Dyno.Casadi.SXFunction
       ( C.SXFunction, sxFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Core.Classes.SXFunction as C
import Dyno.Casadi.SX ( SX )

sxFunction :: Vector SX -> Vector SX -> IO C.SXFunction
sxFunction = C.sxFunction__0
