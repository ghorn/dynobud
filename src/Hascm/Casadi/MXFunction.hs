{-# OPTIONS_GHC -Wall #-}
{-# Language KindSignatures #-}

module Hascm.Casadi.MXFunction ( toMXFunction, evalMXFun, MXFunction ) where

import qualified Data.Vector as V

import Control.Monad ( zipWithM_ )
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.MX
import qualified Casadi.Wrappers.Classes.MXFunction as C
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IOInterfaceFX
import Casadi.Wrappers.Classes.FX

import Hascm.Vectorize

newtype MXFunction (f :: * -> *) (g :: * -> *) = MXFunction C.MXFunction

toMXFunction :: (Vectorize f, Vectorize g) => f MX -> g MX -> IO (MXFunction f g)
toMXFunction inputs outputs = do
  mxf <- C.mxFunction'' (vectorize inputs) (vectorize outputs)
  sharedObject_init' mxf
  return (MXFunction mxf)

evalMXFun :: (Vectorize f, Vectorize g) => MXFunction f g -> f DMatrix -> IO (g DMatrix)
evalMXFun (MXFunction mxf) inputs = do
  -- set inputs
  zipWithM_ (ioInterfaceFX_setInput'''''' mxf) (V.toList (vectorize inputs)) [0..]
  -- eval
  fx_evaluate mxf
  -- get outputs
  numOut <- ioInterfaceFX_getNumOutputs mxf
  outputs <- mapM (ioInterfaceFX_output mxf) (take numOut [0..])
  -- return vectorized outputs
  return (devectorize (V.fromList outputs))
