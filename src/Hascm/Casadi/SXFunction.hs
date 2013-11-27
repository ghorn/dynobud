{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}

module Hascm.Casadi.SXFunction ( toSXFunction, evalSXFun, SXFunction ) where

import qualified Data.Vector as V

import Control.Monad ( zipWithM_ )
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.SXMatrix
import qualified Casadi.Wrappers.Classes.SXFunction as C
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IOInterfaceFX
import Casadi.Wrappers.Classes.FX

import Hascm.Vectorize

newtype SXFunction f g = SXFunction C.SXFunction

toSXFunction :: (Vectorize f, Vectorize g) => f SXMatrix -> g SXMatrix -> IO (SXFunction f g)
toSXFunction inputs outputs = do
  sxf <- C.sxFunction''' (vectorize inputs) (vectorize outputs)
  sharedObject_init sxf
  return (SXFunction sxf)

evalSXFun :: (Vectorize f, Vectorize g) => SXFunction f g -> f DMatrix -> IO (g DMatrix)
evalSXFun (SXFunction sxf) inputs = do
  -- set inputs
  zipWithM_ (ioInterfaceFX_setInput'''''' sxf) (V.toList (vectorize inputs)) [0..]
  -- eval
  fx_evaluate sxf
  -- get outputs
  numOut <- ioInterfaceFX_getNumOutputs sxf
  outputs <- mapM (ioInterfaceFX_output sxf) (take numOut [0..])
  -- return vectorized outputs
  return (devectorize (V.fromList outputs))
