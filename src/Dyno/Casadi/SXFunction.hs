{-# OPTIONS_GHC -Wall #-}

module Dyno.Casadi.SXFunction
       ( C.SXFunction, sxFunction, callSX, evalDMatrix
       ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad ( zipWithM_ )

import Casadi.Wrappers.Classes.SX ( SX )
import Casadi.Wrappers.Classes.DMatrix ( DMatrix )
import qualified Casadi.Wrappers.Classes.SXFunction as C
import qualified Casadi.Wrappers.Classes.SharedObject as C
import qualified Casadi.Wrappers.Classes.IOInterfaceFX as C
import qualified Casadi.Wrappers.Classes.FX as C

sxFunction :: Vector SX -> Vector SX -> C.SXFunction
sxFunction inputs outputs = unsafePerformIO $ do
  sxf <- C.sxFunction''' inputs outputs
  C.sharedObject_init' sxf
  return sxf
{-# NOINLINE sxFunction #-}

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: C.FXClass f => f -> Vector SX -> Vector SX
callSX f ins = unsafePerformIO (C.fx_call''''' f ins)
{-# NOINLINE callSX #-}

-- | evaluate an SXFunction with 1 input and 1 output
evalDMatrix :: (C.FXClass f, C.IOInterfaceFXClass f) => f -> Vector DMatrix -> IO (Vector DMatrix)
evalDMatrix sxf inputs = do
  -- set inputs
  zipWithM_ (C.ioInterfaceFX_setInput'''''' sxf) (V.toList inputs) [0..]

  -- eval
  C.fx_evaluate sxf

  -- get outputs
  numOut <- C.ioInterfaceFX_getNumOutputs sxf
  outputs <- mapM (C.ioInterfaceFX_output sxf) (take numOut [0..])

  -- return vectorized outputs
  return (V.fromList outputs)
