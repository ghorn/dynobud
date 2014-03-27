{-# OPTIONS_GHC -Wall #-}

module Dyno.Casadi.SXFunction
       ( C.SXFunction, sxFunction, callSX, evalDMatrix
       ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad ( zipWithM_ )

import qualified Casadi.Wrappers.Classes.SXFunction as C
import qualified Casadi.Wrappers.Classes.IOInterfaceFunction as C
import qualified Casadi.Wrappers.Classes.Function as C

import Dyno.Casadi.SX ( SX )
import Dyno.Casadi.DMatrix ( DMatrix )

sxFunction :: Vector SX -> Vector SX -> IO C.SXFunction
sxFunction inputs outputs = C.sxFunction''' inputs outputs
{-# NOINLINE sxFunction #-}

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: C.FunctionClass f => f -> Vector SX -> Vector SX
callSX f ins = unsafePerformIO (C.function_call''''' f ins)
{-# NOINLINE callSX #-}

-- | evaluate an SXFunction with 1 input and 1 output
evalDMatrix :: (C.FunctionClass f, C.IOInterfaceFunctionClass f)
               => f -> Vector DMatrix -> IO (Vector DMatrix)
evalDMatrix sxf inputs = do
  -- set inputs
  zipWithM_ (C.ioInterfaceFunction_setInput'''''' sxf) (V.toList inputs) [0..]

  -- eval
  C.function_evaluate sxf

  -- get outputs
  numOut <- C.ioInterfaceFunction_getNumOutputs sxf
  outputs <- mapM (C.ioInterfaceFunction_output sxf) (take numOut [0..])

  -- return vectorized outputs
  return (V.fromList outputs)
