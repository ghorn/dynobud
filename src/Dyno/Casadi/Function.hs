{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.Function
       ( C.MXFunction, C.Function, callMX, callSX, evalDMatrix
       , jacobian, gradient, derivative
       , generateCode, externalFunction
       ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad ( zipWithM_ )

import qualified Casadi.Core.Classes.MXFunction as C
import qualified Casadi.Core.Classes.IOInterfaceFunction as C
import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.ExternalFunction as C

import Dyno.Casadi.SX ( SX )
import Dyno.Casadi.MX ( MX )
import Dyno.Casadi.DMatrix ( DMatrix )

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMX :: C.FunctionClass f => f -> Vector MX -> Vector MX
callMX f ins = unsafePerformIO (C.function_call__0 f ins)
{-# NOINLINE callMX #-}

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: C.FunctionClass f => f -> Vector SX -> Vector SX
callSX f ins = unsafePerformIO (C.function_call__3 f ins)
{-# NOINLINE callSX #-}

-- | evaluate an SXFunction with 1 input and 1 output
evalDMatrix :: (C.FunctionClass f, C.IOInterfaceFunctionClass f)
               => f -> Vector DMatrix -> IO (Vector DMatrix)
evalDMatrix sxf inputs = do
  -- set inputs
  zipWithM_ (C.ioInterfaceFunction_setInput__2 sxf) (V.toList inputs) [0..]

  -- eval
  C.function_evaluate sxf

  -- get outputs
  numOut <- C.ioInterfaceFunction_getNumOutputs sxf
  outputs <- mapM (C.ioInterfaceFunction_output__2 sxf) (take numOut [0..])

  -- return vectorized outputs
  return (V.fromList outputs)

jacobian :: C.FunctionClass a => a -> Int -> Int -> Bool -> Bool -> IO C.Function
jacobian = C.function_jacobian__14

gradient :: C.FunctionClass a => a -> Int -> Int -> IO C.Function
gradient = C.function_gradient__6

derivative :: C.FunctionClass a => a -> Int -> Int -> IO C.Function
derivative = C.function_derivative

generateCode :: C.FunctionClass a => a -> String
generateCode f = unsafePerformIO (C.function_generateCode__0 f)
{-# NOINLINE generateCode #-}

externalFunction :: String -> IO C.Function
externalFunction name = fmap C.castFunction $ C.externalFunction__0 name
