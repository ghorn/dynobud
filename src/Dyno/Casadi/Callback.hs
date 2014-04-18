{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Dyno.Casadi.Callback
       ( makeCallback
       ) where

import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( newForeignPtr_ )

import Casadi.Symbolic.Data
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
import Casadi.Internal.Callback ( mkCallback, c_newCallbackHaskell )

-- | add a callback to an NLPSolver
makeCallback :: (Function -> IO CInt) -> IO Callback
makeCallback callback = do
  -- safely wrap the callback into the C-friendly version
  let callback' :: Ptr Function' -> IO CInt
      callback' ptrFx = do
        foreignCFun <- newForeignPtr_ ptrFx
        callback (Function foreignCFun)

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCallback callback'

  -- create the callback object
  (c_newCallbackHaskell callbackFunPtr :: IO (Ptr Callback')) >>= wrapReturn
