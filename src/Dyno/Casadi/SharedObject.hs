{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Dyno.Casadi.SharedObject
       ( soInit
       ) where

import qualified Casadi.Wrappers.Classes.SharedObject as C

soInit :: C.SharedObjectClass a => a -> IO ()
soInit = C.sharedObject_init'
