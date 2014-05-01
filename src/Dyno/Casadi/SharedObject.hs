{-# OPTIONS_GHC -Wall #-}

module Dyno.Casadi.SharedObject
       ( soInit
       ) where

import qualified Casadi.Core.Classes.SharedObject as C

soInit :: C.SharedObjectClass a => a -> IO ()
soInit = C.sharedObject_init__0
