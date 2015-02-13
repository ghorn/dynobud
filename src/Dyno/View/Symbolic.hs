{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.View.Symbolic
       ( Symbolic(..)
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )

import Casadi.Core.Classes.SharedObject
import Casadi.Core.Classes.Function ( Function, castFunction )
import Casadi.Core.Classes.SXFunction
import Casadi.Core.Classes.MXFunction
import Casadi.Core.Enums ( InputOutputScheme(..) )

import Casadi.CMatrix ( CMatrix(..) )
import Casadi.SX ( SX, ssymV )
import Casadi.Option ( setOption )
import Casadi.MX ( MX, symV )
import Casadi.IOSchemes

import Dyno.View.Unsafe.View ( mkJ )

import Dyno.View.View ( View(..), J )
import Dyno.View.Viewable ( Viewable(..) )

class (Viewable a, CMatrix a) => Symbolic a where
  -- | creating symbolics
  sym :: View f => String -> IO (J f a)
  mkScheme :: InputOutputScheme -> [(String,a)] -> IO (Vector a)
  mkFunction :: String -> Vector a -> Vector a -> IO Function
instance Symbolic SX where
  sym = mkSym ssymV
  mkScheme = mkSchemeSX
  mkFunction name x y = do
    f <- sxFunction__0 x y
    setOption f "name" name
    sharedObject_init__0 f
    return (castFunction f)

instance Symbolic MX where
  sym = mkSym symV
  mkScheme = mkSchemeMX
  mkFunction name x y = do
    f <- mxFunction__0 x y
    setOption f "name" name
    sharedObject_init__0 f
    return (castFunction f)

mkSym :: forall f a . (View f, Viewable a) => (String -> Int -> IO a) -> String -> IO (J f a)
mkSym vsym name = ret
  where
    ret :: IO (J f a)
    ret = fmap mkJ (vsym name n)
    n = size (Proxy :: Proxy f)
