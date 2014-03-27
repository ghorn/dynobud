{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.View.Symbolic
       ( Symbolic(..)
       , Matrix(..)
       , MX
       , SX
       , DMatrix.DMatrix
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )

import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.Function ( Function, castFunction )
import Casadi.Wrappers.Classes.SXFunction ( sxFunction''' )
import Casadi.Wrappers.Classes.MXFunction ( mxFunction'' )
import Casadi.Wrappers.Enums ( InputOutputScheme(..) )

import Dyno.View.View ( View(..), J, mkJ )
import Dyno.View.Viewable ( Viewable(..) )
import Dyno.Casadi.SX ( SX, ssymV )
import Dyno.Casadi.MX ( MX, symV )
import qualified Dyno.Casadi.SX as SX
import qualified Dyno.Casadi.MX as MX
import qualified Dyno.Casadi.DMatrix as DMatrix
import Dyno.Casadi.IOSchemes


class (Show a, Viewable a) => Symbolic a where
  -- | creating symbolics
  sym :: View f => String -> IO (J f a)
  mkScheme :: InputOutputScheme -> [(String,a)] -> IO (Vector a)
  mkFunction :: Vector a -> Vector a -> IO Function
instance Symbolic SX where
  sym = mkSym ssymV
  mkScheme = mkSchemeSX
  mkFunction x y = do
    f <- sxFunction''' x y
    sharedObject_init' f
    return (castFunction f)

instance Symbolic MX where
  sym = mkSym symV
  mkScheme = mkSchemeMX
  mkFunction x y = do
    f <- mxFunction'' x y
    sharedObject_init' f
    return (castFunction f)


class Matrix a where
  diag :: a -> a
instance Matrix DMatrix.DMatrix where
  diag = DMatrix.ddiag
instance Matrix SX where
  diag = SX.sdiag
instance Matrix MX where
  diag = MX.diag


mkSym :: forall f a . (View f, Viewable a) => (String -> Int -> IO a) -> String -> IO (J f a)
mkSym vsym name = ret
  where
    ret :: IO (J f a)
    ret = fmap mkJ (vsym name n)
    n = size (Proxy :: Proxy f)
