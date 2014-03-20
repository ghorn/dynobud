{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE KindSignatures #-}

module Dyno.Casadi.MXVData ( MXV(..), MX, veccat, vertsplit, size1, symV ) where

import Data.Vector ( Vector)
import GHC.Generics

--import Dyno.Casadi.MX ( MX, veccat, vertsplit )
import Dyno.Casadi.SX ( SX, sveccat, svertsplit, ssize1, ssymV )

type MX = SX

veccat :: Vector MX -> MX
veccat = sveccat

vertsplit :: MX -> Vector Int -> Vector MX
vertsplit = svertsplit

size1 :: MX -> Int
size1 = ssize1

symV :: String -> Int -> IO SX
symV = ssymV

data MXV f = MXV { unMXV :: MX } deriving Generic

instance Show (MXV f) where
  showsPrec p (MXV x) = showsPrec p x
