{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE KindSignatures #-}

module Dyno.Casadi.MXVData ( MXV(..), MX, veccat, vertsplit, size1 ) where

import Data.Vector ( Vector)
import GHC.Generics

--import Dyno.Casadi.MX ( MX, veccat, vertsplit )
import Dyno.Casadi.SX ( SX, sveccat, svertsplit, ssize1 )

type MX = SX

veccat :: Vector SX -> SX
veccat = sveccat

vertsplit :: SX -> Vector Int -> Vector SX
vertsplit = svertsplit

size1 :: SX -> Int
size1 = ssize1

--data MXV (f :: k -> *) = MXV MX deriving (Generic, Show)
--data MXV (f :: (* -> *) -> *) = MXV MX deriving Generic
data MXV f = MXV { unMXV :: MX } deriving Generic

instance Show (MXV f) where
  showsPrec p (MXV x) = showsPrec p x
