{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Dyno.TestMX where

import Linear.V ( Dim(..) )
import GHC.Generics ( Generic )
import Data.Proxy
import Dyno.Casadi.V
import Dyno.Casadi.MX ( MX )
import Dyno.Casadi.SX ( SX )

data Id a = Id (V a S) deriving ( Generic )
data Xy a = Xy (V a S) (V a S) deriving ( Generic )
data Xyz a = Xyz (V a S) (V a S) (V a S) deriving ( Generic )
data Fctr f a = Fctr (V a f) deriving ( Generic, Show )
data Tuple f g a = Tuple (V a f) (V a g) deriving ( Generic, Show )

instance View Id
instance View Xy
instance View Xyz
instance View f => View (Fctr f)
instance (View f, View g) => View (Tuple f g)

proxy :: a -> Proxy a
proxy = const Proxy


---- design variables
data CollStage x z u deg a = CollStage (V a x) (V a (Vec deg (CollPoint x z u)))
                           deriving (Generic, Show)
instance (View x, View z, View u, Dim deg) => View (CollStage x z u deg)

data CollPoint x z u a = CollPoint (V a x) (V a z) (V a u) deriving (Generic, Show)
instance (View x, View z, View u) => View (CollPoint x z u)

data D2
instance Dim D2 where
  reflectDim = const 2


go :: IO ()
go = do
  woo <- sym "x" :: IO (V MX (CollPoint (Tuple (Tuple Xy S) Xyz) Xyz Xy)) -- Tuple (Fctr Xy) (Fctr Id)))
  print woo
  let oow@(CollPoint xp yp zp) = split woo
  print oow
  print xp
  print yp
  print zp
  let wootoo = cat oow
  print wootoo
  return ()
