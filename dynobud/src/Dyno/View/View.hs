{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Dyno.View.View
       ( View(..), JV
       , J, S
       , JTuple(..), JTriple(..), JQuad(..)
       , jfill
       , v2d, d2v
       , fmapJ, unzipJ
       , splitJV, catJV
       ) where

import GHC.Generics ( Generic )

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import qualified Casadi.DM as DM
import qualified Casadi.Matrix as CM

import Dyno.View.Unsafe ( View(..), J, S, JV, mkM, unM, catJV, splitJV )

-- some helper types
data JTuple f g a = JTuple (J f a) (J g a) deriving ( Generic, Show )
data JTriple f g h a = JTriple (J f a) (J g a) (J h a) deriving ( Generic, Show )
data JQuad f0 f1 f2 f3 a = JQuad (J f0 a) (J f1 a) (J f2 a) (J f3 a) deriving ( Generic, Show )
instance (View f, View g) => View (JTuple f g)
instance (View f, View g, View h) => View (JTriple f g h)
instance (View f0, View f1, View f2, View f3) => View (JQuad f0 f1 f2 f3)

jfill :: forall a f . View f => a -> J f (Vector a)
jfill x = mkM (V.replicate n x)
  where
    n = size (Proxy :: Proxy f)

v2d :: View f => J f (V.Vector Double) -> J f DM.DM
v2d = mkM . CM.fromDVector . unM

d2v :: View f => J f DM.DM -> J f (V.Vector Double)
d2v = mkM . DM.dnonzeros . CM.densify . unM

fmapJ :: View f => (a -> b) -> J f (Vector a) -> J f (Vector b)
fmapJ f = mkM . V.map f . unM

unzipJ :: View f => J f (Vector (a,b)) -> (J f (Vector a), J f (Vector b))
unzipJ v = (mkM x, mkM y)
  where
    (x,y) = V.unzip (unM v)
