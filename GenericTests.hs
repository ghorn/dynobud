{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module GenericTests where

import Data.TypeLevel.Num.Sets
import GHC.Generics

import Vectorize ( Vectorize(..) )
import TypeVecs

data ExplEulerMsDvs x u nx nu a =
  ExplEulerMsDvs
    { eeX :: Vec nx (x a)
    , eeU :: Vec nu (u a)
    } deriving (Functor, Generic1, Show)

data ExplEulerMsConstraints x n nbc npc a =
  ExplEulerMsConstraints
  { ecBc :: Vec nbc a
  , ecPathc :: Vec n (Vec npc a)
  , ecDynamics :: Vec n (x a)
  } deriving (Functor, Generic1, Show)


data Spring a = Spring { pos :: Xyz a
                       , vel :: Xyz a
                       } deriving (Generic1, Functor, Show)
data One f a = MkOne (f a) deriving (Generic1, Functor, Show)
data Pair f g a = MkPair (f a) (g a) deriving (Generic1, Functor, Show)
data AFew f g h a = MkAFew (f a) (g a) (h a) deriving (Generic1, Functor, Show)
data None a = None deriving (Generic1, Functor, Show)
instance Vectorize None

instance (Vectorize f) => Vectorize (One f)
instance Vectorize Spring
instance (Vectorize f0, Vectorize f1) => Vectorize (Pair f0 f1)
instance (Vectorize f0, Vectorize f1, Vectorize f2) => Vectorize (AFew f0 f1 f2)
instance (Vectorize x, Vectorize u, Nat nx, Nat nu) => Vectorize (ExplEulerMsDvs x u nx nu)
instance (Vectorize x, Nat n, Nat nbc, Nat npc) => Vectorize (ExplEulerMsConstraints x n nbc npc)

data Xyz a = Xyz a a a deriving (Functor, Generic1, Show)
instance Vectorize Xyz
data Test f a = MkTest (Xyz (f a)) deriving (Functor, Generic1)
instance (Show (f a), Show a) => Show (Test f a) where
  show (MkTest xyzs) = "MkTest " ++ show xyzs
instance Vectorize f => Vectorize (Test f)

main :: IO ()
main = do
  print (empty :: One Xyz ())
  print (empty :: Test Xyz ())
  print (empty :: Test Xyz ())
