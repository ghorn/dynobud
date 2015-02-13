{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Dyno.View.Unsafe.M
       ( M(..)
       , mkM
       , mkM'
       ) where

import Data.Proxy
import GHC.Generics ( Generic )

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..) )
import Casadi.CMatrix ( CMatrix )
import qualified Casadi.CMatrix as CM

import Dyno.View.View ( View(..) )

newtype M (f :: * -> *) (g :: * -> *) (a :: *) =
  UnsafeM { unM :: a } deriving (Eq, Functor, Generic)

instance Show a => Show (M f g a) where
  showsPrec p (UnsafeM x) = showsPrec p x

over :: (View f, View g, CMatrix a) => (a -> a) -> M f g a -> M f g a
over f (UnsafeM x) = mkM (f x)

over2 :: (View f, View g, CMatrix a) => (a -> a -> a) -> M f g a -> M f g a -> M f g a
over2 f (UnsafeM x) (UnsafeM y)= mkM (f x y)

instance (View f, View g, CMatrix a) => Num (M f g a) where
  (+) = over2 (+)
  (-) = over2 (-)
  (*) = over2 (*)
  negate = over negate
  abs = over abs
  signum = over signum
  fromInteger k = mkM $ fromInteger k * CM.ones (nx,ny)
    where
      nx = size (Proxy :: Proxy f)
      ny = size (Proxy :: Proxy g)
instance (View f, View g, CMatrix a) => Fractional (M f g a) where
  (/) = over2 (/)
  fromRational k = mkM $ fromRational k * CM.ones (nx,ny)
    where
      nx = size (Proxy :: Proxy f)
      ny = size (Proxy :: Proxy g)
instance (View f, View g, CMatrix a) => Floating (M f g a) where
  pi = mkM $ pi * CM.ones (nx,ny)
    where
      nx = size (Proxy :: Proxy f)
      ny = size (Proxy :: Proxy g)
  (**) = over2 (**)
  exp   = over exp
  log   = over log
  sin   = over sin
  cos   = over cos
  tan   = over tan
  asin  = over asin
  atan  = over atan
  acos  = over acos
  sinh  = over sinh
  cosh  = over cosh
  tanh  = over tanh
  asinh = over asinh
  atanh = over atanh
  acosh = over acosh

instance (View f, View g, CMatrix a) => Fmod (M f g a) where
  fmod = over2 fmod

instance (View f, View g, CMatrix a) => ArcTan2 (M f g a) where
  arctan2 = over2 arctan2

instance (View f, View g, CMatrix a) => SymOrd (M f g a) where
  leq = over2 leq
  geq = over2 geq
  eq  = over2 eq

mkM :: forall f g a . (View f, View g, CMatrix a) => a -> M f g a
mkM x = case mkM' x of
  Right x' -> x'
  Left msg -> error msg

mkM' :: forall f g a . (View f, View g, CMatrix a) => a -> Either String (M f g a)
mkM' x
  | nx == nx' && ny == ny' = Right (UnsafeM x)
  | all (== 0) [nx,nx'] && ny' == 0 =  Right zeros
  | all (== 0) [ny,ny'] && nx' == 0 =  Right zeros
  | otherwise = Left $ "mkM length mismatch: typed size: " ++ show (nx,ny) ++
                ", actual size: " ++ show (nx', ny')
  where
    nx = size (Proxy :: Proxy f)
    ny = size (Proxy :: Proxy g)
    nx' = CM.size1 x
    ny' = CM.size2 x
    zeros = mkM (CM.zeros (nx, ny))
