{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Dyno.View.M
       ( M(..) -- TODO: hide the unsafe constructor
       , mkM
       , mkM'
       , mm
       , trans
       , zeros
       , ones
       , vsplit
       , hsplit
       , vcat
       , hcat
       , vsplit'
       , hsplit'
       , vcat'
       , hcat'
       , row
       , col
       , unrow
       , uncol
       , solve
       ) where

import Data.Proxy
import qualified Data.Vector as V
import GHC.Generics ( Generic )

import Casadi.Overloading

import Dyno.Vectorize
import Dyno.View.CasadiMat ( CasadiMat )
import Dyno.View.JV
import Dyno.TypeVecs ( Vec, Dim(..) )
import Dyno.View.View
import Dyno.View.Viewable
import qualified Dyno.View.CasadiMat as CM

newtype M (f :: * -> *) (g :: * -> *) (a :: *) =
  UnsafeM { unM :: a } deriving (Eq, Functor, Generic)

instance Show a => Show (M f g a) where
  showsPrec p (UnsafeM x) = showsPrec p x

over :: (View f, View g, CasadiMat a) => (a -> a) -> M f g a -> M f g a
over f (UnsafeM x) = mkM (f x)

over2 :: (View f, View g, CasadiMat a) => (a -> a -> a) -> M f g a -> M f g a -> M f g a
over2 f (UnsafeM x) (UnsafeM y)= mkM (f x y)

instance (View f, View g, CasadiMat a) => Num (M f g a) where
  (+) = over2 (+)
  (-) = over2 (-)
  (*) = over2 (*)
  negate = over negate
  abs = over abs
  signum = over signum
  fromInteger k = mkM $ fromInteger k * CM.ones (nx,ny)
    where
      nx = size (Proxy :: Proxy f)
      ny = size (Proxy :: Proxy f)
instance (View f, View g, CasadiMat a) => Fractional (M f g a) where
  (/) = over2 (/)
  fromRational k = mkM $ fromRational k * CM.ones (nx,ny)
    where
      nx = size (Proxy :: Proxy f)
      ny = size (Proxy :: Proxy f)
instance (View f, View g, CasadiMat a) => Floating (M f g a) where
  pi = mkM $ pi * CM.ones (nx,ny)
    where
      nx = size (Proxy :: Proxy f)
      ny = size (Proxy :: Proxy f)
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

instance (View f, View g, CasadiMat a) => Fmod (M f g a) where
  fmod = over2 fmod

instance (View f, View g, CasadiMat a) => ArcTan2 (M f g a) where
  arctan2 = over2 arctan2

instance (View f, View g, CasadiMat a) => SymOrd (M f g a) where
  leq = over2 leq
  geq = over2 geq
  eq  = over2 eq

mkM :: forall f g a . (View f, View g, CasadiMat a) => a -> M f g a
mkM x = case mkM' x of
  Right x' -> x'
  Left msg -> error msg

mkM' :: forall f g a . (View f, View g, CasadiMat a) => a -> Either String (M f g a)
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

mm :: (View f, View h, CasadiMat a) => M f g a -> M g h a -> M f h a
mm (UnsafeM m0) (UnsafeM m1) = mkM (CM.mm m0 m1)

trans :: (View f, View g, CasadiMat a) => M f g a -> M g f a
trans (UnsafeM m) = mkM (CM.trans m)

vsplit ::
  forall f g a .
  (Vectorize f, View g, CasadiMat a)
  => M (JV f) g a -> f (M (JV Id) g a)
vsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.vertsplit x nrs
  where
    nr = size (Proxy :: Proxy (JV f))
    nrs = V.fromList [0,1..nr]

vcat ::
  forall f g a .
  (Vectorize f, View g, CasadiMat a)
  => f (M (JV Id) g a) -> M (JV f) g a
vcat x = mkM $ CM.vertcat $ V.map unM (vectorize x)

hsplit ::
  forall f g a .
  (View f, Vectorize g, CasadiMat a)
  => M f (JV g) a -> g (M f (JV Id) a)
hsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    nc = size (Proxy :: Proxy (JV g))
    ncs = V.fromList [0,1..nc]

hcat ::
  forall f g a .
  (View f, Vectorize g, CasadiMat a)
  => g (M f (JV Id) a) -> M f (JV g) a
hcat x = mkM $ CM.horzcat $ V.map unM (vectorize x)

vcat' ::
  forall f g n a .
  (View f, View g, Dim n, CasadiMat a)
  => Vec n (M f g a) -> M (JVec n f) g a
vcat' x = mkM $ CM.vertcat $ V.map unM (vectorize x)

vsplit' ::
  forall f g n a .
  (View f, View g, Dim n, CasadiMat a)
  => M (JVec n f) g a -> Vec n (M f g a)
vsplit' (UnsafeM x)
  | n == 0 = fill zeros
  | nr == 0 = fill zeros
  | otherwise = fmap mkM $ devectorize $ CM.vertsplit x nrs
  where
    n = reflectDim (Proxy :: Proxy n)
    nr = size (Proxy :: Proxy f)
    nrs = V.fromList [0,nr..n*nr]

hcat' ::
  forall f g n a .
  (View f, View g, Dim n, CasadiMat a)
  => Vec n (M f g a) -> M f (JVec n g) a
hcat' x = mkM $ CM.horzcat $ V.map unM (vectorize x)

hsplit' ::
  forall f g n a .
  (View f, View g, Dim n, CasadiMat a)
  => M f (JVec n g) a -> Vec n (M f g a)
hsplit' (UnsafeM x)
  | n == 0 = fill zeros
  | nc == 0 = fill zeros
  | otherwise = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    n = reflectDim (Proxy :: Proxy n)
    nc = size (Proxy :: Proxy g)
    ncs = V.fromList [0,nc..n*nc]

zeros :: forall f g a . (View f, View g, CasadiMat a) => M f g a
zeros = mkM z
  where
    z = CM.zeros (rows, cols)
    rows = size (Proxy :: Proxy f)
    cols = size (Proxy :: Proxy g)

ones :: forall f g a . (View f, View g, CasadiMat a) => M f g a
ones = mkM z
  where
    z = CM.ones (rows, cols)
    rows = size (Proxy :: Proxy f)
    cols = size (Proxy :: Proxy g)

row :: (CasadiMat a, View f) => J f a -> M (JV Id) f a
row (UnsafeJ x) = mkM (CM.trans x)

col :: (CasadiMat a, View f) => J f a -> M f (JV Id) a
col (UnsafeJ x) = mkM x

unrow :: (Viewable a, CasadiMat a, View f) => M (JV Id) f a -> J f a
unrow (UnsafeM x) = mkJ (CM.trans x)

uncol :: (Viewable a, CasadiMat a, View f) => M f (JV Id) a -> J f a
uncol (UnsafeM x) = mkJ x

solve :: (View g, View h, CasadiMat a) => M f g a -> M f h a -> M g h a
solve (UnsafeM x) (UnsafeM y) = mkM (CM.solve x y)
