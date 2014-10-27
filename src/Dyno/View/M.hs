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
       , row
       , col
       , unrow
       , uncol
       ) where

import Data.Proxy
import qualified Data.Vector as V
import GHC.Generics ( Generic )

import Dyno.Vectorize
import Dyno.View.CasadiMat ( CasadiMat )
import Dyno.View.JV
import Dyno.View.View
import Dyno.View.Viewable
import qualified Dyno.View.CasadiMat as CM

newtype M (f :: * -> *) (g :: * -> *) (a :: *) =
  UnsafeM { unM :: a } deriving (Eq, Functor, Generic)

instance Show a => Show (M f g a) where
  showsPrec p (UnsafeM x) = showsPrec p x

mkM :: forall f g a . (View f, View g, CasadiMat a) => a -> M f g a
mkM x = case mkM' x of
  Right x' -> x'
  Left msg -> error msg

mkM' :: forall f g a . (View f, View g, CasadiMat a) => a -> Either String (M f g a)
mkM' x
  | nx == nx' && ny == ny' = Right (UnsafeM x)
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
  (Vectorize f, View g, CasadiMat a, Viewable a)
  => M (JV f) g a -> f (M (JV Id) g a)
vsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.vertsplit x nrs
  where
    nr = size (Proxy :: Proxy (JV f))
    nrs = V.fromList [0,1..nr+1]

vcat ::
  forall f g a .
  (Vectorize f, View g, CasadiMat a, Viewable a)
  => f (M (JV Id) g a) -> M (JV f) g a
vcat x = mkM $ CM.vertcat $ V.map unM (vectorize x)

hsplit ::
  forall f g a .
  (View f, Vectorize g, CasadiMat a, Viewable a)
  => M f (JV g) a -> g (M f (JV Id) a)
hsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    nc = size (Proxy :: Proxy (JV g))
    ncs = V.fromList [0,1..nc+1]

hcat ::
  forall f g a .
  (View f, Vectorize g, CasadiMat a, Viewable a)
  => g (M f (JV Id) a) -> M f (JV g) a
hcat x = mkM $ CM.horzcat $ V.map unM (vectorize x)

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
