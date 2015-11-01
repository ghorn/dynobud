{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Dyno.View.Scheme
       ( Scheme(..)
       , FunctionIO(..)
       ) where

import GHC.Generics

import Data.Proxy
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Data.Vector ( Vector )

import Casadi.CMatrix ( CMatrix )

import Dyno.View.Unsafe ( mkM', unsafeUnM )
import qualified Dyno.View.M as M

import Dyno.View.View ( View(..), JQuad, JTriple, JTuple )

instance (View f0, View f1, View f2, View f3) => Scheme (JQuad f0 f1 f2 f3)
instance (View f0, View f1, View f2) => Scheme (JTriple f0 f1 f2)
instance (View f0, View f1) => Scheme (JTuple f0 f1)

class FunctionIO (f :: * -> *) where
  fromFioMat :: CMatrix a => a -> Either String (f a)
  toFioMat :: f a -> a
  fioMatSizes :: Proxy f -> (Int,Int)

instance (View f, View g) => Scheme (M.M f g) where
  numFields = const 1
  fromVector v = case V.toList v of
    [m] -> case fromFioMat m of
            Left err -> error $ "Scheme fromVector M error: " ++ err
            Right m' -> m'
    _ -> error $ "Scheme fromVector (M f g) length mismatch, should be 1 but got: "
         ++ show (V.length v)
  toVector = V.singleton . toFioMat
  sizeList p = [fioMatSizes p]

instance (View f, View g) => FunctionIO (M.M f g) where
  toFioMat = unsafeUnM
  fromFioMat = mkM'
  fioMatSizes = const (size (Proxy :: Proxy f), size (Proxy :: Proxy g))

class Scheme (f :: * -> *) where
  numFields :: Proxy f -> Int
  fromVector :: CMatrix a => V.Vector a -> f a
  toVector :: f a -> V.Vector a
  sizeList :: Proxy f -> [(Int,Int)]

  default numFields :: (GNumFields (Rep (f ())), Generic (f ()))
                       => Proxy f -> Int
  numFields = gnumFields . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  default sizeList :: (GSizeList (Rep (f ())), Generic (f ()))
                      => Proxy f -> [(Int,Int)]
  sizeList = F.toList . gsizeList . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  default fromVector :: ( Rep (f a) aa ~ M1 t d ff aa, GFromVector (Rep (f a)) a
                        , Generic (f a), Datatype d, CMatrix a )
                        => Vector a -> f a
  fromVector vs = out'
    where
      out' = to out
      name = datatypeName (from out')
      out = gfromVector name vs proxy
      
      reproxy :: g b -> Proxy (g b)
      reproxy = const Proxy

      proxy = reproxy out
  
  default toVector :: (GToVector (Rep (f a)) a, Generic (f a)) => f a -> Vector a
  toVector = V.fromList . F.toList . gtoVector . from

----------------------------------------------------------
class GNumFields f where
  gnumFields :: Proxy (f p) -> Int
class GSizeList f where
  gsizeList :: Proxy (f p) -> Seq.Seq (Int,Int)
class GFromVector f a where
  gfromVector :: CMatrix a => String -> Vector a -> Proxy (f a) -> f a
class GToVector f a where
  gtoVector :: f a -> Seq.Seq a

------------------------------------ GNumFields ------------------------------
instance (GNumFields f, GNumFields g) => GNumFields (f :*: g) where
  gnumFields pxy = gnumFields px + gnumFields py
    where
      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
instance GNumFields f => GNumFields (M1 i d f) where
  gnumFields = gnumFields . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy _ = Proxy
instance GNumFields (Rec0 f) where
  gnumFields = const 1
--instance GNumFields U1 where
--  gnumFields = const 0

------------------------------------ GSizeList ------------------------------
instance (GSizeList f, GSizeList g) => GSizeList (f :*: g) where
  gsizeList pxy = gsizeList px Seq.>< gsizeList py
    where
      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
instance GSizeList f => GSizeList (M1 i d f) where
  gsizeList = gsizeList . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy = const Proxy
instance FunctionIO f => GSizeList (Rec0 (f p)) where
  gsizeList = Seq.singleton . fioMatSizes . reproxy
    where
      reproxy :: Proxy (Rec0 (f p) q) -> Proxy f
      reproxy = const Proxy

--------------------- GFromVector ----------------------------
instance (GFromVector f a, GFromVector g a, GNumFields f, GNumFields g)
         => GFromVector (f :*: g) a where
  gfromVector name vs pxy
    | V.length vs == nx + ny =
        gfromVector name vx px :*: gfromVector name vy py
    | otherwise =
        error $ "\"" ++ name ++ "\" GFromVector (:*:) length error, need " ++
        show (nx,ny) ++ " but got " ++ show (V.length vs)
    where
      nx = gnumFields px
      ny = gnumFields py
      (vx,vy) = V.splitAt nx vs

      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy

instance (Datatype d, GFromVector f a) => GFromVector (D1 d f) a where
  gfromVector name vs p = ret
    where
      ret = M1 $ gfromVector (name ++ "," ++ dname) vs $ reproxy p
      dname = datatypeName ret
      reproxy :: Proxy (D1 d f a) -> Proxy (f a)
      reproxy = const Proxy

instance (Constructor c, GFromVector f a) => GFromVector (C1 c f) a where
  gfromVector name vs p = ret
    where
      ret = M1 $ gfromVector (name ++ "," ++ cname) vs $ reproxy p
      cname = conName ret
      reproxy :: Proxy (C1 c f a) -> Proxy (f a)
      reproxy = const Proxy

instance GFromVector f a => GFromVector (S1 s f) a where
  gfromVector name vs = M1 . gfromVector name vs . reproxy
    where
      reproxy :: Proxy (S1 s f a) -> Proxy (f a)
      reproxy = const Proxy

instance FunctionIO f => GFromVector (Rec0 (f a)) a where
  gfromVector name ms = const (K1 j)
    where
      j = case fromFioMat m of
        Right j' -> j'
        Left err ->
          error $ "\"" ++ name ++ "\" GFromVector fromFioMat error: " ++ err
      m = case V.toList ms of
        [m'] -> m'
        _ -> error $ "\"" ++ name ++ "\" GFromVector Rec0 length error, " ++
             "need exactly 1 value but got " ++ show (V.length ms)
--instance GFromVector U1 a where
--  gfromVector = const $ const $ const U1


--------------------- GToVector ----------------------------
instance (GToVector f a, GToVector g a, GNumFields f, GNumFields g)
         => GToVector (f :*: g) a where
  gtoVector (x :*: y) = gtoVector x Seq.>< gtoVector y

instance GToVector f a => GToVector (M1 i d f) a where
  gtoVector = gtoVector . unM1

instance (View f, View g) => GToVector (Rec0 (M.M f g a)) a where
  gtoVector = Seq.singleton . unsafeUnM . unK1

--instance GToVector U1 a where
--  gtoVector = const Seq.empty
