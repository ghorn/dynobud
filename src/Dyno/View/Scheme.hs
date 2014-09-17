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
       , M(..)
       , FunctionIO(..)
       ) where

import Data.Proxy
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Data.Vector ( Vector )
import GHC.Generics hiding ( S )

import Dyno.Nats
import Dyno.View.View
import Dyno.View.CasadiMat

data MyScheme a = MyScheme (J (JVec D3 S) a) (J (JVec D2 S) a) deriving (Generic, Generic1, Show)
instance Scheme MyScheme

--go :: MyScheme MX
--go = fromVector (V.fromList [400,500])
--
--og :: V.Vector MX
--og = toVector go

newtype M (f :: * -> *) (g :: * -> *) (a :: *) =
  UnsafeM { unM :: a } deriving (Eq, Functor, Generic)

class FunctionIO (f :: * -> *) where
  fromMat :: CasadiMat a => a -> Either String (f a)
  toMat :: f a -> a
  matSizes :: Proxy f -> (Int,Int)

instance View x => Scheme (J x) where
  numFields = const 1
  fromVector v = case V.toList v of
    [m] -> case fromMat m of Left err -> error $ "Scheme fromVector J error: " ++ err
                             Right m' -> m'
    _ -> error $ "Scheme fromVector (J x) length mismatch, should be 1 but got: "
         ++ show (V.length v)
  toVector = V.singleton . toMat
  sizeList p = [matSizes p]

instance View f => FunctionIO (J f) where
  toMat = unsafeUnJ
  fromMat x
    | n1 /= n1' = mismatch
    | n1 /= 0 && n2 /= n2' = mismatch
    | n1 == 0 && not (n2 `elem` [0,1]) = mismatch
    | otherwise = Right (UnsafeJ x)
    where
      mismatch = Left $ "length mismatch: typed size: " ++ show (n1',n2') ++
                 ", actual size: " ++ show (n1,n2)
      n1' = size (Proxy :: Proxy f)
      n2' = 1
      n1 = size1 x
      n2 = size2 x
  matSizes = const (size (Proxy :: Proxy f), 1)

instance (View f, View g) => FunctionIO (M f g) where
  toMat = unM
  fromMat x
    | n2 /= n2' = mismatch
    | n1 /= n1' = mismatch
    | otherwise = Right (UnsafeM x)
    where
      mismatch = Left $ "length mismatch: typed size: " ++ show (n1',n2') ++
                 ", actual size: " ++ show (n1,n2)
      n1' = size (Proxy :: Proxy f)
      n2' = size (Proxy :: Proxy g)
      n1 = size1 x
      n2 = size2 x
  matSizes = const (size (Proxy :: Proxy f), size (Proxy :: Proxy g))

class Scheme (f :: * -> *) where
  numFields :: Proxy f -> Int
  fromVector :: CasadiMat a => V.Vector a -> f a
  toVector :: f a -> V.Vector a
  sizeList :: Proxy f -> [(Int,Int)]

  default numFields :: (GNumFields (Rep (f ())), Generic (f ())) => Proxy f -> Int
  numFields = gnumFields . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  default sizeList :: (GSizeList (Rep (f ())), Generic (f ())) => Proxy f -> [(Int,Int)]
  sizeList = F.toList . gsizeList . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  default fromVector :: ( Rep (f a) aa ~ M1 t d ff aa, GFromVector (Rep (f a)) a
                        , Generic (f a), Datatype d, CasadiMat a )
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
  gfromVector :: CasadiMat a => String -> Vector a -> Proxy (f a) -> f a
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
  gsizeList = Seq.singleton . matSizes . reproxy
    where
      reproxy :: Proxy (Rec0 (f p) q) -> Proxy f
      reproxy = const Proxy

--------------------- GFromVector ----------------------------
instance (GFromVector f a, GFromVector g a, GNumFields f, GNumFields g) => GFromVector (f :*: g) a where
  gfromVector name vs pxy
    | V.length vs == nx + ny = gfromVector name vx px :*: gfromVector name vy py
    | otherwise = error $ "\"" ++ name ++ "\" GFromVector (:*:) length error, need " ++
                  show (nx,ny) ++ " but got " ++ show (V.length vs)
    where
      nx = gnumFields px
      ny = gnumFields py
      (vx,vy) = V.splitAt nx vs

      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy

instance GFromVector f a => GFromVector (M1 i d f) a where
  gfromVector name vs = M1 . gfromVector name vs . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy = const Proxy

instance FunctionIO f => GFromVector (Rec0 (f a)) a where
  gfromVector name ms = const (K1 j)
    where
      j = case fromMat m of
        Right j' -> j'
        Left err -> error $ "\"" ++ name ++ "\" GFromVector fromMat error: " ++ err
      m = case V.toList ms of
        [m'] -> m'
        _ -> error $ "\"" ++ name ++ "\" GFromVector Rec0 length error, " ++
             "need exactly 1 value but got " ++ show (V.length ms)
--instance GFromVector U1 a where
--  gfromVector = const $ const $ const U1


--------------------- GToVector ----------------------------
instance (GToVector f a, GToVector g a, GNumFields f, GNumFields g) => GToVector (f :*: g) a where
  gtoVector (x :*: y) = gtoVector x Seq.>< gtoVector y

instance GToVector f a => GToVector (M1 i d f) a where
  gtoVector = gtoVector . unM1

instance View f => GToVector (Rec0 (J f a)) a where
  gtoVector = Seq.singleton . unsafeUnJ . unK1

--instance GToVector U1 a where
--  gtoVector = const Seq.empty
