{-# OPTIONS_GHC -Wall #-}

module Dyno.View.FunJac
       ( JacIn(..)
       , JacOut(..), HessOut(..)
       , Jac(..), Hess(..)
       ) where

import Data.Proxy
import qualified Data.Vector as V

import Dyno.View.View
import Dyno.View.Scheme
import Dyno.View.M

data JacIn xj x a = JacIn (J xj a) (x a) deriving Show
data JacOut fj f a = JacOut (J fj a) (f a) deriving Show
data Jac xj fj f a = Jac (M fj xj a) (J fj a) (f a) deriving Show
data HessOut f a = HessOut (S a) (f a) deriving Show
data Hess xj f a = Hess (M xj xj a) (J xj a) (S a) (f a) deriving Show

instance (View xj, Scheme x) => Scheme (JacIn xj x) where
  numFields = (1+) . numFields . reproxy
    where
      reproxy :: Proxy (JacIn xj x) -> Proxy x
      reproxy = const Proxy
  fromVector v = JacIn j0 (fromVector (V.tail v))
    where
      j0 = case fromFioMat (V.head v) of
        Left err -> error $ "JacIn fromVector error: " ++ err
        Right j0' -> j0'

  toVector (JacIn xj x) = V.cons (toFioMat xj) (toVector x)
  sizeList p = fioMatSizes (reproxy' p) : sizeList (reproxy p)
    where
      reproxy :: Proxy (JacIn xj x) -> Proxy x
      reproxy = const Proxy
      reproxy' :: Proxy (JacIn xj x) -> Proxy (J xj)
      reproxy' = const Proxy

instance (View fj, Scheme f) => Scheme (JacOut fj f) where
  numFields = (1+) . numFields . reproxy
    where
      reproxy :: Proxy (JacOut fj f) -> Proxy f
      reproxy = const Proxy
  fromVector v = JacOut j0 (fromVector (V.tail v))
    where
      j0 = case fromFioMat (V.head v) of
        Left err -> error $ "JacOut fromVector error: " ++ err
        Right j0' -> j0'

  toVector (JacOut fj f) = V.cons (toFioMat fj) (toVector f)
  sizeList p = fioMatSizes (reproxy' p) : sizeList (reproxy p)
    where
      reproxy :: Proxy (JacOut fj f) -> Proxy f
      reproxy = const Proxy
      reproxy' :: Proxy (JacOut fj f) -> Proxy (J fj)
      reproxy' = const Proxy

instance Scheme f => Scheme (HessOut f) where
  numFields = (1+) . numFields . reproxy
    where
      reproxy :: Proxy (HessOut f) -> Proxy f
      reproxy = const Proxy
  fromVector v = HessOut h0 (fromVector (V.tail v))
    where
      h0 = case fromFioMat (V.head v) of
        Left err -> error $ "HessOut fromVector error: " ++ err
        Right r -> r

  toVector (HessOut s f) = V.cons (toFioMat s) (toVector f)
  sizeList p = fioMatSizes (reproxy' p) : sizeList (reproxy p)
    where
      reproxy' :: Proxy (HessOut f) -> Proxy S
      reproxy' = const Proxy
      reproxy :: Proxy (HessOut f) -> Proxy f
      reproxy = const Proxy


instance (View xj, View fj, Scheme f) => Scheme (Jac xj fj f) where
  numFields = (2+) . numFields . reproxy
    where
      reproxy :: Proxy (Jac xj fj f) -> Proxy f
      reproxy = const Proxy
  fromVector v = Jac m fj (fromVector (V.drop 2 v))
    where
      m = case fromFioMat (v V.! 0) of
        Left err -> error $ "Jac fromVector error: " ++ err
        Right j0' -> j0'
      fj = case fromFioMat (v V.! 1) of
        Left err -> error $ "Jac fromVector error: " ++ err
        Right j0' -> j0'
  toVector (Jac m fj f) = V.fromList [toFioMat m, toFioMat fj] V.++ toVector f
  sizeList p = fioMatSizes (reproxy'' p) : fioMatSizes (reproxy' p) : sizeList (reproxy p)
    where
      reproxy'' :: Proxy (Jac xj fj f) -> Proxy (M fj xj)
      reproxy'' = const Proxy

      reproxy' :: Proxy (Jac xj fj f) -> Proxy (J fj)
      reproxy' = const Proxy

      reproxy :: Proxy (Jac xj fj f) -> Proxy f
      reproxy = const Proxy


instance (View xj, Scheme f) => Scheme (Hess xj f) where
  numFields = (3+) . numFields . reproxy
    where
      reproxy :: Proxy (Hess xj f) -> Proxy f
      reproxy = const Proxy
  fromVector v = Hess hess jac fun (fromVector (V.drop 3 v))
    where
      hess = case fromFioMat (v V.! 0) of
        Left err -> error $ "Hess fromVector error: " ++ err
        Right r -> r
      jac = case fromFioMat (v V.! 1) of
        Left err -> error $ "Hess fromVector error: " ++ err
        Right r -> r
      fun = case fromFioMat (v V.! 2) of
        Left err -> error $ "Hess fromVector error: " ++ err
        Right r -> r
  toVector (Hess hess jac fun f) = V.fromList [toFioMat hess, toFioMat jac, toFioMat fun] V.++ toVector f
  sizeList p =
      fioMatSizes (reproxy''' p)
    : fioMatSizes (reproxy'' p)
    : fioMatSizes (reproxy' p)
    : sizeList (reproxy p)
    where
      reproxy''' :: Proxy (Hess xj f) -> Proxy (M xj xj)
      reproxy''' = const Proxy

      reproxy'' :: Proxy (Hess xj f) -> Proxy (J xj)
      reproxy'' = const Proxy

      reproxy' :: Proxy (Hess xj f) -> Proxy S
      reproxy' = const Proxy

      reproxy :: Proxy (Hess xj f) -> Proxy f
      reproxy = const Proxy
