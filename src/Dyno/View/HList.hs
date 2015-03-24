{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Dyno.View.HList
       ( (:*:)(..)
       ) where

--import qualified Data.Sequence as Seq
import Data.Proxy
import qualified Data.Vector as V

import Dyno.View.Scheme
--import Dyno.View.View
--import Dyno.View.Viewable ( Viewable(..) )

infixr 6 :*:
data (:*:) f g a = (:*:) (f a) (g a)

--class HFromVec f where
--  

instance (Scheme f, Scheme g) => Scheme (f :*: g) where
  numFields pxy = numFields px + numFields py
    where
      reproxy :: Proxy (x :*: y) -> (Proxy x, Proxy y)
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
  toVector (x :*: y) = toVector x V.++ toVector y
  fromVector xy = fromVector x :*: fromVector y
    where
      (x,y)
        | V.length x' /= nx = error "splitting HList in casadi Fun got length mismatch"
        | V.length y' /= ny = error "splitting HList in casadi Fun got length mismatch"
        | otherwise = (x',y')
      (x',y')= V.splitAt nx xy
      nx = numFields (Proxy :: Proxy f)
      ny = numFields (Proxy :: Proxy g)
  sizeList pxy = xs ++ ys
    where
      xs = sizeList px
      ys = sizeList py

      reproxy :: Proxy (x :*: y) -> (Proxy x, Proxy y)
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy

--instance (View f, View g) => View (f :*: g) where
--  cat (x :*: y) = mkJ (vveccat (V.fromList [x', y']))
--    where
--      UnsafeJ x' = cat x
--      UnsafeJ y' = cat y
--  size = const $ size (Proxy :: Proxy f) + size (Proxy :: Proxy g)
--  sizes k0 = const $  xs Seq.>< ys
--    where
--      xs = sizes k0 (Proxy :: Proxy f)
--      ys = sizes k1 (Proxy :: Proxy g)
--      k1 = case Seq.viewr xs of
--        Seq.EmptyR -> k0
--        _ Seq.:> k1' -> k1'
----  split :: forall a . Viewable a => J S a -> S a
--  split = undefined -- S . unJ
--  
--
--
--
--
--
--
--
--data Tup f g -- only for proxies
--
--class HSplit f where
----  hsplit :: M f g a -> HSplitT f g a
--  hsizeList :: Proxy f -> [Int]
--  hfromList :: Viewable a => [a] -> (HSplitT f a, [a])
--
--instance (HSplit f1, HSplit f2) => HSplit (f1 :*: f2) where
----  hsplit = undefined
--  hsizeList p = hsizeList p1 ++ hsizeList p2
--    where
--      reproxy :: Proxy (x :*: y) -> (Proxy x, Proxy y)
--      reproxy = const (Proxy, Proxy)
--      (p1,p2) = reproxy p
--
--instance View f => HSplit f where
----  hsplit = undefined
--  hsizeList p = [size p]
--    where
--  hfromList (x:xs) = (mkJ x, xs)
--
----hsplit :: HSplit f g => M f g a -> HSplitT f g a
----hsplit m@(UnsafeM mat) = undefined
----  where
----    reproxy :: M f g a -> Proxy (Tup f g)
----    splitargs = scanl (+) 0 $ hsizeList (reproxy m)
--
--type family HSplitT (f :: * -> *) a where
--  HSplitT (f1 :*: f2) a = (HSplitT f1 :*: HSplitT f2) a
--  HSplitT f a = J f a
