{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Dyno.View.View
       ( J(..), mkJ, unJ, unJ', View(..), JVec(..), JNone(..), S(..), JV(..), JV'(..), JV''(..)
       , JTuple(..)
       , jreplicate, jreplicate'
       , reifyJVec, jfill
       ) where

import GHC.Generics hiding ( S )

import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Traversable ( Traversable )
import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Serialize ( Serialize(..) )

import Dyno.TypeVecs ( Vec(..), unVec, mkVec, mkVec', reifyVector )
import Dyno.View.Viewable ( Viewable(..), CasadiMat(..) )
import Dyno.Vectorize ( Vectorize(..), vlength )
import Dyno.Server.Accessors -- ( Lookup(..) )
import Dyno.Casadi.MX ( MX )
import Dyno.Casadi.SX ( SX )
import Dyno.Casadi.DMatrix ( DMatrix )

data JTuple f g a = JTuple (J f a) (J g a) deriving ( Generic, Show )

--instance View Id
--instance View Xy
--instance View Xyz
--instance View f => View (Fctr f)
instance (View f, View g) => View (JTuple f g)

newtype J (f :: * -> *) (a :: *) = UnsafeJ { unsafeUnJ :: a } deriving (Eq, Functor, Generic)

--------------------------------------------------------------------
-- num/fractional/floating instances
instance (View f) => Num (J f SX) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ (x + y)
  (UnsafeJ x) - (UnsafeJ y) = mkJ (x - y)
  (UnsafeJ x) * (UnsafeJ y) = mkJ (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = mkJ (fromInteger k * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Fractional (J f SX) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ (x / y)
  fromRational x = mkJ (fromRational x * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Floating (J f SX) where
  pi = mkJ (pi * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ (x ** y)
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

--------------------------------------------------------------------
-- num/fractional/floating instances
instance (View f) => Num (J f MX) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ (x + y)
  (UnsafeJ x) - (UnsafeJ y) = mkJ (x - y)
  (UnsafeJ x) * (UnsafeJ y) = mkJ (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = mkJ (fromInteger k * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Fractional (J f MX) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ (x / y)
  fromRational x = mkJ (fromRational x * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Floating (J f MX) where
  pi = mkJ (pi * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ (x ** y)
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

--------------------------------------------------------------------
-- num/fractional/floating instances
instance (View f) => Num (J f DMatrix) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ (x + y)
  (UnsafeJ x) - (UnsafeJ y) = mkJ (x - y)
  (UnsafeJ x) * (UnsafeJ y) = mkJ (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger k = mkJ (fromInteger k * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Fractional (J f DMatrix) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ (x / y)
  fromRational x = mkJ (fromRational x * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)

instance (View f) => Floating (J f DMatrix) where
  pi = mkJ (pi * ones (n, 1))
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ (x ** y)
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh
--------------------------------------------------------------------
-- num/fractional/floating instances
instance (View f, Num a) => Num (J f (Vector a)) where
  (UnsafeJ x) + (UnsafeJ y) = mkJ $ V.zipWith (+) x y
  (UnsafeJ x) - (UnsafeJ y) = mkJ $ V.zipWith (-) x y
  (UnsafeJ x) * (UnsafeJ y) = mkJ $ V.zipWith (*) x y
  abs = fmap (fmap abs)
  signum = fmap (fmap signum)
  fromInteger k = mkJ (V.replicate n (fromInteger k))
    where
      n = size (Proxy :: Proxy f)

instance (View f, Fractional a) => Fractional (J f (Vector a)) where
  (UnsafeJ x) / (UnsafeJ y) = mkJ $ V.zipWith (/) x y
  fromRational x = mkJ (V.replicate n (fromRational x))
    where
      n = size (Proxy :: Proxy f)

instance (View f, Floating a) => Floating (J f (Vector a)) where
  pi = mkJ (V.replicate n pi)
    where
      n = size (Proxy :: Proxy f)
  (**) (UnsafeJ x) (UnsafeJ y) = mkJ $ V.zipWith (**) x y
  exp   = fmap (fmap exp)
  log   = fmap (fmap log)
  sin   = fmap (fmap sin)
  cos   = fmap (fmap cos)
  tan   = fmap (fmap tan)
  asin  = fmap (fmap asin)
  atan  = fmap (fmap atan)
  acos  = fmap (fmap acos)
  sinh  = fmap (fmap sinh)
  cosh  = fmap (fmap cosh)
  tanh  = fmap (fmap tanh)
  asinh = fmap (fmap asinh)
  atanh = fmap (fmap atanh)
  acosh = fmap (fmap acosh)
-----------------------------------------------------------------------

mkJ :: forall f a . (View f, Viewable a) => a -> J f a
mkJ x
  | nx == nx' = UnsafeJ x
  | otherwise = error $ "mkJ length mismatch: typed size: " ++ show nx ++
                ", actual size: " ++ show nx'
  where
    nx = size (Proxy :: Proxy f)
    nx' = vsize1 x

unJ :: forall f a . (View f, Viewable a) => J f a -> a
unJ (UnsafeJ x)
  | nx == nx' = x
  | otherwise = error $ "unJ length mismatch: typed size: " ++ show nx ++
                ", actual size: " ++ show nx'
  where
    nx = size (Proxy :: Proxy f)
    nx' = vsize1 x

unJ' :: forall f a . (View f, Viewable a) => String -> J f a -> a
unJ' msg (UnsafeJ x)
  | nx == nx' = x
  | otherwise = error $ "unJ length mismatch in \"" ++ msg ++ "\": typed size: " ++ show nx ++
                ", actual size: " ++ show nx'
  where
    nx = size (Proxy :: Proxy f)
    nx' = vsize1 x

instance Serialize a => Serialize (J f a)
instance Show a => Show (J f a) where
  showsPrec p (UnsafeJ x) = showsPrec p x
instance (Show a, Lookup a) => Lookup (J S (Vector a)) where
  toAccessorTree :: J S (Vector a) -> (b -> J S (Vector a)) -> AccessorTree b
  toAccessorTree (UnsafeJ x) f =
    toAccessorTree (V.head x) (V.head . unJ . f)

-- | vectors in View
newtype JVec n f a = JVec { unJVec :: Vec n (J f a) } deriving ( Show, Eq )
instance (Dim n, View f) => View (JVec n f) where
  cat = mkJ . vveccat . fmap unJ . unVec . unJVec
  split = JVec . fmap mkJ . mkVec . flip vvecsplit ks . unJ
    where
      ks = V.fromList (take (n+1) [0,m..])
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)
  size = const (n * m)
    where
      n = reflectDim (Proxy :: Proxy n)
      m = size (Proxy :: Proxy f)
instance (Dim n, Serialize (J f a)) => Serialize (JVec n f a) where
  get = fmap (JVec . mkVec') get
  put = put . F.toList . unJVec

jreplicate' :: forall a n f . (Dim n, View f) => J f a -> JVec n f a
jreplicate' el =  ret
  where
    ret = JVec (mkVec (V.replicate nvec el))
    nvec = size (Proxy :: Proxy (JVec n S))

jreplicate :: forall a n f . (Dim n, View f, Viewable a) => J f a -> J (JVec n f) a
jreplicate = cat . jreplicate'

jfill :: forall a f . (View f) => a -> J f (Vector a)
jfill x = mkJ (V.replicate n x)
  where
    n = size (Proxy :: Proxy f)

reifyJVec :: forall a f r . Vector (J f a) -> (forall (n :: *). Dim n => JVec n f a -> r) -> r
reifyJVec v f = reifyVector v $ \(v' :: Vec n (J f a)) -> f (JVec v' :: JVec n f a)
{-# INLINE reifyJVec #-}

-- | view into a None, for convenience
data JNone a = JNone deriving ( Eq, Generic, Generic1, Show, Functor, Foldable, Traversable )
-- just use a Vectorize instance
instance Vectorize JNone where
--instance View JNone where
--  cat = J . unJ . cat . JV
--  size = const (size (Proxy :: Proxy (JV JNone)))
--  split = unJV . split . J . unJ
instance View JNone where
  cat = const $ mkJ (vveccat V.empty)
  size = const 0
  split = const JNone

-- | view into a scalar, for convenience
newtype S a = S { unS :: a } deriving ( Eq, Num, Fractional, Floating, Generic, Generic1, Show, Functor, Foldable, Traversable )
instance View S where
  cat :: forall a . Viewable a => S a -> J S a
  cat (S x) = mkJ x
  size = const 1
  split :: forall a . Viewable a => J S a -> S a
  split (UnsafeJ x)
    | n == 1 = S x
    | otherwise = error $ "split S: got length " ++ show n
    where
      n = vsize1 x

newtype JV'' f a = JV'' { unJV'' :: f (J S a) }
instance Vectorize f => View (JV'' f) where
  cat :: forall a . Viewable a => JV'' f a -> J (JV'' f) a
  cat = mkJ . vveccat . fmap unJ . vectorize . unJV''
  size = const $ vlength (empty :: f ())
  split :: forall a . Viewable a => J (JV'' f) a -> JV'' f a
  split = JV'' . devectorize . fmap mkJ . flip vvecsplit ks. unJ
    where
      ks = V.fromList (take (n+1) [0..])
      n = size (Proxy :: Proxy (JV'' f))


newtype JV' f a = JV' { unJV' :: f (S a) }
instance Vectorize f => View (JV' f) where
  cat :: forall a . Viewable a => JV' f a -> J (JV' f) a
  cat = mkJ . vveccat . fmap unS . vectorize . unJV'
  size = const $ vlength (empty :: f ())
  split :: forall a . Viewable a => J (JV' f) a -> JV' f a
  split = JV' . devectorize . fmap S . flip vvecsplit ks. unJ
    where
      ks = V.fromList (take (n+1) [0..])
      n = size (Proxy :: Proxy (JV' f))

newtype JV f a = JV { unJV :: f a }
instance Vectorize f => View (JV f) where
  cat :: forall a . Viewable a => JV f a -> J (JV f) a
  cat = mkJ . vveccat . vectorize . unJV
  size = const $ vlength (empty :: f ())
  split :: forall a . Viewable a => J (JV f) a -> JV f a
  split = JV . devectorize . flip vvecsplit ks. unJ
    where
      ks = V.fromList (take (n+1) [0..])
      n = size (Proxy :: Proxy (JV f))



-- | Type-save "views" into vectors, which can access subvectors
--   without splitting then concatenating everything.
class View f where
  cat :: Viewable a => f a -> J f a
  default cat :: (GCat (Rep (f a)) a, Generic (f a), Viewable a) => f a -> J f a
  cat = mkJ . gcat vveccat . from

  size :: Proxy f -> Int
  default size :: (GSize (Rep (f ())), Generic (f ())) => Proxy f -> Int
  size = gsize . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  split :: Viewable a => J f a -> f a
  default split :: (GSplit (Rep (f a)) a, Generic (f a), Viewable a) => J f a -> f a
  split = to . gsplit vvecsplit . unJ

------------------------------------ SIZE ------------------------------
class GSize f where
  gsize :: Proxy (f p) -> Int

instance (GSize f, GSize g) => GSize (f :*: g) where
  gsize pxy = gsize px + gsize py
    where
      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
instance GSize f => GSize (M1 i d f) where
  gsize = gsize . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy _ = Proxy

instance View f => GSize (Rec0 (J f a)) where
  gsize = size . reproxy
    where
      reproxy :: Proxy (Rec0 (J f a) p) -> Proxy f
      reproxy _ = Proxy

instance GSize U1 where
  gsize = const 0

----------------------------- CAT -------------------------------
class GCat f a where
  gcat :: (Vector a -> a) -> f p -> a

-- concatenate fields recursively
instance (GCat f a, GCat g a) => GCat (f :*: g) a where
  gcat veccat (x :*: y) = veccat (V.fromList [x',y'])
    where
      x' = gcat veccat x
      y' = gcat veccat y
-- discard the metadata
instance GCat f a => GCat (M1 i d f) a where
  gcat veccat = gcat veccat . unM1

-- any field should just hold a view, no recursion here
instance GCat (Rec0 (J f a)) a where
  gcat _ (K1 (UnsafeJ x)) = x

instance GCat U1 a where
  gcat veccat U1 = veccat V.empty

------------------------------- SPLIT -------------------------------
class GSplit f a where
  gsplit :: (a -> Vector Int -> Vector a) -> a -> f p

-- split fields recursively
instance (GSplit f a, GSplit g a, GSize f, GSize g) => GSplit (f :*: g) a where
  gsplit vecsplit mxy = ret
    where
      ret = x :*: y
      
      nx = gsize (proxy x)
      ny = gsize (proxy y)

      proxy :: h -> Proxy h
      proxy = const Proxy

      mx :: a
      my :: a
      (mx,my) = case V.toList (vecsplit mxy (V.fromList [0, nx, nx + ny])) of
        [mx',my'] -> (mx',my')
--        [mx',my'] -> if nx == size1 mx' && ny == size1 my'
--                          then (mx',my')
--                          else error "size mismatch in vertsplit"
        bad -> error $ "vecsplit error, wanted 2 but got: " ++ show (length bad)
      
      x = gsplit vecsplit mx -- :: (J f)
      y = gsplit vecsplit my -- :: g p -- (J xy)
-- discard the metadata
instance GSplit f a => GSplit (M1 i d f) a where
  gsplit vecsplit = M1 . gsplit vecsplit
-- any field should just hold a view, no recursion here
instance GSplit (Rec0 (J f a)) a where
  gsplit _ x = K1 (UnsafeJ x)

instance GSplit U1 a where
  gsplit _ _ = U1
