{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Dyno.View.Unsafe
       ( View(..), Viewable(..), M(..), J, S, JV
       , mkM, mkM', unM, unM'
       ) where

import GHC.Generics hiding ( S )

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import qualified Data.Binary as B
import qualified Data.Serialize as S

import Casadi.CMatrix ( CMatrix )
import qualified Casadi.CMatrix as CM
import Casadi.Overloading ( ArcTan2(..), Erf(..), Fmod(..), SymOrd(..) )
import Casadi.Viewable ( Viewable(..) )

import Dyno.Vectorize ( Vectorize(..), Id, devectorize, vlength )

-- | Matrix with dimensions encoded as Views.
newtype M (f :: * -> *) (g :: * -> *) (a :: *) =
  UnsafeM { unsafeUnM :: a } deriving (Eq, Functor, Generic)

instance (Viewable a, View f, View g, FromJSON a) => FromJSON (M f g a) where
  parseJSON = fmap mkM . parseJSON
instance ToJSON a => ToJSON (M f g a) where
  toJSON = toJSON . unsafeUnM

-- | Type alias for a column vector view.
type J f = M f (JV Id)

-- | Type alias for a scalar view.
type S = M (JV Id) (JV Id)

instance (View f, View g, Viewable a, B.Binary a) => B.Binary (M f g a) where
  put = B.put . unM
  get = do
    x <- B.get
    case mkM' x of
      Right y -> return y
      Left msg -> fail msg

instance (View f, View g, Viewable a, S.Serialize a) => S.Serialize (M f g a) where
  put = S.put . unM
  get = do
    x <- S.get
    case mkM' x of
      Right y -> return y
      Left msg -> fail msg

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
  fromRational x = mkM $ fromRational x * CM.ones (nx, ny)
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

instance (View f, View g, CMatrix a) => Erf (M f g a) where
  erf = over erf
  erfinv = over erfinv

mkM' :: forall f g a
        . (View f, View g, Viewable a)
        => a -> Either String (M f g a)
mkM' x
  | tx == rx && ty == ry = Right (UnsafeM x)
  | all (== 0) [tx, rx] && ry == 0 = Right zeros
  | all (== 0) [ty, ry] && rx == 0 = Right zeros
  | otherwise = Left $ "mkM' length mismatch: " ++
                "typed size: " ++ show (tx,ty) ++
                ", runtime size: " ++ show (rx, ry)
  where
    tx = size (Proxy :: Proxy f)
    ty = size (Proxy :: Proxy g)
    rx = vsize1 x
    ry = vsize2 x
    zeros = mkM (vrecoverDimension x (tx, ty))

unM' :: forall f g a
        . (View f, View g, Viewable a)
        => M f g a -> Either String a
unM' (UnsafeM x)
  | nx == nx' && ny == ny' = Right x
  | otherwise = Left $ "unM' length mismatch: " ++
                "typed size: " ++ show (nx, ny) ++
                ", actual size: " ++ show (nx', ny')
  where
    nx = size (Proxy :: Proxy f)
    ny = size (Proxy :: Proxy g)
    nx' = vsize1 x
    ny' = vsize2 x

mkM :: (View f, View g, Viewable a) => a -> M f g a
mkM x = case mkM' x of
  Right r -> r
  Left msg -> error msg

unM :: (View f, View g, Viewable a) => M f g a -> a
unM x = case unM' x of
  Right r -> r
  Left msg -> error msg


-- | Type-save "views" into vectors, which can access subvectors
--   without splitting then concatenating everything.
class View f where
  cat :: Viewable a => f a -> J f a
  default cat :: (GCat (Rep (f a)) a, Generic (f a), Viewable a) => f a -> J f a
  cat = mkM . vvertcat . V.fromList . F.toList . gcat . from

  size :: Proxy f -> Int
  default size :: (GSize (Rep (f ())), Generic (f ())) => Proxy f -> Int
  size = gsize . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  sizes :: Int -> Proxy f -> Seq.Seq Int
  default sizes :: (GSize (Rep (f ())), Generic (f ())) => Int -> Proxy f -> Seq.Seq Int
  sizes k0 = gsizes k0 . reproxy
    where
      reproxy :: Proxy g -> Proxy ((Rep (g ())) p)
      reproxy = const Proxy

  split :: Viewable a => J f a -> f a
  default split :: (GBuild (Rep (f a)) a, Generic (f a), Viewable a) => J f a -> f a
  split x'
    | null leftovers = to ret
    | otherwise = error $ unlines
                  [ "split got " ++ show (length leftovers) ++ " leftover fields"
                  , "ns: " ++ show ns ++ "\n" ++ show (map vsize1 leftovers)
                  --, "x: " ++ show x'
                  , "size1(x): " ++ show (vsize1 (unM x'))
                  --, "leftovers: " ++ show leftovers
                  , "errors: " ++ show (reverse errors)
                  ]
    where
      x = unM x'
      (ret,leftovers,errors) = gbuild [] xs
      xs = V.toList $ vvertsplit x (V.fromList ns)
      ns :: [Int]
      ns = (0 :) $ F.toList $ sizes 0 (Proxy :: Proxy f)

------------------------------------ SIZE ------------------------------
class GSize f where
  gsize :: Proxy (f p) -> Int
  gsizes :: Int -> Proxy (f p) -> Seq.Seq Int

instance (GSize f, GSize g) => GSize (f :*: g) where
  gsize pxy = gsize px + gsize py
    where
      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
  gsizes k0 pxy = xs Seq.>< ys
    where
      xs = gsizes k0 px
      ys = gsizes k1 py
      k1 = case Seq.viewr xs of
        Seq.EmptyR -> k0
        _ Seq.:> k1' -> k1'

      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy
instance GSize f => GSize (M1 i d f) where
  gsize = gsize . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy _ = Proxy
  gsizes k0 = gsizes k0 . reproxy
    where
      reproxy :: Proxy (M1 i d f p) -> Proxy (f p)
      reproxy _ = Proxy

instance View f => GSize (Rec0 (J f a)) where
  gsize = size . reproxy
    where
      reproxy :: Proxy (Rec0 (J f a) p) -> Proxy f
      reproxy _ = Proxy
  gsizes k0 = Seq.singleton . (k0 +) . size . reproxy
    where
      reproxy :: Proxy (Rec0 (J f a) p) -> Proxy f
      reproxy _ = Proxy

instance GSize U1 where
  gsize = const 0
  gsizes = const . Seq.singleton

----------------------------- CAT -------------------------------
class GCat f a where
  gcat :: f p -> Seq.Seq a

-- concatenate fields recursively
instance (GCat f a, GCat g a) => GCat (f :*: g) a where
  gcat (x :*: y) = x' Seq.>< y'
    where
      x' = gcat x
      y' = gcat y
-- discard the metadata
instance GCat f a => GCat (M1 i d f) a where
  gcat = gcat . unM1

-- any field should just hold a view, no recursion here
instance (View f, Viewable a) => GCat (Rec0 (J f a)) a where
  gcat (K1 x) = Seq.singleton (unM x)

instance GCat U1 a where
  gcat U1 = Seq.empty

-------------------------
class GBuild f a where
  gbuild :: [String] -> [a] -> (f p, [a], [String])

-- split fields recursively
instance (GBuild f a, GBuild g a, GSize f, GSize g) => GBuild (f :*: g) a where
  gbuild errs0 xs0 = (x :*: y, xs2, errs2)
    where
      (x,xs1,errs1) = gbuild errs0 xs0
      (y,xs2,errs2) = gbuild errs1 xs1

instance (GBuild f a, Datatype d) => GBuild (D1 d f) a where
  gbuild :: forall p . [String] -> [a] -> (D1 d f p, [a], [String])
  gbuild errs0 xs0 = (ret, xs1, errs1)
    where
      err = moduleName ret ++ "." ++ datatypeName ret :: String
      ret = M1 x :: D1 d f p
      (x,xs1,errs1) = gbuild (err:errs0) xs0

instance (GBuild f a, Constructor c) => GBuild (C1 c f) a where
  gbuild :: forall p . [String] -> [a] -> (C1 c f p, [a], [String])
  gbuild errs0 xs0 = (ret, xs1, errs1)
    where
      err = conName ret :: String
      ret = M1 x :: C1 c f p
      (x,xs1,errs1) = gbuild (err:errs0) xs0

instance (GBuild f a, Selector s) => GBuild (S1 s f) a where
  gbuild :: forall p . [String] -> [a] -> (S1 s f p, [a], [String])
  gbuild errs0 xs0 = (ret, xs1, errs1)
    where
      err = selName ret :: String
      ret = M1 x :: S1 s f p
      (x,xs1,errs1) = gbuild (err:errs0) xs0

-- any field should just hold a view, no recursion here
instance (View f, Viewable a) => GBuild (Rec0 (J f a)) a where
  gbuild errs (x:xs) = (K1 (mkM x), xs, errs)
  gbuild errs [] = error $ "GBuild (Rec0 (J f a)) a: empty list" ++ show (reverse errs)

instance Viewable a => GBuild U1 a where
  gbuild errs (x:xs)
    | vsize1 x /= 0 = error $ "GBuild U1: got non-empty element: " ++
                      show (vsize1 x) ++ "\n" ++ show (reverse errs)
    | otherwise = (U1, xs, errs)
  gbuild errs [] = error $ "GBuild U1: got empty" ++ show (reverse errs)

------------------------------- JV -----------------------------------
-- | views into Vectorizable things
newtype JV f a = JV { unJV :: f a } deriving (Functor, Generic, Generic1)

instance Vectorize f => View (JV f) where
  cat :: forall a . Viewable a => JV f a -> J (JV f) a
  cat = mkM . vvertcat . vectorize . unJV
  size = const $ vlength (Proxy :: Proxy f)
  sizes = const . Seq.singleton . (vlength (Proxy :: Proxy f) +)
  split :: forall a . Viewable a => J (JV f) a -> JV f a
  split = JV . devectorize . flip vvertsplit ks . unM
    where
      ks = V.fromList (take (n+1) [0..])
      n = size (Proxy :: Proxy (JV f))
