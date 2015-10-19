{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-} -- so that "Vec (n :: Nat) a" works
{-# LANGUAGE InstanceSigs #-}

module Dyno.TypeVecs
       ( Vec
       , Succ
       , unVec
       , mkVec'
       , tvlength
       , (|>)
       , (<|)
       , tvtranspose
       , tvzip
       , tvzip3
       , tvzip4
       , tvzipWith
       , tvzipWith3
       , tvzipWith4
       , tvzipWith5
       , tvzipWith6
       , tvunzip
       , tvunzip3
       , tvunzip4
       , tvunzip5
       , tvhead
       , tvtail
       , tvlast
       , tvshiftl
       , tvshiftr
       , tvlinspace
       , reifyVector
       , reifyDim
       , Dim(..)
       )
       where

import GHC.Generics ( Generic, Generic1 )

import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Vector as V
import Data.Vector.Binary () -- instances
import Data.Vector.Cereal () -- instances
import qualified Data.Binary as B
import qualified Data.Serialize as S
import Linear.Vector
import Linear.V ( Dim(..) )
import Data.Proxy
import Data.Reflection as R
import Data.Distributive ( Distributive(..) )
import Prelude -- BBP workaround

import Accessors ( Lookup(..), AccessorTree(..) )

import Dyno.Vectorize

-- length-indexed vectors using phantom types
newtype Vec (n :: k) a = MkVec (V.Vector a)
                deriving (Functor, Generic, Generic1)
instance (Dim n, B.Binary a) => B.Binary (Vec n a) where
  put = B.put . unVec
  get = do
    x <- B.get
    case devectorize' x of
      Right y -> return y
      Left msg -> fail msg
instance (Dim n, S.Serialize a) => S.Serialize (Vec n a) where
  put = S.put . unVec
  get = do
    x <- S.get
    case devectorize' x of
      Right y -> return y
      Left msg -> fail msg

instance (Lookup a, Dim n) => Lookup (Vec n a) where
  toAccessorTree vec get set = Data ("Vec " ++ show n, "Vec " ++ show n) $ map child (take n [0..])
    where
      n = reflectDim (Proxy :: Proxy n)
      child k = ("v" ++ show k, toAccessorTree (getK vec) (getK . get) setK)
        where
          setK vk new = set (devectorize (v V.// [(k,vk)])) new
            where
              MkVec v = get new

          getK :: Vec n a -> a
          getK (MkVec v) = v V.! k

instance Dim n => Distributive (Vec n) where
  distribute f = devectorize $ V.generate (reflectDim (Proxy :: Proxy n))
                 $ \i -> fmap (\v -> V.unsafeIndex (vectorize v) i) f
  {-# INLINE distribute #-}

data Succ n
instance Dim n => Dim (Succ n) where
  reflectDim _ = 1 + reflectDim (Proxy :: Proxy n)

instance Dim n => Dim (Vec n a) where
  reflectDim _ = reflectDim (Proxy :: Proxy n)

instance Dim n => Applicative (Vec n) where
  pure x = ret
    where
      ret = MkVec $ V.replicate (tvlength ret) x
  MkVec xs <*> MkVec ys = MkVec $ V.zipWith id xs ys

instance Dim n => Additive (Vec n) where
  zero = pure 0
  MkVec xs ^+^ MkVec ys = MkVec (V.zipWith (+) xs ys)
  MkVec xs ^-^ MkVec ys = MkVec (V.zipWith (-) xs ys)

instance Dim n => Vectorize (Vec n) where
  fill = pure
  vectorize = unVec
  devectorize' :: V.Vector a -> Either String (Vec n a)
  devectorize' x
    | n == n' = Right (MkVec x)
    | otherwise = Left $ "mkVec: length mismatch, type-level: "
                  ++ show n ++ ", value-level: " ++ show n'
    where
      n = reflectDim (Proxy :: Proxy n)
      n' = V.length x

tvtranspose :: (Dim n, Dim m) => Vec n (Vec m a) -> Vec m (Vec n a)
tvtranspose = T.sequenceA

infixr 5 <|
infixl 5 |>
(<|) :: a -> Vec n a -> Vec (Succ n) a
(<|) x (MkVec xs) = MkVec $ V.cons x xs

(|>) :: Vec n a -> a -> Vec (Succ n) a
(|>) (MkVec xs) x = MkVec $ V.snoc xs x

unVec :: forall n a . Dim n => Vec n a -> V.Vector a
unVec (MkVec x)
  | n == n' = x
  | otherwise = error $ "unVec: length mismatch, type-level: "
                ++ show n ++ ", value-level: " ++ show n'
  where
    n = reflectDim (Proxy :: Proxy n)
    n' = V.length x

mkVec' :: Dim n => [a] -> Vec n a
mkVec' = devectorize . V.fromList

tvlength :: forall n a. Dim n => Vec n a -> Int
tvlength = const $ reflectDim (Proxy :: Proxy n)

tvzip :: Dim n => Vec n a -> Vec n b -> Vec n (a,b)
tvzip x y = devectorize (V.zip (unVec x) (unVec y))

tvzip3 :: Dim n => Vec n a -> Vec n b -> Vec n c -> Vec n (a,b,c)
tvzip3 x y z = devectorize (V.zip3 (unVec x) (unVec y) (unVec z))

tvzip4 :: Dim n => Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n (a,b,c,d)
tvzip4 x y z w = devectorize (V.zip4 (unVec x) (unVec y) (unVec z) (unVec w))

tvzipWith :: Dim n => (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
tvzipWith f x y = devectorize (V.zipWith f (unVec x) (unVec y))

tvzipWith3 :: Dim n => (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
tvzipWith3 f x y z = devectorize (V.zipWith3 f (unVec x) (unVec y) (unVec z))

tvzipWith4 :: Dim n => (a -> b -> c -> d -> e) -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e
tvzipWith4 f x y z u = devectorize (V.zipWith4 f (unVec x) (unVec y) (unVec z) (unVec u))

tvzipWith5 :: Dim n => (a -> b -> c -> d -> e -> f)
              -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f
tvzipWith5 f x0 x1 x2 x3 x4 =
  devectorize (V.zipWith5 f (unVec x0) (unVec x1) (unVec x2) (unVec x3) (unVec x4))

tvzipWith6 :: Dim n => (a -> b -> c -> d -> e -> f -> g)
              -> Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n e -> Vec n f -> Vec n g
tvzipWith6 f x0 x1 x2 x3 x4 x5 =
  devectorize (V.zipWith6 f (unVec x0) (unVec x1) (unVec x2) (unVec x3) (unVec x4) (unVec x5))





tvunzip :: Dim n => Vec n (a,b) -> (Vec n a, Vec n b)
tvunzip v = (devectorize v1, devectorize v2)
  where
    (v1,v2) = V.unzip (unVec v)

tvunzip3 :: Dim n => Vec n (a,b,c) -> (Vec n a, Vec n b, Vec n c)
tvunzip3 v = (devectorize v1, devectorize v2, devectorize v3)
  where
    (v1,v2,v3) = V.unzip3 (unVec v)

tvunzip4 :: Dim n => Vec n (a,b,c,d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
tvunzip4 v = (devectorize v1, devectorize v2, devectorize v3, devectorize v4)
  where
    (v1,v2,v3,v4) = V.unzip4 (unVec v)

tvunzip5 :: Dim n => Vec n (a,b,c,d,e) -> (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
tvunzip5 v = (devectorize v1, devectorize v2, devectorize v3, devectorize v4, devectorize v5)
  where
    (v1,v2,v3,v4,v5) = V.unzip5 (unVec v)

tvhead :: Dim n => Vec n a -> a
tvhead x = case V.length v of
  0 -> error "tvhead: empty"
  _ -> V.head v
  where
    v = unVec x

tvtail :: Dim n => Vec (Succ n) a -> Vec n a
tvtail x = case V.length v of
  0 -> error "tvtail: empty"
  _ -> devectorize $ V.tail v
  where
    v = unVec x

tvlast :: Dim n => Vec n a -> a
tvlast x = case V.length v of
  0 -> error "tvlast: empty"
  _ -> V.last v
  where
    v = unVec x

tvshiftl :: Dim n => Vec n a -> a -> Vec n a
tvshiftl xs x = devectorize $ V.tail (V.snoc (unVec xs) x)

tvshiftr :: Dim n => a -> Vec n a -> Vec n a
tvshiftr x xs = devectorize $ V.init (V.cons x (unVec xs))

instance Show a => Show (Vec n a) where
  showsPrec _ (MkVec v) = showV (V.toList v)
    where
      showV []      = showString "<>"
      showV (x:xs)  = showChar '<' . shows x . showl xs
        where
          showl []      = showChar '>'
          showl (y:ys)  = showChar ',' . shows y . showl ys

data ReifiedDim (s :: *)

retagDim :: (Proxy s -> a) -> proxy (ReifiedDim s) -> a
retagDim f _ = f Proxy
{-# INLINE retagDim #-}

instance Reifies s Int => Dim (ReifiedDim s) where
  reflectDim = retagDim reflect
  {-# INLINE reflectDim #-}

reifyDim :: Int -> (forall (n :: *). Dim n => Proxy n -> r) -> r
reifyDim i f = R.reify i (go f) where
  go :: Reifies n Int => (Proxy (ReifiedDim n) -> a) -> proxy n -> a
  go g _ = g Proxy
{-# INLINE reifyDim #-}

reifyVector :: forall a r. V.Vector a -> (forall (n :: *). Dim n => Vec n a -> r) -> r
reifyVector v f = reifyDim (V.length v) $ \(Proxy :: Proxy n) -> f (devectorize v :: Vec n a)
{-# INLINE reifyVector #-}

tvlinspace :: forall n a . (Dim n, Fractional a) => a -> a -> Vec n a
tvlinspace x0 xf = mkVec' [x0 + h * fromIntegral k  | k <- take n [(0::Int)..]]
  where
    n = reflectDim (Proxy :: Proxy n)
    h = (xf - x0) / fromIntegral (n - 1)
