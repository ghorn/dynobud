{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Vectorize ( Vectorize(..)
                 , Generic1
                 , GVectorize(..)
                 , None(..)
                 , vlength
                 , fill
                 ) where


import qualified GHC.Generics as G
import GHC.Generics hiding ( D1, (:+:), (:*:) )
import qualified Data.Sequence as S

import TypeNats
import TypeVecs

data None a = None deriving (Generic1, Functor, Show)
instance Vectorize None D0

fill :: Vectorize f n => a -> f a
fill x = fmap (const x) empty

--type family Nat (f :: * -> *) :: *
--type instance Nat None = D0

-- | fmap f == devectorize . (V.map f) . vectorize
class (Functor f, NaturalT n) => Vectorize f n | f -> n where
  vectorize :: f a -> Vec n a
  devectorize :: Vec n a -> f a
  empty :: f ()

  default vectorize :: (Generic1 f, GVectorize (Rep1 f) n) => f a -> Vec n a
  vectorize f = gvectorize (from1 f)

  default devectorize :: (Generic1 f, GVectorize (Rep1 f) n) => Vec n a -> f a
  devectorize f = to1 (gdevectorize f)

  default empty :: (Generic1 f, GVectorize (Rep1 f) n) => f ()
  empty = to1 gempty

class NaturalT n => GVectorize f n | f -> n where
  gvectorize :: f a -> Vec n a
  gdevectorize :: Vec n a -> f a
  gempty :: f ()

instance (NaturalT n) => Vectorize (Vec n) n where
  vectorize = id
  devectorize = id
  empty = ret
    where
      ret = mkSeq $ S.replicate k ()
      k = tvlength ret

instance (NaturalT n) => GVectorize (Vec n) n where
  gvectorize = id
  gdevectorize = id
  gempty = ret
    where
      ret = tvreplicate k ()
      k = tvlengthT ret

vlength :: Vectorize f n => f a -> Int
vlength = S.length . unSeq . vectorize . (empty `asFunctorOf`)
  where
    asFunctorOf :: f a -> f b -> f a
    asFunctorOf x _ = x

vlengthT :: Vectorize f n => f a -> n
vlengthT _ = undefined

gvlengthT :: GVectorize f n => f a -> n
gvlengthT _ = undefined


instance (GVectorize f nf, GVectorize g ng, NaturalT n, (nf :+: ng) ~ n) =>
         GVectorize (f G.:*: g) n where
  gvectorize (f G.:*: g) = gvectorize f <++> gvectorize g
  gdevectorize v0s = f0 G.:*: f1
    where
      f0 = gdevectorize v0
      f1 = gdevectorize v1

      (v0,v1) = tvsplitAt (gvlengthT f0) v0s

  gempty = gempty G.:*: gempty

-- Metadata (constructor name, etc)
instance GVectorize f n => GVectorize (M1 i c f) n where
  gvectorize = gvectorize . unM1
  gdevectorize = M1 . gdevectorize
  gempty = M1 gempty

instance GVectorize Par1 D1 where
  gvectorize = tvsingleton . unPar1
  gdevectorize v = Par1 (tvindex d0 v)
--  gdevectorize v = case V.toList v of
--    [] -> error "gdevectorize Par1: got empty list"
--    [x] -> Par1 x
--    xs -> error $ "gdevectorize Par1: got non-1 length: " ++ show (length xs)
  gempty = Par1 ()

instance GVectorize U1 D0 where
  gvectorize = const tvempty
  gdevectorize = const U1
  gempty = U1

-- Constants, additional parameters, and rank-1 recursion
instance Vectorize f n => GVectorize (Rec1 f) n where
  gvectorize = vectorize . unRec1
  gdevectorize = Rec1 . devectorize
  gempty = Rec1 empty

instance (Vectorize f nf, GVectorize g ng, NaturalT n, (ng :*: nf) ~ n) => GVectorize (f :.: g) n where
  gempty = Comp1 ret
    where
      ret = devectorize $ tvreplicate k gempty
      k = vlengthT ret
  gvectorize = tvconcatMap gvectorize . (vectorize . unComp1)
  gdevectorize v = Comp1 ret
    where
--      ret :: f (g a)
      ret = devectorize vs

      kf = vlengthT ret
      kg = gvlengthT (insideOf vs)

      insideOf :: Vec nf (g a) -> g a
      insideOf _ = undefined

--      vs :: Vec nf (g a)
      vs = fmap gdevectorize (tvsplitsAt kg kf v {-:: Vec nf (Vec ng a)-} )
