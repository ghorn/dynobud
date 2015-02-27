{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.View.M
       ( M
       , sparse, dense
       , mm
       , ms
       , vs
       , trans
       , zeros
       , eye
       , diag
       , ones
       , countUp
       , vsplit
       , hsplit
       , vcat
       , hcat
       , vsplit'
       , hsplit'
       , vcat'
       , hcat'
       , hsplitTup
       , hsplitTrip
       , hcatTup
       , hcatTrip
       , vsplitTup
       , vsplitTrip
       , vcatTup
       , vcatTrip
       , row
       , col
       , unrow
       , uncol
       , solve
       , toHMat
       , fromHMat
       , fromHMat'
         -- * hmatrix wrappers
       , rcond
       , rank
       ) where

import qualified Data.Vector as V
import Data.Proxy ( Proxy(..) )
import Casadi.CMatrix ( CMatrix )
import Casadi.DMatrix ( DMatrix, ddata, dsparsify )
import qualified Casadi.CMatrix as CM
import qualified Data.Packed.Matrix as HMat
import qualified Numeric.LinearAlgebra.HMatrix as HMat

import Dyno.View.Unsafe.View ( unJ, mkJ )
import Dyno.View.Unsafe.M ( M(UnsafeM), mkM, mkM', unM )

import Dyno.Vectorize ( Vectorize(..), Id, fill )
import Dyno.TypeVecs ( Vec, Dim(..) )
import Dyno.View.View ( View(..), J, JTuple, JTriple )
import Dyno.View.JV ( JV )
import Dyno.View.JVec ( JVec )
import Dyno.View.Viewable ( Viewable )


-- todo: generalize once casadi 2.3 is ready
sparse :: (View f, View g) => M f g DMatrix -> M f g DMatrix
sparse (UnsafeM m) = mkM (dsparsify m)

dense :: (View f, View g, CMatrix a) => M f g a -> M f g a
dense (UnsafeM m) = mkM (CM.dense m)

mm :: (View f, View h, CMatrix a) => M f g a -> M g h a -> M f h a
mm (UnsafeM m0) (UnsafeM m1) = mkM (CM.mm m0 m1)

ms :: (View f, View h, Viewable a, CMatrix a) => M f g a -> J (JV Id) a -> M f h a
ms (UnsafeM m0) m1 = mkM (m0 * (unJ m1))

vs :: (View f, Viewable a, CMatrix a) => J f a -> J (JV Id) a -> J f a
vs m0 m1 = uncol $ ms (col m0) m1

trans :: (View f, View g, CMatrix a) => M f g a -> M g f a
trans (UnsafeM m) = mkM (CM.trans m)

vsplit ::
  forall f g a .
  (Vectorize f, View g, CMatrix a)
  => M (JV f) g a -> f (M (JV Id) g a)
vsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.vertsplit x nrs
  where
    nr = size (Proxy :: Proxy (JV f))
    nrs = V.fromList [0,1..nr]

vcat ::
  forall f g a .
  (Vectorize f, View g, CMatrix a)
  => f (M (JV Id) g a) -> M (JV f) g a
vcat x = mkM $ CM.vertcat $ V.map unM (vectorize x)

hsplit ::
  forall f g a .
  (View f, Vectorize g, CMatrix a)
  => M f (JV g) a -> g (M f (JV Id) a)
hsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    nc = size (Proxy :: Proxy (JV g))
    ncs = V.fromList [0,1..nc]

hsplitTup ::
  forall f g h a .
  (View f, View g, View h, CMatrix a)
  => M f (JTuple g h) a -> (M f g a, M f h a)
hsplitTup (UnsafeM x) =
  case V.toList (CM.horzsplit x ncs) of
    [g,h] -> (mkM g, mkM h)
    n -> error $ "hsplitTup made a bad split with length " ++ show (length n)
  where
    ng = size (Proxy :: Proxy g)
    nh = size (Proxy :: Proxy h)
    ncs = V.fromList [0,ng,ng+nh]

hcatTup ::
  forall f g h a .
  (View f, View g, View h, CMatrix a)
  => M f g a -> M f h a -> M f (JTuple g h) a
hcatTup (UnsafeM x) (UnsafeM y) = mkM (CM.horzcat (V.fromList [x,y]))

hsplitTrip ::
  forall f g h j a .
  (View f, View g, View h, View j, CMatrix a)
  => M f (JTriple g h j) a -> (M f g a, M f h a, M f j a)
hsplitTrip (UnsafeM x) =
  case V.toList (CM.horzsplit x ncs) of
    [g,h,j] -> (mkM g, mkM h, mkM j)
    n -> error $ "hsplitTrip made a bad split with length " ++ show (length n)
  where
    ng = size (Proxy :: Proxy g)
    nh = size (Proxy :: Proxy h)
    nj = size (Proxy :: Proxy j)
    ncs = V.fromList [0,ng,ng+nh,ng+nh+nj]

hcatTrip ::
  forall f g1 g2 g3 a .
  (View f, View g1, View g2, View g3, CMatrix a)
  => M f g1 a -> M f g2 a -> M f g3 a -> M f (JTriple g1 g2 g3) a
hcatTrip (UnsafeM x) (UnsafeM y) (UnsafeM z) = mkM (CM.horzcat (V.fromList [x,y,z]))

hcat ::
  forall f g a .
  (View f, Vectorize g, CMatrix a)
  => g (M f (JV Id) a) -> M f (JV g) a
hcat x = mkM $ CM.horzcat $ V.map unM (vectorize x)

vcat' ::
  forall f g n a .
  (View f, View g, Dim n, CMatrix a)
  => Vec n (M f g a) -> M (JVec n f) g a
vcat' x = mkM $ CM.vertcat $ V.map unM (vectorize x)

vsplit' ::
  forall f g n a .
  (View f, View g, Dim n, CMatrix a)
  => M (JVec n f) g a -> Vec n (M f g a)
vsplit' (UnsafeM x)
  | n == 0 = fill zeros
  | nr == 0 = fill zeros
  | otherwise = fmap mkM $ devectorize $ CM.vertsplit x nrs
  where
    n = reflectDim (Proxy :: Proxy n)
    nr = size (Proxy :: Proxy f)
    nrs = V.fromList [0,nr..n*nr]

vsplitTup ::
  forall f g h a .
  (View f, View g, View h, CMatrix a)
  => M (JTuple f g) h a -> (M f h a, M g h a)
vsplitTup (UnsafeM x) =
  case V.toList (CM.vertsplit x ncs) of
    [f,g] -> (mkM f, mkM g)
    n -> error $ "vsplitTup made a bad split with length " ++ show (length n)
  where
    nf = size (Proxy :: Proxy f)
    ng = size (Proxy :: Proxy g)
    ncs = V.fromList [0,nf,nf+ng]

vcatTup ::
  forall f g h a .
  (View f, View g, View h, CMatrix a)
  => M f h a -> M g h a -> M (JTuple f g) h a
vcatTup (UnsafeM x) (UnsafeM y) = mkM (CM.vertcat (V.fromList [x,y]))

vsplitTrip ::
  forall f g h j a .
  (View f, View g, View h, View j, CMatrix a)
  => M (JTriple f g h) j a -> (M f j a, M g j a, M h j a)
vsplitTrip (UnsafeM x) =
  case V.toList (CM.vertsplit x ncs) of
    [f,g,h] -> (mkM f, mkM g, mkM h)
    n -> error $ "vsplitTrip made a bad split with length " ++ show (length n)
  where
    nf = size (Proxy :: Proxy f)
    ng = size (Proxy :: Proxy g)
    nh = size (Proxy :: Proxy h)
    ncs = V.fromList [0,nf,nf+ng,nf+ng+nh]

vcatTrip ::
  forall f1 f2 f3 h a .
  (View f1, View f2, View f3, View h, CMatrix a)
  => M f1 h a -> M f2 h a -> M f3 h a -> M (JTriple f1 f2 f3) h a
vcatTrip (UnsafeM x) (UnsafeM y) (UnsafeM z) = mkM (CM.vertcat (V.fromList [x,y,z]))

hcat' ::
  forall f g n a .
  (View f, View g, Dim n, CMatrix a)
  => Vec n (M f g a) -> M f (JVec n g) a
hcat' x = mkM $ CM.horzcat $ V.map unM (vectorize x)

hsplit' ::
  forall f g n a .
  (View f, View g, Dim n, CMatrix a)
  => M f (JVec n g) a -> Vec n (M f g a)
hsplit' (UnsafeM x)
  | n == 0 = fill zeros
  | nc == 0 = fill zeros
  | otherwise = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    n = reflectDim (Proxy :: Proxy n)
    nc = size (Proxy :: Proxy g)
    ncs = V.fromList [0,nc..n*nc]

zeros :: forall f g a . (View f, View g, CMatrix a) => M f g a
zeros = mkM z
  where
    z = CM.zeros (rows, cols)
    rows = size (Proxy :: Proxy f)
    cols = size (Proxy :: Proxy g)

eye :: forall f a . (View f, CMatrix a) => M f f a
eye = mkM z
  where
    z = CM.eye n
    n = size (Proxy :: Proxy f)

diag :: forall f a . (View f, Viewable a, CMatrix a) => J f a -> M f f a
diag x = mkM z
  where
    z = CM.diag (unJ x)

ones :: forall f g a . (View f, View g, CMatrix a) => M f g a
ones = mkM z
  where
    z = CM.ones (rows, cols)
    rows = size (Proxy :: Proxy f)
    cols = size (Proxy :: Proxy g)

-- this is mainly for unit tests
countUp :: forall f g a . (View f, View g, CMatrix a) => M f g a
countUp = mkM z
  where
    z = CM.vertcat (V.fromList [CM.horzcat (V.fromList [ fromIntegral (c + cols*r)
                                                       | c <- [0..(cols-1)]
                                                       ])
                               | r <- [0..(rows-1)]
                               ])
    rows = size (Proxy :: Proxy f)
    cols = size (Proxy :: Proxy g)

row :: (CMatrix a, View f, Viewable a) => J f a -> M (JV Id) f a
row = mkM . CM.trans . unJ

col :: (CMatrix a, View f, Viewable a) => J f a -> M f (JV Id) a
col = mkM . unJ

unrow :: (Viewable a, CMatrix a, View f) => M (JV Id) f a -> J f a
unrow (UnsafeM x) = mkJ (CM.trans x)

uncol :: (Viewable a, CMatrix a, View f) => M f (JV Id) a -> J f a
uncol (UnsafeM x) = mkJ x

solve :: (View g, View h, CMatrix a) => M f g a -> M f h a -> M g h a
solve (UnsafeM x) (UnsafeM y) = mkM (CM.solve x y)

toHMat :: forall n m
       . (View n, View m)
       => M n m DMatrix -> HMat.Matrix Double
toHMat (UnsafeM d) = HMat.trans $ (m HMat.>< n) (V.toList v)
  where
    v = ddata (CM.dense d)
    n = size (Proxy :: Proxy n)
    m = size (Proxy :: Proxy m)

fromHMat :: (View f, View g) => HMat.Matrix Double -> M f g DMatrix
fromHMat x = case fromHMat' x of
  Right x' -> x'
  Left msg -> error msg

fromHMat' :: (View f, View g) => HMat.Matrix Double -> Either String (M f g DMatrix)
fromHMat' = mkM' . CM.vertcat . V.fromList . fmap (CM.trans . CM.fromDVector . V.fromList) . HMat.toLists

rcond :: (View f, View g) => M f g DMatrix -> Double
rcond = HMat.rcond . toHMat

rank :: (View f, View g) => M f g DMatrix -> Int
rank = HMat.rank . toHMat
