{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Dyno.View.M
       ( M
       , sparse, dense
       , mm
       , ms
       , sm
       , trans
       , zeros
       , eye
       , diag
       , takeDiag
       , ones
       , countUp
       , hcat
       , hcat'
       , hcat''
       , hsplit
       , hsplit'
       , hsplit''
       , vcat
       , vcat'
       , vcat''
       , vsplit
       , vsplit'
       , vsplit''
       , blockcat
       , hsplitTup
       , hsplitTrip
       , hsplitQuad
       , hcatTup
       , hcatTrip
       , hcatQuad
       , vsplitTup
       , vsplitTrip
       , vsplitQuad
       , vcatTup
       , vcatTrip
       , vcatQuad
       , solve
       , solve'
       , sum1, sum2
       , fromDM
       , toHMat
       , fromHMat
       , fromHMat'
       , blocksplit
       , flatten
       , unflatten
       , jflatten
       , junflatten
       , repmat
       , inv
       , gradient, jacobian, hessian
         -- * hmatrix wrappers
       , rcond
       , rank
       ) where

import Data.Proxy ( Proxy(..) )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as HMat

import Casadi.DM ( DM, dnonzeros, dsparsify )
import Casadi.GenericType ( GType )
import Casadi.Matrix ( CMatrix, SMatrix )
import qualified Casadi.Matrix as CM
import Casadi.Viewable ( Viewable(..) )

import Dyno.View.Unsafe ( M(UnsafeM), mkM, mkM', unM )
import Dyno.View.Vectorize ( Vectorize(..), Id, (:.), fill, devectorize, vlength )
import Dyno.TypeVecs ( Vec, Dim, reflectDim )
import qualified Dyno.TypeVecs as TV
import Dyno.View.View ( View(..), J, S, JV, JTuple, JTriple, JQuad )
import Dyno.View.JVec ( JVec )


-- todo: generalize once casadi 2.3 is ready
sparse :: (View f, View g) => M f g DM -> M f g DM
sparse (UnsafeM m) = mkM (dsparsify m)

dense :: (View f, View g, CMatrix a) => M f g a -> M f g a
dense (UnsafeM m) = mkM (CM.densify m)

mm :: (View f, View h, CMatrix a) => M f g a -> M g h a -> M f h a
mm (UnsafeM m0) (UnsafeM m1) = mkM (CM.mtimes m0 m1)

ms :: (View f, View g, CMatrix a) => M f g a -> S a -> M f g a
ms m0 m1 = mkM $ (unM m0) * (unM m1)

sm :: (View f, View g, CMatrix a) => S a -> M f g a -> M f g a
sm m0 m1 = mkM $ (unM m0) * (unM m1)

trans :: (View f, View g, CMatrix a) => M f g a -> M g f a
trans (UnsafeM m) = mkM (CM.trans m)

gradient :: (View f, SMatrix a) => S a -> J f a -> J f a
gradient (UnsafeM expr) (UnsafeM args) = mkM (CM.gradient expr args)

jacobian :: (View f, View x, SMatrix a) => J f a -> J x a -> M f x a
jacobian (UnsafeM expr) (UnsafeM args) = mkM (CM.jacobian expr args)

hessian :: (View x, SMatrix a) => S a -> J x a -> (M x x a, J x a)
hessian (UnsafeM expr) (UnsafeM args) = (mkM hess, mkM grad)
  where
    (hess, grad) = CM.hessian expr args

vcat ::
  forall f g a .
  (Vectorize f, View g, Viewable a)
  => f (M (JV Id) g a) -> M (JV f) g a
vcat x = mkM $ vvertcat $ V.map unM (vectorize x)

vcat' ::
  forall f g n a .
  (View f, View g, Dim n, CMatrix a)
  => Vec n (M f g a) -> M (JVec n f) g a
vcat' x = mkM $ CM.vertcat $ V.map unM (vectorize x)

vcat'' ::
  forall f g h a .
  (Vectorize f, Vectorize g, View h, Viewable a)
  => f (M (JV g) h a) -> M (JV (f :. g)) h a
vcat'' x = mkM $ vvertcat $ V.map unM (vectorize x)

vsplit ::
  forall f g a .
  (Vectorize f, View g, Viewable a)
  => M (JV f) g a -> f (M (JV Id) g a)
vsplit (UnsafeM x) = fmap mkM $ devectorize $ vvertsplit x nrs
  where
    nr = size (Proxy :: Proxy (JV f))
    nrs = V.fromList [0,1..nr]

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

vsplit'' ::
  forall f g h a .
  (Vectorize f, Vectorize g, View h, Viewable a)
  => M (JV (f :. g)) h a -> f (M (JV g) h a)
vsplit'' (UnsafeM x) = fmap mkM $ devectorize $ vvertsplit x nrs
  where
    nf = size (Proxy :: Proxy (JV f))
    ng = size (Proxy :: Proxy (JV g))
    nrs = V.fromList [0,ng..(nf*ng)]

hcat ::
  forall f g a .
  (View f, Vectorize g, CMatrix a)
  => g (M f (JV Id) a) -> M f (JV g) a
hcat x = mkM $ CM.horzcat $ V.map unM (vectorize x)

hcat' ::
  forall f g n a .
  (View f, View g, Dim n, CMatrix a)
  => Vec n (M f g a) -> M f (JVec n g) a
hcat' x = mkM $ CM.horzcat $ V.map unM (vectorize x)

hcat'' ::
  forall f g h a .
  (View f, Vectorize g, Vectorize h, CMatrix a)
  => g (M f (JV h) a) -> M f (JV (g :. h)) a
hcat'' x = mkM $ CM.horzcat $ V.map unM (vectorize x)

hsplit ::
  forall f g a .
  (View f, Vectorize g, CMatrix a)
  => M f (JV g) a -> g (M f (JV Id) a)
hsplit (UnsafeM x) = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    nc = size (Proxy :: Proxy (JV g))
    ncs = V.fromList [0,1..nc]

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

hsplit'' ::
  forall f g h a .
  (View f, Vectorize g, Vectorize h, CMatrix a)
  => M f (JV (g :. h)) a -> g (M f (JV h) a)
hsplit'' (UnsafeM x) = fmap mkM $ devectorize $ CM.horzsplit x ncs
  where
    ng = vlength (Proxy :: Proxy g)
    nh = vlength (Proxy :: Proxy h)
    ncs = V.fromList [0,nh..(ng*nh)]

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

hsplitQuad ::
  forall f g0 g1 g2 g3 a .
  (View f, View g0, View g1, View g2, View g3, CMatrix a)
  => M f (JQuad g0 g1 g2 g3) a -> (M f g0 a, M f g1 a, M f g2 a, M f g3 a)
hsplitQuad (UnsafeM x) =
  case V.toList (CM.horzsplit x ncs) of
    [g0,g1,g2,g3] -> (mkM g0, mkM g1, mkM g2, mkM g3)
    n -> error $ "hsplitQuad made a bad split with length " ++ show (length n)
  where
    ng0 = size (Proxy :: Proxy g0)
    ng1 = size (Proxy :: Proxy g1)
    ng2 = size (Proxy :: Proxy g2)
    ng3 = size (Proxy :: Proxy g3)
    ncs = V.fromList [0,ng0,ng0+ng1,ng0+ng1+ng2,ng0+ng1+ng2+ng3]

hcatQuad ::
  forall f g0 g1 g2 g3 a .
  (View f, View g0, View g1, View g2, View g3, CMatrix a)
  => M f g0 a -> M f g1 a -> M f g2 a -> M f g3 a -> M f (JQuad g0 g1 g2 g3) a
hcatQuad (UnsafeM x0) (UnsafeM x1) (UnsafeM x2) (UnsafeM x3) =
  mkM (CM.horzcat (V.fromList [x0,x1,x2,x3]))

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

vsplitQuad ::
  forall f0 f1 f2 f3 h a .
  (View f0, View f1, View f2, View f3, View h, CMatrix a)
  => M (JQuad f0 f1 f2 f3) h a -> (M f0 h a, M f1 h a, M f2 h a, M f3 h a)
vsplitQuad (UnsafeM x) =
  case V.toList (CM.vertsplit x ncs) of
    [f0,f1,f2,f3] -> (mkM f0, mkM f1, mkM f2, mkM f3)
    n -> error $ "vsplitQuad made a bad split with length " ++ show (length n)
  where
    nf0 = size (Proxy :: Proxy f0)
    nf1 = size (Proxy :: Proxy f1)
    nf2 = size (Proxy :: Proxy f2)
    nf3 = size (Proxy :: Proxy f3)
    ncs = V.fromList [0,nf0,nf0+nf1,nf0+nf1+nf2,nf0+nf1+nf2+nf3]

vcatQuad ::
  forall f0 f1 f2 f3 h a .
  (View f0, View f1, View f2, View f3, View h, CMatrix a)
  => M f0 h a -> M f1 h a -> M f2 h a -> M f3 h a -> M (JQuad f0 f1 f2 f3) h a
vcatQuad (UnsafeM x0) (UnsafeM x1) (UnsafeM x2) (UnsafeM x3) =
  mkM (CM.vertcat (V.fromList [x0,x1,x2,x3]))

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

diag :: forall f a . (View f, CMatrix a) => J f a -> M f f a
diag x = mkM z
  where
    z = CM.diag (unM x)

takeDiag :: forall f a . (View f, CMatrix a) => M f f a -> J f a
takeDiag m = mkM $ CM.diag (unM m)

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

solve :: (View g, View h, CMatrix a)
         => M f g a -> M f h a -> String -> M.Map String GType
         -> M g h a
solve (UnsafeM x) (UnsafeM y) n options = mkM (CM.solve x y n options)

-- TODO(greg): deprecate this?
solve' :: (View g, View h, CMatrix a) => M f g a -> M f h a -> M g h a
solve' (UnsafeM x) (UnsafeM y) = mkM (CM.solve' x y)

toHMat :: forall n m
       . (View n, View m)
       => M n m DM -> HMat.Matrix Double
toHMat (UnsafeM d) = HMat.tr' $ (m HMat.>< n) (V.toList v)
  where
    v = dnonzeros (CM.densify d)
    n = size (Proxy :: Proxy n)
    m = size (Proxy :: Proxy m)

fromHMat :: (View f, View g, CMatrix a) => HMat.Matrix Double -> M f g a
fromHMat x = case fromHMat' x of
  Right x' -> x'
  Left msg -> error msg

fromHMat' :: (View f, View g, CMatrix a) => HMat.Matrix Double -> Either String (M f g a)
fromHMat' = mkM' . CM.fromDM . CM.vertcat . V.fromList . fmap (CM.trans . CM.fromDVector . V.fromList) . HMat.toLists

rcond :: (View f, View g) => M f g DM -> Double
rcond = HMat.rcond . toHMat

rank :: (View f, View g) => M f g DM -> Int
rank = HMat.rank . toHMat

fromDM :: (CM.CMatrix a, View f, View g)
               => M f g DM -> M f g a
fromDM = mkM . CM.fromDM . unM


repmat :: forall f g a . (CM.CMatrix a, View f, View g) => S a -> M f g a
repmat = mkM . flip CM.repmat (nx, ny) . unM
  where
    nx = size (Proxy :: Proxy f)
    ny = size (Proxy :: Proxy g)

-- | Break a typed matrix into a list of its elements given by the
-- sizes of the View constructor.
-- For example:
-- > data F a = F (J (JV V2) a) (J (JV V3) a)
-- > data G a = G (J (JV V4) a) (J (JV V5) a) (J (JV V6) a)
-- > x :: M F G DM
-- > x = ...
-- >
-- > y :: Vector (Vector DM)
-- > y = blockSplit x
--
-- > -- y is a 2x3 group with DM dimensions:
-- > --   [ [ (2,4), (2,5), (2,6) ]
-- > --   , [ (3,4), (3,5), (3,6) ]
-- > --   ]
blocksplit :: forall f g a . (View f, View g, CMatrix a) => M f g a -> Vector (Vector a)
blocksplit (UnsafeM m) = CM.blocksplit m vsizes hsizes
  where
    vsizes = V.fromList $ 0 : (F.toList (sizes 0 (Proxy :: Proxy f)))
    hsizes = V.fromList $ 0 : (F.toList (sizes 0 (Proxy :: Proxy g)))

blockcat :: forall f g a . (Vectorize f, Vectorize g, CMatrix a) => f (g (S a)) -> M (JV f) (JV g) a
blockcat = vcat . fmap hcat

-- TODO(greg):
-- blockSplit :: forall f g a . (Vectorize f, Vectorize g) => M (JV f) (JV g) DMatrix -> f (g Double)
-- blockCat :: forall f g a . (Vectorize f, Vectorize g) => f (g Double) -> M (JV f) (JV g) DMatrix

sum1 :: (View g, CMatrix a) => M f g a -> M (JV Id) g a
sum1 (UnsafeM x) = mkM (CM.sum1 x)

sum2 :: (View f, CMatrix a) => M f g a -> M f (JV Id) a
sum2 (UnsafeM x) = mkM (CM.sum2 x)

inv :: (View f, CMatrix a) => M f f a -> M f f a
inv (UnsafeM x) = mkM (CM.inv x)

-- | reshape a column-major matrix into a vector
jflatten ::
  forall n f a
  . (Dim n, View f, CMatrix a)
  => M f (JVec n (JV Id)) a -> J (JVec n f) a
jflatten (UnsafeM x) = mkM (CM.reshape x (nx*ny, 1))
  where
    nx = size (Proxy :: Proxy f)
    ny = TV.reflectDim (Proxy :: Proxy n)

-- | reshape a vector into a column-major matrix
junflatten ::
  forall n f a
  . (Dim n, View f, CMatrix a)
  => J (JVec n f) a -> M f (JVec n (JV Id)) a
junflatten (UnsafeM x) = mkM (CM.reshape x (nx, ny))
  where
    nx = size (Proxy :: Proxy f)
    ny = TV.reflectDim (Proxy :: Proxy n)

-- | reshape a column-major matrix into a vector
flatten ::
  forall f g a
  . (Vectorize f, Vectorize g, CMatrix a)
  => M (JV f) (JV g) a -> J (JV (g :. f)) a
flatten (UnsafeM x) = mkM (CM.reshape x (nx*ny, 1))
  where
    nx = vlength (Proxy :: Proxy f)
    ny = vlength (Proxy :: Proxy g)

-- | reshape a vector into a column-major matrix
unflatten ::
  forall f g a
  . (Vectorize f, Vectorize g, CMatrix a)
  => J (JV (g :. f)) a -> M (JV f) (JV g) a
unflatten (UnsafeM x) = mkM (CM.reshape x (nx, ny))
  where
    nx = vlength (Proxy :: Proxy f)
    ny = vlength (Proxy :: Proxy g)
