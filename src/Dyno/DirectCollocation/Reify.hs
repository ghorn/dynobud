{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Dyno.DirectCollocation.Reify
       ( reifyCollTraj
       , reifyCollTrajCov
       ) where

import Linear.V ( Dim )

import Dyno.Vectorize
import Dyno.TypeVecs ( Vec )
import Dyno.View.View
import Dyno.View.Viewable
import qualified Dyno.TypeVecs as TV
import Dyno.DirectCollocation.Types

-- TODO: re-enable the check on output dimension
reifyCollTraj
  :: forall a r x' z' u' p' o' .
     Viewable a
  => (Int,Int,Int,Int,Int,Int,Int)
  -> J (CollTraj x' z' u' p' () ()) a
  -> Vec () (Vec () (J o' a))
  -> (forall x z u p o n deg .
      (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize o, Dim n, Dim deg)
      => J (CollTraj x z u p n deg) a -> Vec n (Vec deg (J (JV o) a)) -> r)
  -> r
reifyCollTraj (nx,nz,nu,np,no,n,deg) (UnsafeJ x) outputs f
  | ntotal /= ntotal' =
      error $ "reifyCollTraj stages dimension mismatch, " ++
        "expected: " ++ show ntotal ++
        "actual : " ++ show ntotal'
--  | nOutsTotal /= nOutsTotal' =
--      error $ "reifyCollTraj outputs dimension mismatch, " ++
--        "expected: " ++ show nOutsTotal ++
--        "actual : " ++ show nOutsTotal'
  | otherwise =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim no $ \(Proxy :: Proxy no) ->
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f
  (mkJ x :: J (CollTraj (Vec nx) (Vec nz) (Vec nu) (Vec np) n deg) a)
  (unsafeCastDim (fmap (unsafeCastDim . fmap unsafeToVec) outputs)
   :: Vec n (Vec deg (J (JV (Vec no)) a)))
  where
    ntotal = 1 + np + n*(nx + deg*(nx + nz + nu)) + nx
    ntotal' = vsize1 x

--    nOutsTotal = n*deg*no :: Int
--    nOutsTotal' = :: Int --vsize1 outs

-- TODO: re-enable the check on output dimension
reifyCollTrajCov
  :: forall a r x' z' u' p' o' sx' .
     Viewable a
  => (Int,Int,Int,Int,Int,Int,Int,Int)
  -> J (CollTrajCov sx' x' z' u' p' () ()) a
  -> Vec () (Vec () (J o' a))
  -> (forall x z u p o sx n deg .
      (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize o, Vectorize sx, Dim n, Dim deg)
      => J (CollTrajCov sx x z u p n deg) a -> Vec n (Vec deg (J (JV o) a)) -> r)
  -> r
reifyCollTrajCov (nsx,nx,nz,nu,np,no,n,deg) (UnsafeJ x) outputs f
  | ntotal /= ntotal' =
      error $ "reifyCollTraj stages dimension mismatch, " ++
        "expected: " ++ show ntotal ++
        "actual : " ++ show ntotal'
--  | nOutsTotal /= nOutsTotal' =
--      error $ "reifyCollTraj outputs dimension mismatch, " ++
--        "expected: " ++ show nOutsTotal ++
--        "actual : " ++ show nOutsTotal'
  | otherwise =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim no $ \(Proxy :: Proxy no) ->
  TV.reifyDim nsx $ \(Proxy :: Proxy nsx) ->
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f
  (mkJ x :: J (CollTrajCov (Vec nsx) (Vec nx) (Vec nz) (Vec nu) (Vec np) n deg) a)
  (unsafeCastDim (fmap (unsafeCastDim . fmap unsafeToVec) outputs)
   :: Vec n (Vec deg (J (JV (Vec no)) a)))
  where
    ncov = (nsx*nsx + nsx) `div` 2
    ntotal = 1 + ncov + np + n*(nx + deg*(nx + nz + nu)) + nx
    ntotal' = vsize1 x

--    nOutsTotal = n*deg*no :: Int
--    nOutsTotal' = :: Int --vsize1 outs

unsafeToVec :: (Viewable a, Dim no) => J dummy a -> J (JV (Vec no)) a
unsafeToVec (UnsafeJ x) = mkJ x

unsafeCastDim :: Dim no => Vec () a -> Vec no a
unsafeCastDim = TV.mkSeq . TV.unSeq
