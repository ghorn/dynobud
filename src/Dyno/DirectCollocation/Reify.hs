{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Dyno.DirectCollocation.Reify
       ( reifyCollPoint
       , reifyCollStage
       , reifyCollTraj
       , reifyCollTraj'
       ) where

import Linear.V ( Dim )
import Data.Proxy ( Proxy(..) )

import Dyno.View.View
import Dyno.View.Viewable
import Dyno.Cov
import qualified Dyno.TypeVecs as TV
import Dyno.DirectCollocation.Types

reifyCollPoint
  :: forall a r x' z' u' . Viewable a =>
     CollPoint x' z' u' a ->
     (forall x z u . (View x, View z, View u) => CollPoint x z u a -> r) ->
     r
reifyCollPoint (CollPoint (UnsafeJ x) (UnsafeJ z) (UnsafeJ u)) f =
  TV.reifyDim (vsize1 x) $ \(Proxy :: Proxy nx) ->
  TV.reifyDim (vsize1 z) $ \(Proxy :: Proxy nz) ->
  TV.reifyDim (vsize1 u) $ \(Proxy :: Proxy nu) ->
  f (CollPoint (mkJ x :: J (Jec nx S) a) (mkJ z :: J (Jec nz S) a) (mkJ u :: J (Jec nu S) a))

reifyCollStage
  :: forall a r x' z' u' . Viewable a =>
     (Int,Int,Int,Int) -> CollStage x' z' u' () a ->
     (forall x z u deg . (View x, View z, View u, Dim deg) => CollStage x z u deg a -> r) ->
     r
reifyCollStage ns@(nx, nz, nu, deg) (CollStage (UnsafeJ x0) (UnsafeJ points)) f
  | nx /= nx' = error $ "reifyCollStage x0 dimension mismatch, expected " ++
                show nx ++ ", actual " ++ show nx'
  | ncps /= ncps'= error $ "reifyCollStage coll points dimension mismatch, " ++
                   "given (nx,nz,nu,deg): " ++ show ns ++
                   ", expected " ++ show ncps ++
                   ", actual " ++ show ncps'
  | otherwise =
      TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
      TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
      TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
      TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
      f (CollStage
         (mkJ x0 :: J (Jec nx S) a)
         (mkJ points :: J (Jec deg (CollPoint (Jec nx S) (Jec nz S) (Jec nu S))) a)
        )
  where
    nx' = vsize1 x0
    ncps = (nx + nz + nu)*deg
    ncps' = vsize1 points


reifyCollTraj
  :: forall a r x' z' u' p' s' . Viewable a =>
     (Int,Int,Int,Int,Int,Int,Int)
  -> CollTraj x' z' u' p' s' () () a
  -> (forall x z u p s n deg . (View x,View z,View u,View p, View s, Dim n, Dim deg) =>
      CollTraj x z u p s n deg a -> r)
  -> r
reifyCollTraj (nx,nz,nu,np,ns,n,deg) (CollTraj (UnsafeJ endTime) (UnsafeJ cov) (UnsafeJ params) (UnsafeJ stages) (UnsafeJ xf)) f
  | nx /= nx' = error $ "reifyCollTraj x dimension mismatch, expected: " ++ show nx ++
                ", actual: " ++ show nx'
  | np /= np' = error $ "reifyCollTraj param dimension mismatch, expected: " ++ show np ++
                ", actual: " ++ show np'
  | 1 /= nEndtime' = error $ "reifyCollTraj endTime dimension mismatch, expected: 1" ++
                     ", actual: " ++ show nEndtime'
  | ncov /= ncov' = error $ "reifyCollTraj covariance dimension mismatch, " ++
                    "expected dimension: " ++ show ns ++
                    "expected ncov: " ++ show ncov ++
                    "actual ncov: " ++ show ncov'
  | nstages /= nstages' = error $ "reifyCollTraj stages dimension mismatch, " ++
                          "expected: " ++ show nstages ++
                          "actual : " ++ show nstages'
  | otherwise =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim ns $ \(Proxy :: Proxy ns) ->
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f (CollTraj
     (mkJ endTime :: J S a)
     (mkJ cov :: J (Cov (Jec ns S)) a)
     (mkJ params :: J (Jec np S) a)
     (mkJ stages :: J (Jec n (CollStage (Jec nx S) (Jec nz S) (Jec nu S) deg)) a)
     (mkJ xf :: J (Jec nx S) a)
    )
  where
    nx' = vsize1 xf
    ncov' = vsize1 cov
    ncov = (ns*ns + ns) `div` 2 
    nEndtime' = vsize1 endTime
    np' = vsize1 params
    nstages = n*(nx + deg*(nx + nz + nu))
    nstages' = vsize1 stages



reifyCollTraj'
  :: forall a r x' z' u' p' s' . Viewable a =>
     (Int,Int,Int,Int,Int,Int,Int)
  -> J (CollTraj x' z' u' p' s' () ()) a
  -> (forall x z u p s n deg . (View x,View z,View u,View p, View s, Dim n, Dim deg) =>
      J (CollTraj x z u p s n deg) a -> r)
  -> r
reifyCollTraj' (nx,nz,nu,np,ns,n,deg) (UnsafeJ x) f
  | ntotal /= ntotal' = error $ "reifyCollTraj' stages dimension mismatch, " ++
                          "expected: " ++ show ntotal ++
                          "actual : " ++ show ntotal'
  | otherwise =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim ns $ \(Proxy :: Proxy ns) ->
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f (mkJ x :: J (CollTraj (Jec nx S) (Jec nz S) (Jec nu S) (Jec np S) (Jec ns S) n deg) a)
  where
    ncov = (ns*ns + ns) `div` 2 
    ntotal = 1 + ncov + np + n*(nx + deg*(nx + nz + nu)) + nx
    ntotal' = vsize1 x
