{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Dyno.DirectCollocation.Reify
       ( reifyCollTraj
       ) where

import Linear.V ( Dim )
import Data.Proxy ( Proxy(..) )

import Dyno.Vectorize
import Dyno.TypeVecs ( Vec )
import Dyno.View.View
import Dyno.View.Viewable
import qualified Dyno.TypeVecs as TV
import Dyno.DirectCollocation.Types

reifyCollTraj
  :: forall a r x' z' u' p' s' . Viewable a =>
     (Int,Int,Int,Int,Int,Int,Int)
  -> J (CollTraj x' z' u' p' s' () ()) a
  -> (forall x z u p s n deg . (Vectorize x,Vectorize z,Vectorize u,Vectorize p, View s, Dim n, Dim deg) =>
      J (CollTraj x z u p s n deg) a -> r)
  -> r
reifyCollTraj (nx,nz,nu,np,ns,n,deg) (UnsafeJ x) f
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
  f (mkJ x :: J (CollTraj (Vec nx) (Vec nz) (Vec nu) (Vec np) (JVec ns S) n deg) a)
  where
    ncov = (ns*ns + ns) `div` 2 
    ntotal = 1 + ncov + np + n*(nx + deg*(nx + nz + nu)) + nx
    ntotal' = vsize1 x
