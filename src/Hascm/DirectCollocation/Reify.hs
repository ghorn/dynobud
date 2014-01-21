{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Hascm.DirectCollocation.Reify
       ( reifyCollPoint
       , reifyCollStage
       , reifyCollTraj
       ) where

import qualified Data.Vector as V
import Linear.V ( Dim )
import Data.Proxy ( Proxy(..) )

import Hascm.Vectorize
import qualified Hascm.TypeVecs as TV
import Hascm.TypeVecs ( Vec )
import Hascm.DirectCollocation.Types

reifyCollPoint
  :: forall a r .
     CollPoint V.Vector V.Vector V.Vector a ->
     (forall x z u . (Vectorize x, Vectorize z, Vectorize u) => CollPoint x z u a -> r) ->
     r
reifyCollPoint (CollPoint x z u) f =
  TV.reifyDim (V.length x) $ \(Proxy :: Proxy nx) ->
  TV.reifyDim (V.length z) $ \(Proxy :: Proxy nz) ->
  TV.reifyDim (V.length u) $ \(Proxy :: Proxy nu) ->
  f (CollPoint (devectorize x :: Vec nx a) (devectorize z :: Vec nz a) (devectorize u :: Vec nu a))

reifyCollStage
  :: forall a r .
     CollStage V.Vector V.Vector V.Vector Int a ->
     (forall x z u deg . (Vectorize x, Vectorize z, Vectorize u, Dim deg) => CollStage x z u deg a -> r) ->
     r
reifyCollStage (CollStage x0 points') f =
  TV.reifyDim (V.length x1) $ \(Proxy :: Proxy nx) ->
  TV.reifyDim (V.length z1) $ \(Proxy :: Proxy nz) ->
  TV.reifyDim (V.length u1) $ \(Proxy :: Proxy nu) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f (CollStage
     (devectorize x0 :: Vec nx a)
     (devectorize (V.map (\(CollPoint x z u) -> (CollPoint (devectorize x) (devectorize z) (devectorize u))) points) :: Vec deg (CollPoint (Vec nx) (Vec nz) (Vec nu) a))
    )
  where
    points = TV.unVec points'
    deg = V.length points
    CollPoint x1 z1 u1 = V.head points

reifyCollTraj
  :: forall a r.
     CollTraj V.Vector V.Vector V.Vector V.Vector Int Int a
  -> (forall x z u p n deg . (Vectorize x,Vectorize z,Vectorize u,Vectorize p, Dim n, Dim deg) =>
      CollTraj x z u p n deg a -> r)
  -> r
reifyCollTraj (CollTraj endTime params stages' xf) f =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
  f (CollTraj
     endTime
     (devectorize params :: Vec np a)
     (devectorize
      (V.map (\(CollStage x0 points'') ->
               CollStage
               (devectorize x0)
               (devectorize (V.map (\(CollPoint x z u) ->
                                     CollPoint
                                     (devectorize x)
                                     (devectorize z)
                                     (devectorize u)
                                   ) (TV.unVec points'')
                            )
               )
             ) stages) :: Vec n (CollStage (Vec nx) (Vec nz) (Vec nu) deg a))
     (devectorize xf :: Vec nx a)
    )
  where
    nx = V.length x1
    nz = V.length z1
    nu = V.length u1
    np = V.length params
    n = V.length stages
    deg = V.length points

    stages = TV.unVec stages' :: V.Vector (CollStage V.Vector V.Vector V.Vector Int a)
    points = TV.unVec points' :: V.Vector (CollPoint V.Vector V.Vector V.Vector a)

    CollStage _ points' = V.head stages :: CollStage V.Vector V.Vector V.Vector Int a
    CollPoint x1 z1 u1 = V.head points :: CollPoint V.Vector V.Vector V.Vector a
