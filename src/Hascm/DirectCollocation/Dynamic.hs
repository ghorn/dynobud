{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}

module Hascm.DirectCollocation.Dynamic
       ( PlotPoints(..)
       , PlotPointsL(..)
       , plotPoints
       , plotPointLists
       , toPlotTree
       ) where

import Data.Tree ( Tree(..) )
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Linear.V

import Plotter ( AccessorTree(..), Lookup(..), accessors )
import Hascm.Vectorize
import qualified Hascm.TypeVecs as TV
import Hascm.TypeVecs ( Vec )

import Hascm.DirectCollocation.Types
import Hascm.DirectCollocation.Formulate ( mkTaus, interpolate )

data PlotPoints n deg x z u a =
  PlotPoints (Vec n ((a, x a), Vec deg (a, x a, z a, u a), (a, x a))) (a, x a)

data PlotPointsL x z u a =
  PlotPointsL [[(a, x a)]] [[(a, z a)]] [[(a, u a)]]

plotPoints ::
  forall x z u p n deg a .
  (Dim n, Dim deg, Fractional a, Vectorize x)
  => CollTraj x z u p n deg a ->
  PlotPoints n deg x z u a
plotPoints ct@(CollTraj tf _ stages xf) = PlotPoints ret (tf', xf)
  where
    (tf', ret) = T.mapAccumL f 0 stages
    nStages = TV.tvlength stages
    h = tf / (fromIntegral nStages)
    taus = mkTaus (ctDeg ct)

    f :: a -> CollStage x z u deg a -> (a, ((a, x a), Vec deg (a, x a, z a, u a), (a, x a)))
    f t0 (CollStage x0 xs) = (tnext, stage)
      where
        tnext = t0 + h
        stage = ( (t0, x0)
                , TV.tvzipWith (\(CollPoint x z u) tau -> (t0 + h*tau, x, z, u)) xs taus
                , (tnext, interpolate taus x0 (fmap getX xs))
                )

plotPointLists :: forall n deg x z u a .
                  (RealFrac a, Read a, Vectorize x, Vectorize z, Vectorize u) =>
                  PlotPoints n deg x z u a ->
                  PlotPointsL x z u a
plotPointLists (PlotPoints vec txf) =
  PlotPointsL (xs' ++ [[txf]]) zs' us'
  where
    (xs', zs', us') = unzip3 $ map f (F.toList vec)

    f :: ((a, x a), Vec deg (a, x a, z a, u a), (a, x a)) -> ([(a, x a)], [(a, z a)], [(a, u a)])
    f (x0, stage, xf) = (x0 : xs ++ [xf], zs, us)
      where
        (xs,zs,us) = unzip3 $ map (\(t, x, z, u) -> ((t,x), (t,z), (t,u))) (F.toList stage)

toPlotTree :: forall x z u .
              (Lookup (x Double), Lookup (z Double), Lookup (u Double),
               Vectorize x, Vectorize z, Vectorize u) =>
              Tree (String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
toPlotTree = Node ("trajectory", "trajectory", Nothing) [xtree, ztree, utree]
  where
    xtree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
    xtree = toGetterTree (\(PlotPointsL x _ _) -> x) "differential states" $ accessors (fill 0)

    ztree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
    ztree = toGetterTree (\(PlotPointsL _ z _) -> z) "algebraic variables" $ accessors (fill 0)

    utree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
    utree = toGetterTree (\(PlotPointsL _ _ u) -> u) "controls" $ accessors (fill 0)

toGetterTree ::
  (b -> [[(Double, x Double)]]) -> String -> AccessorTree (x Double)
  -> Tree (String, String, Maybe (b -> [[(Double,Double)]]))
toGetterTree toXs name (Getter f) = Node (name, name, Just g) []
  where
    g = map (map (\(t,x) -> (t,f x))) . toXs
toGetterTree toXs name (Data (_,name') children) =
  Node (name, name', Nothing) $ map (\(n,t) -> toGetterTree toXs n t) children
