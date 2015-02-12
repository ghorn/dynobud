{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

module Dyno.DirectCollocation.Dynamic
       ( DynPlotPoints
       , CollTrajMeta(..)
       , MetaTree
       , forestFromMeta
       , toMeta
       , toMetaCov
       , dynPlotPoints
       , catDynPlotPoints
--       , toPlotTree
       , NameTree(..)
       ) where

import Data.Proxy ( Proxy(..) )
import Data.List ( mapAccumL, unzip5 )
import Data.Tree ( Tree(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Tree as Tree
import Data.Serialize ( Serialize(..) )
import GHC.Generics ( Generic )
import Linear.V

import Dyno.View.Internal.View ( unJ, unJ' )

import Dyno.Server.Accessors ( AccessorTree(..), Lookup(..), accessors )
import Dyno.Vectorize ( Vectorize, Id(..) )
import Dyno.View.JV
import Dyno.View.View
import Dyno.View.JVec ( JVec(..) )
import qualified Dyno.TypeVecs as TV
import Dyno.TypeVecs ( Vec )

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots, mkTaus )


data DynPlotPoints a = DynPlotPoints
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                     deriving (Show, Generic)
instance Serialize a => Serialize (DynPlotPoints a)
instance Serialize a => Serialize (Vector a) where
  get = fmap V.fromList get
  put = put . V.toList

catDynPlotPoints :: [DynPlotPoints a] -> DynPlotPoints a
catDynPlotPoints pps =
  DynPlotPoints
  (concatMap (\(DynPlotPoints x _ _ _ _) -> x) pps)
  (concatMap (\(DynPlotPoints _ x _ _ _) -> x) pps)
  (concatMap (\(DynPlotPoints _ _ x _ _) -> x) pps)
  (concatMap (\(DynPlotPoints _ _ _ x _) -> x) pps)
  (concatMap (\(DynPlotPoints _ _ _ _ x) -> x) pps)

dynPlotPoints ::
  forall x z u p o n deg a .
  (Dim n, Dim deg, Real a, Fractional a, Show a,
   Vectorize x, Vectorize z, Vectorize u, Vectorize o, Vectorize p)
  => QuadratureRoots
  -> CollTraj x z u p n deg (Vector a)
  -> Vec n (Vec deg (J (JV o) (Vector a), J (JV x) (Vector a)), J (JV x) (Vector a))
  -> DynPlotPoints a
dynPlotPoints quadratureRoots (CollTraj tf' _ stages' xf) outputs =
  DynPlotPoints (xss++[[(tf,unJ xf)]]) zss uss oss xdss
  where
    nStages = size (Proxy :: Proxy (JVec n (JV Id)))
    tf,h :: a
    Id tf = splitJV tf'
    h = tf / fromIntegral nStages

    taus :: Vec deg a
    taus = mkTaus quadratureRoots

    stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg (Vector a))
    stages = fmap split (unJVec (split stages'))
    (xss,zss,uss,oss,xdss) = unzip5 $ F.toList $ f 0 $ zip (F.toList stages) (F.toList outputs)


    -- todo: check this final time against expected tf
    f :: a
         -> [( CollStage (JV x) (JV z) (JV u) deg (Vector a)
             , (Vec deg (J (JV o) (Vector a), J (JV x) (Vector a)), J (JV x) (Vector a))
             )]
         -> [( [(a,Vector a)]
             , [(a,Vector a)]
             , [(a,Vector a)]
             , [(a,Vector a)]
             , [(a,Vector a)]
             )]
    f _ [] = []
    f t0 ((CollStage x0 xzus', (xdos, xnext)) : css) = (xs,zs,us,os,xds) : f tnext css
      where
        tnext = t0 + h
        xzus0 = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint (JV x) (JV z) (JV u) (Vector a))

        xs :: [(a,Vector a)]
        xs = (t0,unJ x0):xs'++[(tnext,unJ xnext)]

        xs',zs,us,os,xds :: [(a,Vector a)]
        (xs',zs,us,os,xds) = unzip5 $ F.toList $ TV.tvzipWith3 g xzus0 xdos taus

        g (CollPoint x z u) (o,x') tau = ( (t,unJ' "x" x), (t,unJ' "z" z), (t,unJ' "u" u), (t,unJ' "o" o), (t,unJ' "x'" x') )
          where
            t = t0 + h*tau




--toPlotTree :: forall x z u .
--              (Lookup (x Double), Lookup (z Double), Lookup (u Double),
--               Vectorize x, Vectorize z, Vectorize u) =>
--              Tree (String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
--toPlotTree = Node ("trajectory", "trajectory", Nothing) [xtree, ztree, utree]
--  where
--    xtree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
--    xtree = toGetterTree (\(PlotPointsL x _ _) -> x) "differential states" $ accessors (fill 0)
--
--    ztree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
--    ztree = toGetterTree (\(PlotPointsL _ z _) -> z) "algebraic variables" $ accessors (fill 0)
--
--    utree :: Tree ( String, String, Maybe (PlotPointsL x z u Double -> [[(Double, Double)]]))
--    utree = toGetterTree (\(PlotPointsL _ _ u) -> u) "controls" $ accessors (fill 0)
--
--    toGetterTree toXs name (Getter f) = Node (name, name, Just g) []
--      where
--        g = map (map (second f)) . toXs
--    toGetterTree toXs name (Data (_,name') children) =
--      Node (name, name', Nothing) $ map (uncurry (toGetterTree toXs)) children


data NameTree = NameTreeNode (String,String) [(String,NameTree)]
              | NameTreeLeaf Int
              deriving (Show, Eq, Generic)
instance Serialize NameTree

data CollTrajMeta = CollTrajMeta { ctmX :: NameTree
                                 , ctmZ :: NameTree
                                 , ctmU :: NameTree
                                 , ctmP :: NameTree
                                 , ctmO :: NameTree
                                 , ctmNx :: Int
                                 , ctmNz :: Int
                                 , ctmNu :: Int
                                 , ctmNp :: Int
                                 , ctmNo :: Int
                                 , ctmNsx :: Int
                                 , ctmN :: Int
                                 , ctmDeg :: Int
                                 , ctmQuadRoots :: QuadratureRoots
                                 } deriving (Eq, Generic, Show)
instance Serialize CollTrajMeta

namesFromAccTree :: AccessorTree a -> NameTree
namesFromAccTree x = (\(_,(_,y)) -> y) $ namesFromAccTree' 0 ("",x)

namesFromAccTree' :: Int -> (String, AccessorTree a) -> (Int, (String, NameTree))
namesFromAccTree' k (nm, Getter _) = (k+1, (nm, NameTreeLeaf k))
namesFromAccTree' k0 (nm, Data names ats) = (k, (nm, NameTreeNode names children))
  where
    (k, children) = mapAccumL namesFromAccTree' k0 ats


type MetaTree a = Tree.Forest (String, String, Maybe (DynPlotPoints a -> [[(a,a)]]))

forestFromMeta :: CollTrajMeta -> MetaTree Double
forestFromMeta meta = [xTree,zTree,uTree,oTree,xdTree]
  where
    xTree  = blah (\(DynPlotPoints x _ _ _ _ ) ->  x) "differential states" (ctmX meta)
    zTree  = blah (\(DynPlotPoints _ z _ _ _ ) ->  z) "algebraic variables" (ctmZ meta)
    uTree  = blah (\(DynPlotPoints _ _ u _ _ ) ->  u) "controls" (ctmU meta)
    oTree  = blah (\(DynPlotPoints _ _ _ o _ ) ->  o) "outputs" (ctmO meta)
    xdTree = blah (\(DynPlotPoints _ _ _ _ xd) -> xd) "diff state derivatives" (ctmX meta)

    blah :: (c -> [[(t, V.Vector t)]]) -> String -> NameTree ->
            Tree (String, String, Maybe (c -> [[(t, t)]]))
    blah f myname (NameTreeNode (nm1,_) children) =
      Tree.Node (myname,nm1,Nothing) $ map (uncurry (blah f)) children
    blah f myname (NameTreeLeaf k) = Tree.Node (myname,"",Just (woo . f)) []
      where
        woo = map (map (\(t,x) -> (t, x V.! k)))


toMeta :: forall x z u p o n deg .
          (Lookup (x ()), Lookup (z ()), Lookup (u ()), Lookup (p ()), Lookup (o ()),
           Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize o,
           Dim n, Dim deg)
          => QuadratureRoots -> Proxy o -> Proxy (CollTraj x z u p n deg) -> CollTrajMeta
toMeta roots _ _ =
  CollTrajMeta { ctmX = namesFromAccTree $ accessors (jfill () :: J (JV x) (Vector ()))
               , ctmZ = namesFromAccTree $ accessors (jfill () :: J (JV z) (Vector ()))
               , ctmU = namesFromAccTree $ accessors (jfill () :: J (JV u) (Vector ()))
               , ctmP = namesFromAccTree $ accessors (jfill () :: J (JV p) (Vector ()))
               , ctmO = namesFromAccTree $ accessors (jfill () :: J (JV o) (Vector ()))
               , ctmNx = size (Proxy :: Proxy (JV x))
               , ctmNz = size (Proxy :: Proxy (JV z))
               , ctmNu = size (Proxy :: Proxy (JV u))
               , ctmNp = size (Proxy :: Proxy (JV p))
               , ctmNo = size (Proxy :: Proxy (JV o))
               , ctmNsx = 0
               , ctmN = reflectDim (Proxy :: Proxy n)
               , ctmDeg = reflectDim (Proxy :: Proxy deg)
               , ctmQuadRoots = roots
               }

toMetaCov :: forall sx x z u p o n deg .
          (Lookup (x ()), Lookup (z ()), Lookup (u ()), Lookup (p ()), Lookup (o ()),
           Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize o,
           Vectorize sx,
           Dim n, Dim deg)
          => QuadratureRoots -> Proxy o -> Proxy (CollTrajCov sx x z u p n deg) -> CollTrajMeta
toMetaCov roots _ _ = meta0 { ctmNsx = size (Proxy :: Proxy (JV sx)) }
  where
    meta0 = toMeta roots (Proxy :: Proxy o) (Proxy :: Proxy (CollTraj x z u p n deg))
