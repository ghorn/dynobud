{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

module Dyno.DirectCollocation.Dynamic
       ( DynCollTraj(..)
       , DynPlotPoints
       , CollTrajMeta(..)
       , MetaTree
       , forestFromMeta
       , toMeta
       , ctToDynamic
       , dynPlotPoints
--       , toPlotTree
       , NameTree(..)
       ) where

import Data.List ( mapAccumL, unzip4 )
import Data.Tree ( Tree(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Tree as Tree
import Data.Serialize ( Serialize(..) )
import GHC.Generics ( Generic )
import Linear.V

import Dyno.Server.Accessors ( AccessorTree(..), Lookup(..), accessors )
import Dyno.Vectorize
import Dyno.View.View
import Dyno.View.Viewable
import qualified Dyno.TypeVecs as TV
import Dyno.TypeVecs ( Vec )

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Quadratures ( mkTaus, interpolate )
import Dyno.DirectCollocation.Reify ( reifyCollTraj )


data DynPlotPoints a = DynPlotPoints
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                       [[(a, Vector a)]]
                     deriving Show

data D a
data DynCollTraj a = DynCollTraj (J (CollTraj D D D D D () ()) a) (Vec () (Vec () (J D a)))
                      deriving (Generic, Show)
instance Serialize a => Serialize (DynCollTraj a)
instance Serialize a => Serialize (V.Vector a) where
  put = put . V.toList
  get = fmap V.fromList get

dynPlotPoints :: forall a . (Real a, Fractional a, Show a) => DynCollTraj (Vector a) -> CollTrajMeta -> DynPlotPoints a
dynPlotPoints (DynCollTraj traj outputs) meta =
  reifyCollTraj (nx,nz,nu,np,no,ns,n,deg) traj outputs foo
  where
    nx  = ctmNx meta
    nz  = ctmNz meta
    nu  = ctmNu meta
    np  = ctmNp meta
    no  = ctmNo meta
    ns  = ctmNs meta
    n   = ctmN meta
    deg = ctmDeg meta
    
    foo :: (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize o, View s, Dim deg, Dim n)
           => J (CollTraj x z u p s n deg) (Vector a)
           -> Vec n (Vec deg (J (JV o) (Vector a)))
           -> DynPlotPoints a
    foo ct outs = plotPoints (split ct) outs


-- a safe, point maker which is difficult to work with
-- first stage in making a list
plotPoints ::
  forall x z u p o s n deg a .
  (Dim n, Dim deg, Real a, Fractional a, Show a, Vectorize x, Vectorize z, Vectorize u, Vectorize o)
  => CollTraj x z u p s n deg (Vector a)
  -> Vec n (Vec deg (J (JV o) (Vector a)))
  -> DynPlotPoints a
plotPoints ct@(CollTraj (UnsafeJ tf') _ _ stages' xf) outputs = DynPlotPoints (xss++[[(tf,unJ xf)]]) zss uss oss
  where
    nStages = size (Proxy :: Proxy (JVec n S))
    tf,h :: a
    tf = V.head tf'
    h = tf / fromIntegral nStages
    --taus :: Vec deg Double
    taus = mkTaus (ctDeg ct)

    stages = fmap split (unJVec (split stages')) :: Vec n (CollStage (JV x) (JV z) (JV u) deg (Vector a))
    (xss,zss,uss,oss) = unzip4 $ f 0 $ zip (F.toList stages) (F.toList outputs)

    -- todo: check this final time against expected tf
    f :: a
         -> [(CollStage (JV x) (JV z) (JV u) deg (Vector a), Vec deg (J (JV o) (Vector a)))]
         -> [([(a,Vector a)], [(a,Vector a)], [(a,Vector a)], [(a,Vector a)])] 
    f _ [] = []
    f t0 ((CollStage x0 xzus', os') : css) = (xs,zs,us,os) : f tnext css
      where
        tnext = t0 + h
        xzus0 = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint (JV x) (JV z) (JV u) (Vector a))
        xnext = interpolate taus x0 (fmap getX xzus0)

        xs :: [(a,Vector a)]
        xs = (t0,unJ x0):xs'++[(tnext,unJ xnext)]

        xs',zs,us,os :: [(a,Vector a)]
        (xs',zs,us,os) = unzip4 $ F.toList $ TV.tvzipWith3 g xzus0 os' taus

        g (CollPoint x z u) o tau = ( (t,unJ' "x" x), (t,unJ' "z" z), (t,unJ' "u" u), (t,unJ' "o" o) )
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
                                 , ctmNs :: Int
                                 , ctmN :: Int
                                 , ctmDeg :: Int
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
forestFromMeta meta = [xTree,zTree,uTree,oTree]
  where
    xTree = blah (\(DynPlotPoints x _ _ _) -> x) "differential states" (ctmX meta)
    zTree = blah (\(DynPlotPoints _ z _ _) -> z) "algebraic variables" (ctmZ meta)
    uTree = blah (\(DynPlotPoints _ _ u _) -> u) "controls" (ctmU meta)
    oTree = blah (\(DynPlotPoints _ _ _ o) -> o) "outputs" (ctmO meta)

    blah :: (c -> [[(t, V.Vector t)]]) -> String -> NameTree ->
            Tree (String, String, Maybe (c -> [[(t, t)]]))
    blah f myname (NameTreeNode (nm1,_) children) =
      Tree.Node (myname,nm1,Nothing) $ map (uncurry (blah f)) children
    blah f myname (NameTreeLeaf k) = Tree.Node (myname,"",Just (woo . f)) []
      where
        woo = map (map (\(t,x) -> (t, x V.! k)))


toMeta :: forall x z u p o s n deg a .
          (Lookup (x ()), Lookup (z ()), Lookup (u ()), Lookup (p ()), Lookup (o ()),
           Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize o,
           View s, Viewable a,
           Dim n, Dim deg)
          => Proxy o -> J (CollTraj x z u p s n deg) a -> CollTrajMeta
toMeta _ ct =
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
               , ctmNs = size (Proxy :: Proxy s)
               , ctmN = ctN ct'
               , ctmDeg = ctDeg ct'
               }
  where
    ct' = split ct

ctToDynamic :: forall x z u p o s n deg a .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p, View s) =>
  J (CollTraj x z u p s n deg) a -> Vec n (Vec deg (J (JV o) a)) -> DynCollTraj a
ctToDynamic (UnsafeJ x) os = DynCollTraj (UnsafeJ x) (castO os) -- this should be totally safe
  where
    castO :: Vec n (Vec deg (J (JV o) a)) -> Vec () (Vec () (J D a))
    castO = TV.mkUnit . fmap (TV.mkUnit . fmap cast)

    cast :: J (JV o) a -> J D a
    cast (UnsafeJ o) = UnsafeJ o
