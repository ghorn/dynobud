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

import Data.List ( mapAccumL )
import Data.Proxy ( Proxy(..) )
import Data.Tree ( Tree(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Tree as Tree
import Data.Serialize ( Serialize(..) )
import GHC.Generics ( Generic )
import Linear.V

import Dyno.Server.Accessors ( AccessorTree(..), Lookup(..), accessors )
import Dyno.View.View
import qualified Dyno.TypeVecs as TV
import Dyno.TypeVecs ( Vec )

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Formulate ( mkTaus, interpolate )
import Dyno.DirectCollocation.Reify


data DynPlotPoints a = DynPlotPoints [[(a, Vector a)]] [[(a, Vector a)]] [[(a, Vector a)]]
                     deriving Show

data D a
newtype DynCollTraj a = DynCollTraj (J (CollTraj D D D D D () ()) a)
                      deriving (Generic, Show)
instance Serialize a => Serialize (DynCollTraj a)
instance Serialize a => Serialize (V.Vector a) where
  put = put . V.toList
  get = fmap V.fromList get

dynPlotPoints :: forall a . (Real a, Fractional a, Show a) => DynCollTraj (Vector a) -> CollTrajMeta -> DynPlotPoints a
dynPlotPoints (DynCollTraj traj) meta = reifyCollTraj' (nx,nz,nu,np,ns,n,deg) traj foo
  where
    nx  = ctmNx meta
    nz  = ctmNz meta
    nu  = ctmNu meta
    np  = ctmNp meta
    ns  = ctmNs meta
    n   = ctmN meta
    deg = ctmDeg meta
    
    foo :: (View x, View z, View u, View p, View s, Dim deg, Dim n) => J (CollTraj x z u p s n deg) (Vector a) -> DynPlotPoints a
    foo ct = plotPoints (split ct)


-- a safe, point maker which is difficult to work with
-- first stage in making a list
plotPoints ::
  forall x z u p s n deg a .
  (Dim n, Dim deg, Real a, Fractional a, Show a, View x, View z, View u)
  => CollTraj x z u p s n deg (Vector a) -> DynPlotPoints a
plotPoints ct@(CollTraj (UnsafeJ tf') _ _ stages' xf) = DynPlotPoints (xss++[[(tf,unJ xf)]]) zss uss
  where
    nStages = size (Proxy :: Proxy (JVec n S))
    tf,h :: a
    tf = V.head tf'
    h = tf / fromIntegral nStages
    --taus :: Vec deg Double
    taus = mkTaus (ctDeg ct)

    stages = fmap split (unJVec (split stages')) :: Vec n (CollStage x z u deg (Vector a))
    (xss,zss,uss) = unzip3 $ f 0 (F.toList stages)

    -- todo: check this final time against expected tf
    f :: a -> [CollStage x z u deg (Vector a)] -> [([(a,Vector a)], [(a,Vector a)], [(a,Vector a)])] 
    f _ [] = []
    f t0 ((CollStage x0 xzus'):css) = (xs,zs,us) : f tnext css
      where
        tnext = t0 + h
        xzus0 = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u (Vector a))
        xnext = interpolate taus x0 (fmap getX xzus0)

        xs :: [(a,Vector a)]
        xs = (t0,unJ x0):xs'++[(tnext,unJ xnext)]

        xs',zs,us :: [(a,Vector a)]
        (xs',zs,us) = unzip3 $ F.toList $ TV.tvzipWith g xzus0 taus

        g (CollPoint x z u) tau = ( (t,unJ' "x" x), (t,unJ' "z" z), (t,unJ' "u" u) )
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
                                 , ctmNx :: Int
                                 , ctmNz :: Int
                                 , ctmNu :: Int
                                 , ctmNp :: Int
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
forestFromMeta meta = [xTree,zTree,uTree]
  where
    xTree = blah (\(DynPlotPoints x _ _) -> x) "differential states" (ctmX meta)
    zTree = blah (\(DynPlotPoints _ z _) -> z) "algebraic variables" (ctmZ meta)
    uTree = blah (\(DynPlotPoints _ _ u) -> u) "controls" (ctmU meta)

    blah :: (c -> [[(t, V.Vector t)]]) -> String -> NameTree ->
            Tree (String, String, Maybe (c -> [[(t, t)]]))
    blah f myname (NameTreeNode (nm1,_) children) =
      Tree.Node (myname,nm1,Nothing) $ map (uncurry (blah f)) children
    blah f myname (NameTreeLeaf k) = Tree.Node (myname,"",Just (woo . f)) []
      where
        woo = map (map (\(t,x) -> (t, x V.! k)))


toMeta :: forall x z u p s n deg a .
          (Lookup (x (Vector ())), Lookup (z (Vector ())), Lookup (u (Vector ())), Lookup (p (Vector ())),
           View x, View z, View u, View p, View s,
           Dim n, Dim deg)
          => CollTraj x z u p s n deg a -> CollTrajMeta
toMeta ct =
  CollTrajMeta { ctmX = namesFromAccTree $ accessors $ split $ (jfill () :: J x (Vector ()))
               , ctmZ = namesFromAccTree $ accessors $ split $ (jfill () :: J z (Vector ()))
               , ctmU = namesFromAccTree $ accessors $ split $ (jfill () :: J u (Vector ()))
               , ctmP = namesFromAccTree $ accessors $ split $ (jfill () :: J p (Vector ()))
               , ctmNx = size (Proxy :: Proxy x)
               , ctmNz = size (Proxy :: Proxy z)
               , ctmNu = size (Proxy :: Proxy u)
               , ctmNp = size (Proxy :: Proxy p)
               , ctmNs = size (Proxy :: Proxy s)
               , ctmN = ctN ct
               , ctmDeg = ctDeg ct
               }

ctToDynamic ::
  (View x, View z, View u, View p, View s) =>
  J (CollTraj x z u p s n deg) a -> DynCollTraj a
ctToDynamic (UnsafeJ x) = DynCollTraj (UnsafeJ x) -- this should be totally safe
