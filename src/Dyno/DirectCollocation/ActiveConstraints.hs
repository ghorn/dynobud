{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.DirectCollocation.ActiveConstraints
       ( ActiveConstraints(..)
       , Active(..)
       , getActiveConstraints
       , flattenActiveConstraints
       , summarizeActiveConstraints
       , matlabActiveConstraints
       , pythonActiveConstraints
       ) where

import GHC.Generics ( Generic )

import Accessors ( Lookup, GATip(..), GAField(..), flatten', accessors, describeGAField )
import Control.Applicative
import Control.Lens ( (^.) )
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Text.Printf ( printf )

import Dyno.DirectCollocation.Types
import Dyno.Ocp ( OcpPhase(..), OcpPhaseInputs(..) )
import Dyno.Nlp ( Bounds )
import Dyno.View.Vectorize ( Vectorize, unId )
import Dyno.View.View ( View(..), J, JV, splitJV )
import Dyno.View.JVec ( unJVec )
import Dyno.TypeVecs ( Dim )

data Active a = Active { activeLower :: a, activeUpper :: a }
              deriving (Functor, F.Foldable, T.Traversable, Generic)
instance Lookup a => Lookup (Active a)

data ActiveConstraints x z u p h c a =
  ActiveConstraints
  { xBounds :: x a
  , zBounds :: z a
  , uBounds :: u a
  , pBounds :: p a
  , pathConstraintBounds :: h a
  , boundaryConditionBounds :: c a
  , endTimeBounds :: a
  } deriving (Functor, F.Foldable, T.Traversable, Generic)
instance ( Lookup (x a), Lookup (z a), Lookup (u a), Lookup (p a)
         , Lookup (h a), Lookup (c a), Lookup a
         ) => Lookup (ActiveConstraints x z u p h c a)

summarizeActiveConstraints ::
  ( Functor x, Functor z, Functor u, Functor p, Functor h, Functor c
  , Lookup (x Int)
  , Lookup (z Int)
  , Lookup (u Int)
  , Lookup (p Int)
  , Lookup (h Int)
  , Lookup (c Int)
  ) => ActiveConstraints x z u p h c (Active Int) -> String
summarizeActiveConstraints activeCons =
  unlines $ catMaybes $ map report $ flattenActiveConstraints activeCons
  where
    report (_, Active 0 0) = Nothing
    report (mnames, Active lb ub) =
      Just $ printf "% 4d lower, % 4d upper (%s)" lb ub (intercalate "." names)
      where
        names :: [String]
        names = map (maybe "()" id) mnames

matlabActiveConstraints ::
  ( Functor x, Functor z, Functor u, Functor p, Functor h, Functor c
  , Lookup (x Int)
  , Lookup (z Int)
  , Lookup (u Int)
  , Lookup (p Int)
  , Lookup (h Int)
  , Lookup (c Int)
  ) => ActiveConstraints x z u p h c (Active Int) -> String
matlabActiveConstraints activeCons = "{" ++ intercalate "; " cons ++ "}"
  where
    cons = map report $ flattenActiveConstraints activeCons
    report (mnames, Active lb ub) = printf "'%s', %d, %d" (intercalate "." names) lb ub
      where
        names :: [String]
        names = map (maybe "()" id) mnames

pythonActiveConstraints ::
  ( Functor x, Functor z, Functor u, Functor p, Functor h, Functor c
  , Lookup (x Int)
  , Lookup (z Int)
  , Lookup (u Int)
  , Lookup (p Int)
  , Lookup (h Int)
  , Lookup (c Int)
  ) => ActiveConstraints x z u p h c (Active Int) -> String
pythonActiveConstraints activeCons = "[" ++ intercalate ", " cons ++ "]"
  where
    cons = map report $ flattenActiveConstraints activeCons
    report (mnames, Active lb ub) = printf "('%s', %d, %d)" (intercalate "." names) lb ub
      where
        names :: [String]
        names = map (maybe "()" id) mnames


flattenActiveConstraints ::
  forall x z u p h c .
  ( Functor x, Functor z, Functor u, Functor p, Functor h, Functor c
  , Lookup (x Int)
  , Lookup (z Int)
  , Lookup (u Int)
  , Lookup (p Int)
  , Lookup (h Int)
  , Lookup (c Int)
  ) => ActiveConstraints x z u p h c (Active Int) -> [([Maybe String], Active Int)]
flattenActiveConstraints activeCons = map report $ flatten' accessors
  where
    report (mnames, GATipField (FieldInt f)) = (mnames, Active (lbs ^. f) (ubs ^. f))
    report (mnames, GATipField f) =
      error $ "the 'impossible' happened, " ++
      "flattenActiveConstraints got a non-int getter " ++ show mnames ++
      " with type " ++ describeGAField f
    report (mnames, GATipSimpleEnum _) =
      error $ "the 'impossible' happened, " ++
      "flattenActiveConstraints got a SimpleEnum getter " ++ show mnames
    lbs = fmap activeLower activeCons
    ubs = fmap activeUpper activeCons

getActiveConstraints ::
  forall x z u p h c n deg r fp o q qo po
  . ( Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize h, Vectorize c, Vectorize r
    , Applicative x, Applicative z, Applicative u, Applicative p, Applicative h, Applicative c
    , Dim n, Dim deg
    )
  => (J (CollTraj x z u p n deg) (Vector Double)
      -> J (JV fp) (Vector Double)
      -> IO (J (CollOcpConstraints x p r c h n deg) (Vector Double))
     )
  -> OcpPhase x z u p r o c h q qo po fp
  -> Double
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> J (JV fp) (Vector Double)
  -> OcpPhaseInputs x z u p c h fp
  -> IO (ActiveConstraints x z u p h c (Active Int))
getActiveConstraints evalConstraints ocp eps x p inputs = do
  g <- evalConstraints x p
  return $ whatsActive eps (split x) (split g) inputs ocp

whatsActive ::
  forall x z u p h c n deg r fp o q qo po
  . ( Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize h, Vectorize c
    , Applicative x, Applicative z, Applicative u, Applicative p, Applicative h, Applicative c
    , Dim n, Dim deg
    )
  => Double
  -> CollTraj x z u p n deg (Vector Double)
  -> CollOcpConstraints x p r c h n deg (Vector Double)
  -> OcpPhaseInputs x z u p c h fp
  -> OcpPhase x z u p r o c h q qo po fp
  -> ActiveConstraints x z u p h c (Active Int)
whatsActive userEps traj@(CollTraj tf p _ _) g inputs ocp =
  ActiveConstraints
  { xBounds = countEmAll $ map (isActive (ocpXScale ocp) (ocpXbnd inputs)) xs
  , zBounds = countEmAll $ map (isActive (ocpZScale ocp) (ocpZbnd inputs)) zs
  , uBounds = countEmAll $ map (isActive (ocpUScale ocp) (ocpUbnd inputs)) us
  , pBounds = isActive (ocpPScale ocp) (ocpPbnd inputs) (splitJV p)
  , pathConstraintBounds = countEmAll $ map (isActive (ocpPathCScale ocp) (ocpPathCBnds inputs)) pathC
  , boundaryConditionBounds = isActive (ocpBcScale ocp) (ocpBcBnds inputs) bc
  , endTimeBounds = scalarIsActive userEps (ocpTScale ocp) (ocpTbnd inputs) (unId (splitJV tf))
  }
  where
    countEmAll :: forall f . Applicative f => [f (Active Int)] -> f (Active Int)
    countEmAll counts = liftA2 Active lowers uppers
      where
        lowers :: f Int
        lowers = fmap sum $ T.sequenceA $ map (fmap activeLower) counts

        uppers :: f Int
        uppers = fmap sum $ T.sequenceA $ map (fmap activeUpper) counts

    pathC :: [h Double]
    pathC = concatMap (map splitJV . F.toList . unJVec . split) $ F.toList $ unJVec $ split (coPathC g)
    (xs', zs', us') = getXzus'' traj
    xs = concatMap F.toList (F.toList xs')
    zs = concatMap F.toList (F.toList zs')
    us = concatMap F.toList (F.toList us')

    isActive :: Applicative f => Maybe (f Double) -> f Bounds -> f Double -> f (Active Int)
    isActive scale bnds val = (scalarIsActive userEps) <$> T.sequenceA scale <*> bnds <*> val

    bc :: c Double
    bc = splitJV (coBc g)


scalarIsActive :: Double -> Maybe Double -> Bounds -> Double -> Active Int
scalarIsActive _ _ (Nothing, Nothing) _ = Active 0 0
scalarIsActive userEps scale bnd@(Just lb, Nothing) x
  | eps >= x - lb = Active 1 0 -- lower active
  | otherwise = Active 0 0
  where
    eps = toEps userEps scale bnd
scalarIsActive userEps scale bnd@(Nothing, Just ub) x
  | eps >= ub - x = Active 0 1 -- upper active
  | otherwise = Active 0 0
  where
    eps = toEps userEps scale bnd
scalarIsActive userEps scale bnd@(Just lb, Just ub) x
  | lb == ub = Active 0 0 -- don't report equality constraints, duh
  | eps >= x - lb = Active 1 0 -- lower active
  | eps >= ub - x = Active 0 1 -- upper active
  | otherwise = Active 0 0
  where
    eps = toEps userEps scale bnd


toEps :: Double -> Maybe Double -> (Maybe Double, Maybe Double) -> Double
toEps userEps (Just scale) _ = scale * userEps
toEps userEps Nothing (Just lb, Just ub) = scale * userEps
  where
    scale = ub - lb
toEps userEps Nothing _ = userEps
