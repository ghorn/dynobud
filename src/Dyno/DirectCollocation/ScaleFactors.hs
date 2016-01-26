{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.DirectCollocation.ScaleFactors
       ( ScaleFactors(..), ScaleFactors', ScaleFactor(..)
       , getScaleFactors, summarizeScaleFactors
       ) where

import GHC.Generics ( Generic, Generic1 )

import Control.Lens ( (.~) )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Serialize ( Serialize )
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Text.Printf ( printf )

import Dyno.DirectCollocation.Types
import Dyno.Nlp ( Bounds )
import Dyno.Vectorize ( Vectorize(..), Id(..), fill )
import Dyno.View.View ( View(..), splitJV )
import Dyno.View.JVec ( unJVec )
import Dyno.TypeVecs ( Dim )
import Dyno.Ocp

import Accessors ( Lookup, Field(..), accessors, describeField, flatten )

data ScaleFactor =
  ScaleFactor
  { sfMyScale :: Double
  , sfBounds :: Bounds
  , sfMagnitude :: Double
  , sfRelDiff :: Double
  , sfName :: String
  } deriving Generic
instance Serialize ScaleFactor

type ScaleFactors' ocp = ScaleFactors (X ocp) (Z ocp) (U ocp) (P ocp) (H ocp) (C ocp)

data ScaleFactors x z u p h c a =
  ScaleFactors
  { xScale :: x a
  , zScale :: z a
  , uScale :: u a
  , pScale :: p a
  , pathConstraintScale :: h a
  , boundaryConditionScale :: c a
  , endTimeScale :: a
  } deriving (Functor, F.Foldable, T.Traversable, Generic, Generic1)
instance ( Lookup (x a), Lookup (z a), Lookup (u a), Lookup (p a)
         , Lookup (h a), Lookup (c a), Lookup a
         ) => Lookup (ScaleFactors x z u p h c a)
instance ( Serialize (x a), Serialize (z a), Serialize (u a), Serialize (p a)
         , Serialize (h a), Serialize (c a), Serialize a
         ) => Serialize (ScaleFactors x z u p h c a)
instance ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
         , Vectorize h, Vectorize c
         ) => Vectorize (ScaleFactors x z u p h c)

summarizeScaleFactors ::
  ( Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize h, Vectorize c
  ) => ScaleFactors x z u p h c ScaleFactor -> Double -> String
summarizeScaleFactors sfs fracThreshold =
  case catMaybes (map report (F.toList (vectorize sfs))) of
    [] -> ""
    xs -> unlines $ " ratio      scale  magnitude  name" : xs
  where
    report :: ScaleFactor -> Maybe String
    report (ScaleFactor {sfBounds = (Just 0, Just 0)}) = Nothing
    report sf
      | ratio < fracThreshold && 1/ratio < fracThreshold = Nothing
      | otherwise = Just $ printf "%6.2g  %9.2g  %9.2g  %s"
                    ratio (sfMyScale sf) (sfMagnitude sf) (sfName sf)
      where
        ratio = sfMyScale sf / sfMagnitude sf

-- | get scale factors based on the largest magnitude of each type over a trajectory
getScaleFactors ::
  forall x z u p h c n deg r fp o q qo po
  . ( Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize h, Vectorize c, Vectorize r
    , Applicative x, Applicative z, Applicative u, Applicative p, Applicative h, Applicative c
    , Lookup (x String), Lookup (z String), Lookup (u String), Lookup (p String)
    , Lookup (h String), Lookup (c String)
    , Dim n, Dim deg
    )
  => CollTraj x z u p n deg (Vector Double)
  -> CollOcpConstraints x p r c h n deg (Vector Double)
  -> OcpPhase x z u p r o c h q qo po fp
  -> OcpPhaseInputs x z u p c h fp
  -> ScaleFactors x z u p h c ScaleFactor
getScaleFactors x g ocp inputs =
  getScaleFactor <$> myScale <*> bounds <*> magnitude <*> names
  where
    getScaleFactor :: Double -> Bounds -> Double -> String -> ScaleFactor
    getScaleFactor myscale' bounds' magnitude' name' =
      ScaleFactor
      { sfMyScale = myscale'
      , sfBounds = bounds'
      , sfMagnitude = magnitude'
      , sfRelDiff = abs (magnitude' - myscale') / (0.5 * (magnitude' + myscale'))
      , sfName = name'
      }

    magnitude :: ScaleFactors x z u p h c Double
    magnitude = getMagnitude x g

    myScale :: ScaleFactors x z u p h c Double
    myScale =
      ScaleFactors
      { xScale = fromMaybe (fill 1) (ocpXScale ocp)
      , zScale = fromMaybe (fill 1) (ocpZScale ocp)
      , uScale = fromMaybe (fill 1) (ocpUScale ocp)
      , pScale = fromMaybe (fill 1) (ocpPScale ocp)
      , pathConstraintScale = fromMaybe (fill 1) (ocpPathCScale ocp)
      , boundaryConditionScale = fromMaybe (fill 1) (ocpBcScale ocp)
      , endTimeScale = fromMaybe 1 (ocpTScale ocp)
      }
    bounds :: ScaleFactors x z u p h c Bounds
    bounds =
      ScaleFactors
      { xScale = ocpXbnd inputs
      , zScale = ocpZbnd inputs
      , uScale = ocpUbnd inputs
      , pScale = ocpPbnd inputs
      , pathConstraintScale = ocpPathCBnds inputs
      , boundaryConditionScale = ocpBcBnds inputs
      , endTimeScale = ocpTbnd inputs
      }

    names :: ScaleFactors x z u p h c String
    names = F.foldl' ff (fill "") (flatten accessors)
      where
        ff sf0 (name, FieldString f) = (f .~ name) sf0
        ff _ (name, f) =
          error $ "the 'impossible' happened, got a non-strong getter for "
          ++ show name ++ " with type " ++ describeField f

getMagnitude ::
  forall x z u p h c n deg r
  . ( Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize h, Vectorize c
    , Applicative x, Applicative z, Applicative u, Applicative p, Applicative h, Applicative c
    , Dim n, Dim deg
    )
  => CollTraj x z u p n deg (Vector Double)
  -> CollOcpConstraints x p r c h n deg (Vector Double)
  -> ScaleFactors x z u p h c Double
getMagnitude traj@(CollTraj tf' p' _ _) g =
  ScaleFactors
  { xScale = getMagnitude' xs
  , zScale = getMagnitude' zs
  , uScale = getMagnitude' us
  , pScale = p
  , pathConstraintScale = getMagnitude' pathC
  , boundaryConditionScale = bc
  , endTimeScale = tf
  }
  where
    getMagnitude' :: forall f . Applicative f => [f Double] -> f Double
    getMagnitude' fs = fmap maximum (sequenceA fs)

    bc :: c Double
    bc = splitJV (coBc g)

    pathC :: [h Double]
    pathC = concatMap (map splitJV . F.toList . unJVec . split) $ F.toList $ unJVec $ split (coPathC g)

    ((xs',xf), zs', us') = getXzus''' traj
    xs :: [x Double]
    xs = map (fmap abs) $ concatMap (\(x0,xss) -> x0 : F.toList xss) (F.toList xs') ++ [xf]
    zs :: [z Double]
    zs = map (fmap abs) $ concatMap F.toList (F.toList zs')
    us :: [u Double]
    us = map (fmap abs) $ concatMap F.toList (F.toList us')
    p = fmap abs (splitJV p')
    tf = abs $ unId (splitJV tf')
