{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- todo(greg): use this in the untit tests
module Dyno.DirectCollocation.CheckAccuracy
       ( Err(..)
       , Checks(..)
       , CheckState(..)
       , toErr
       , checkIntegrationAccuracy
       , summarizeAccuracy
       ) where

import GHC.Generics ( Generic, Generic1 )

import Data.List ( sortBy )
import Data.Maybe ( isJust, fromJust )
import Data.Proxy ( Proxy(..) )
import Data.Foldable ( foldl', maximumBy )
import qualified Data.Vector as V
import Linear ( Additive )
import Text.Printf ( printf )

import Accessors

import Dyno.Integrate
import Dyno.Vectorize ( Vectorize(..), Id(..), None(..), fill )
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( splitJV )
import Dyno.TypeVecs ( Vec, Dim )
import qualified Dyno.TypeVecs as TV
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots, collocationTimes )
import Dyno.DirectCollocation.Types
import Dyno.LagrangePolynomials ( interpolate )

data Checks x n =
  Checks
  { checksStageMismatch :: Vec n (x (Err Double))
  , checksWorstStageMismatch :: x (Err Double)
  , checksTrajMismatch :: x (Err Double)
  }

data CheckState x q a =
  CheckState
  { csX :: x a
  , csQ :: q a
  } deriving (Functor, Generic, Generic1)
instance (Vectorize x, Vectorize q) => Vectorize (CheckState x q)
instance (Lookup (x a), Lookup (q a), Lookup a) => Lookup (CheckState x q a)

data Err a =
  Err
  { errRef :: a
  , errVal :: a
  , errAbs :: a
  , errRel :: a
  }

summarizeAccuracy ::
  forall x n
  . (Vectorize x, Lookup (x Double), Dim n)
  => Checks x n -> String
summarizeAccuracy (Checks _ worstStageMismatch trajMismatch) =
  unlines $
  ("worst stage mismatches:" : map showOne stageMismatches) ++
  ("" : "worst overall mismatches:" : map showOne trajMismatches)
  where
    showOne :: (String, Err Double) -> String
    showOne (name, err) = printf "relerr: %.2g, abserr: %.2g - %s - dir coll: %.2g, rk45: %.2g"
                          (errRel err) (errAbs err) name (errRef err) (errVal err)

    acs = flatten $ accessors (fill 0 :: x Double)

    stageMismatches = sortBy (flip comp) $ map (report worstStageMismatch) acs
    trajMismatches = sortBy (flip comp) $ map (report trajMismatch) acs
    comp (_,x) (_,y) = compare (errRel x) (errRel y)

    report x (name, GetDouble g, _) = (name, Err ref val abs' rel)
      where
        ref  = g (fmap errRef x)
        val  = g (fmap errVal x)
        abs' = g (fmap errAbs x)
        rel  = g (fmap errRel x)
    report _ (name, _, _) = error $ "summarizeAccuracy got a non-double getter for " ++ show name

toErr :: (Ord a, Fractional a) => Maybe a -> a -> a -> Err a
toErr mscale ref val =
  Err
  { errRef = ref
  , errVal = val
  , errAbs = abs (ref - val)
  , errRel = relerr
  }
  where
    relerr
      | ref == 0 && val == 0 = 0
      | isJust mscale = abs (ref - val) / fromJust mscale
      | ref == 0  = abs (ref - val) / (max 1e-15 (abs val))
      | val == 0  = abs (ref - val) / (max 1e-15 (abs ref))
      | otherwise = abs (ref - val) / (maximum [1e-15, abs ref, abs val])

checkIntegrationAccuracy
  :: forall x q u p n deg
  . (Vectorize x, Vectorize q, Vectorize u, Additive u, Vectorize p, Dim n, Dim deg)
  => x (Maybe Double)
  -> QuadratureRoots
  -> J (CollTraj x None u p n deg) (V.Vector Double)
  -> ( Double
       -> u Double
       -> p Double
       -> CheckState x q Double
       -> CheckState x q Double)
  -> Vec n (q Double)
  -> Checks (CheckState x q) n
checkIntegrationAccuracy xscale roots traj' ode qfs =
  Checks
  { checksStageMismatch = mismatch
  , checksWorstStageMismatch = worstStageMismatches
  , checksTrajMismatch =
    toErr <$> scale <*> CheckState (TV.tvlast xfs) (TV.tvlast qfs) <*> integratedFullTraj
  }
  where
    scale :: CheckState x q (Maybe Double)
    scale = CheckState
            { csX = xscale
            , csQ = fill Nothing
            }

    integrate :: Double
                 -> CheckState x q Double
                 -> Vec deg Double -> Vec deg (u Double)
                 -> CheckState x q Double
    integrate t0 cs0 ts us = rk45 f (InitialTime t0) (TimeStep h) cs0
      where
        f :: Double -> CheckState x q Double -> CheckState x q Double
        f t = ode t u params
          where
            u :: u Double
            u = interpolate ts us t

    integratedFullTraj :: CheckState x q Double
    integratedFullTraj = foldl' g (CheckState (TV.tvhead x0s) (TV.tvhead q0s)) foldInputs
      where
        foldInputs :: Vec n (Double, Vec deg Double, Vec deg (u Double))
        foldInputs = TV.tvzipWith3 (\y0 y1 y2 -> (y0, y1, y2)) t0s utimes fullus

        g cs0 (t0, ts, us) = integrate t0 cs0 ts us

    params = splitJV params'
    traj@(CollTraj tf params' _ _) = split traj'

    xs :: Vec n (x Double, Vec deg (x Double))
    fullus :: Vec n (Vec deg (u Double))
    ((xs, xf), _, fullus) = getXzus''' traj

    t0s :: Vec n Double
    t0s = fmap fst times

    utimes :: Vec n (Vec deg Double)
    utimes = fmap snd times

    times :: Vec n (Double, Vec deg Double)
    times = collocationTimes 0 roots h

    h = unId (splitJV tf) / fromIntegral (TV.reflectDim (Proxy :: Proxy n))

    q0s :: Vec n (q Double)
    q0s = TV.tvshiftr (fill 0) (qfs)

    x0s :: Vec n (x Double)
    x0s = fmap fst xs

    xfs :: Vec n (x Double)
    xfs = TV.tvshiftl x0s xf

    cs0s = TV.tvzipWith CheckState x0s q0s
    csfs = TV.tvzipWith CheckState xfs qfs

    integratedCsfs :: Vec n (CheckState x q Double)
    integratedCsfs = integrate <$> t0s <*> cs0s <*> utimes <*> fullus

    mismatch :: Vec n (CheckState x q (Err Double))
    mismatch = TV.tvzipWith (\ref val -> toErr <$> scale <*> ref <*> val) csfs integratedCsfs

    worstStageMismatches :: CheckState x q (Err Double)
    worstStageMismatches = fmap (maximumBy comp) (sequenceA mismatch)
      where
        comp :: Err Double -> Err Double -> Ordering
        comp x y = compare (errRel x) (errRel y)
