{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Interpolate
       ( interpolateTraj
       , interpolateConstraints
       ) where

import qualified Data.Traversable as T
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Linear.V
import Linear ( lerp )

import Dyno.View.Unsafe.View ( unJ, mkJ )
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( JV )
import Dyno.View.JVec
import Dyno.TypeVecs ( Vec )
import Dyno.Vectorize ( Vectorize )
import qualified Dyno.TypeVecs as TV
import qualified Dyno.LagrangePolynomials as LP
import Dyno.DirectCollocation.Types ( CollTraj(..), CollStage(..), CollPoint(..), CollOcpConstraints(..) )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots, timesFromTaus, mkTaus )


type TimeVal f deg = (Double, Vec deg (Double, J f (V.Vector Double)), Double)

-- a zipper to avoid quadratic lookup
data TimeZ deg f = TimeZ [TimeVal f deg] (TimeVal f deg) [TimeVal f deg]


closestTime :: TimeZ deg f -> Double -> (TimeZ deg f, Vec deg (Double, J f (V.Vector Double)))
-- time too big and we have another value
closestTime (TimeZ ls mid@(_, _, t1) (r:rs)) t
  | t > t1 = closestTime (TimeZ (mid:ls) r rs) t
-- time too big and we don't have another value
closestTime tz@(TimeZ _ (t0, xs, t1) []) t
  | t > t1 + 0.5*(t1 - t0) = error $ "requested time which is too big " ++ show (t0, t, t1)
  | t > t1 = (tz, xs)
-- braket ok
closestTime tz@(TimeZ _ (t0, xs, _) _) t
  | t0 <= t = (tz, xs)
  | otherwise = error "time isn't increasing monotonically"


interp ::
  forall f deg
  . (Dim deg, View f)
  => TimeZ deg f -> Double -> (TimeZ deg f, J f (V.Vector Double))
interp tz0 t = (tz, ret)
  where
    txs :: Vec deg (Double, J f (V.Vector Double))
    (tz, txs) = closestTime tz0 t

    ts :: Vec deg Double
    xs :: Vec deg (J f (V.Vector Double))
    (ts,xs) = TV.tvunzip txs

    ret :: J f (V.Vector Double)
    ret = mkJ $ LP.interpolate ts (fmap unJ xs) t


type Point x z u = CollPoint (JV x) (JV z) (JV u)
newtype Times deg a = Times (a, Vec deg a) deriving Functor

-- | re-discretize a collocation trajectory using the lagrange interpolation polynomials
-- from the quadrature scheme
interpolateTraj ::
  forall x z u p n0 n1 deg0 deg1
  . ( Dim n0, Dim n1, Dim deg0, Dim deg1
    , Vectorize x, Vectorize z, Vectorize u
    )
  => Vec deg0 Double
  -> CollTraj x z u p n0 deg0 (V.Vector Double)
  -> QuadratureRoots
  -> CollTraj x z u p n1 deg1 (V.Vector Double)
interpolateTraj taus0 traj0 roots1 = traj0 { ctStages = cat (JVec (fmap cat stages1)) }
  where
    n0 = reflectDim (Proxy :: Proxy n0)
    n1 = reflectDim (Proxy :: Proxy n1)

    tf = 1.0 -- could be anything, returned traj doesn't use this it uses the correct tf
    dt0 = tf / fromIntegral n0
    dt1 = tf / fromIntegral n1

    taus1 :: Vec deg1 Double
    taus1 = mkTaus roots1

    times0' :: Vec n0 (Double, Vec deg0 Double)
    times0' = timesFromTaus 0 taus0 dt0

    stages0 :: Vec n0 (Vec deg0 (J (CollPoint (JV x) (JV z) (JV u)) (V.Vector Double)))
    stages0 = fmap (points . split) $ unJVec $ split (ctStages traj0)
    points (CollStage _ ps) = unJVec (split ps)

    times0 :: Vec n0 (TimeVal (Point x z u) deg0)
    times0 = TV.tvzipWith3 (\(t0,ts) t1 xs -> (t0, TV.tvzip ts xs, t1))
             times0' (TV.tvshiftl (fmap fst times0') tf) stages0

    tzip0 :: TimeZ deg0 (Point x z u)
    tzip0 = case F.toList times0 of
      [] -> error "can't interpolate with 0 length input"
      (x:xs) -> TimeZ [] x xs

    times1 :: Vec n1 (Double, Vec deg1 Double)
    times1 = timesFromTaus 0 taus1 dt1

    stages1 :: Vec n1 (CollStage (JV x) (JV z) (JV u) deg1 (V.Vector Double))
    stages1 = snd $ T.mapAccumL foo tzip0 times1

    foo :: TimeZ deg0 (Point x z u)
           -> (Double, Vec deg1 Double)

           -> ( TimeZ deg0 (Point x z u)
              , CollStage (JV x) (JV z) (JV u) deg1 (V.Vector Double)
              )
    foo timez0 (t0, ts) = (timezf, CollStage x0 (cat (JVec xzus)))
      where
        CollPoint x0 _ _ = split xzu0
        (timez1, xzu0) = interp timez0 t0
        (timezf, xzus) = T.mapAccumL interp timez1 ts


-- | Re-discretize collocation constraints using the lagrange interpolation polynomials
-- from the quadrature scheme. This is useful for lagrange multipliers.
interpolateConstraints ::
  forall x r c h n0 n1 deg0 deg1
  . ( Dim n0, Dim n1, Dim deg0, Dim deg1
    , Vectorize x, Vectorize r, Vectorize c, Vectorize h
    )
  => Vec deg0 Double
  -> CollOcpConstraints x r c h n0 deg0 (V.Vector Double)
  -> QuadratureRoots
  -> CollOcpConstraints x r c h n1 deg1 (V.Vector Double)
interpolateConstraints taus0 con0 roots1 = con1
  where
    con1 = CollOcpConstraints
           { coCollPoints =  go' (coCollPoints con0)
           , coPathC = go' (coPathC con0)
           , coContinuity = cat (JVec cont)
           , coBc = coBc con0
           }

    cont0 :: Vec n0 (J (JV x) (V.Vector Double))
    cont0 = unJVec $ split (coContinuity con0)

    tc0 :: Vec n0 (Double, J (JV x) (V.Vector Double))
    tc0 = TV.tvzip (fmap fst times0') cont0

    cont :: Vec n1 (J (JV x) (V.Vector Double))
    cont = snd $ T.mapAccumL linterp (F.toList tc0) (fmap fst times1)

    go' :: forall s
           . Vectorize s
           => J (JVec n0 (JVec deg0 (JV s))) (V.Vector Double)
           -> J (JVec n1 (JVec deg1 (JV s))) (V.Vector Double)
    go' x = cat $ JVec $ fmap (cat . JVec) (go (fmap (unJVec . split) (unJVec (split x))))

    n0 = reflectDim (Proxy :: Proxy n0)
    n1 = reflectDim (Proxy :: Proxy n1)

    tf = 1.0 -- could be anything
    dt0 = tf / fromIntegral n0
    dt1 = tf / fromIntegral n1

    taus1 :: Vec deg1 Double
    taus1 = mkTaus roots1

    times0' :: Vec n0 (Double, Vec deg0 Double)
    times0' = timesFromTaus 0 taus0 dt0

    times1 :: Vec n1 (Double, Vec deg1 Double)
    times1 = timesFromTaus 0 taus1 dt1

    go :: forall s
          . View s
          => Vec n0 (Vec deg0 (J s (V.Vector Double)))
          -> Vec n1 (Vec deg1 (J s (V.Vector Double)))
    go x0s = x1s
      where
        x1s :: Vec n1 (Vec deg1 (J s (V.Vector Double)))
        x1s = snd $ T.mapAccumL (T.mapAccumL interp) tzip0' (fmap snd times1)

        tzip0' :: TimeZ deg0 s
        tzip0' = case F.toList times00 of
          [] -> error "can't interpolate with 0 length input"
          (x:xs) -> TimeZ [] x xs

        times00 :: Vec n0 (TimeVal s deg0)
        times00 = TV.tvzipWith3 (\(t0,ts) t1 xs -> (t0, TV.tvzip ts xs, t1))
                  times0' (TV.tvshiftl (fmap fst times0') tf) x0s



linterp :: View s
           => [(Double, J s (V.Vector Double))]
           -> Double
           -> ([(Double, J s (V.Vector Double))], J s (V.Vector Double))
-- if t is too big and there are others available
linterp (_:others@((t1,_):_:_)) t
  | t > t1 = linterp others t
linterp acc@((t0,x0):(t1,x1):_) t = (acc, mkJ (lerp ((t - t0) / (t1 - t0)) (unJ x0) (unJ x1)))
linterp _ _ = error "linear interpolation ran out of nodes"
