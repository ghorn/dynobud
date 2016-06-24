{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.DirectCollocation.Types
       ( CollTraj(..)
       , CollTraj'
       , CollStage(..)
       , CollPoint(..)
       , CollStageConstraints(..)
       , CollOcpConstraints'
       , CollOcpConstraints(..)
       , fillCollTraj
       , fillCollTraj'
       , fmapCollTraj
       , fmapCollTraj'
       , fmapStage
       , fmapStageJ
       , fmapCollPoint
       , fmapCollPointJ
       , fillCollConstraints
       , getXzus
       , getXzus'
       , getXzus''
       , getXzus'''
       , fromXzus
         -- * for callbacks
       , Quadratures(..)
       , StageOutputs(..)
       , StageOutputs'
       , StageOutputsPoint(..)
         -- * robust
       , CollTrajCov(..)
       , CollOcpCovConstraints(..)
       ) where

import GHC.Generics ( Generic, Generic1 )

import Linear ( Additive(..) )
import Linear.V ( Dim(..) )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Binary ( Binary )
import Data.Vector ( Vector )

import Casadi.Viewable ( Viewable )
import Accessors ( Lookup )

import Dyno.Ocp
import Dyno.View.View ( View(..), J, S, JV, splitJV, catJV, jfill )
import Dyno.View.JVec ( JVec(..), jreplicate )
import Dyno.View.Cov ( Cov )
import Dyno.View.Vectorize ( Vectorize(..), Id(..), unId, vapply )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV


-- | CollTraj using type families to compress type parameters
type CollTraj' ocp n deg = CollTraj (X ocp) (Z ocp) (U ocp) (P ocp) n deg

-- design variables
data CollTraj x z u p n deg a =
  CollTraj
  { ctTf :: S a
  , ctP :: J (JV p) a
  , ctStages :: J (JVec n (CollStage (JV x) (JV z) (JV u) (JV p) deg)) a
  , ctXf :: J (JV x) a
  } deriving (Eq, Generic, Show)

-- design variables
data CollTrajCov sx x z u p n deg a =
  CollTrajCov (J (Cov (JV sx)) a) (J (CollTraj x z u p n deg) a)
  deriving (Eq, Generic, Show)

data CollStage x z u p deg a =
  CollStage (J x a) (J (JVec deg (CollPoint x z u)) a) (J p a) (S a)
  deriving (Eq, Generic, Show)

data CollPoint x z u a =
  CollPoint (J x a) (J z a) (J u a)
  deriving (Eq, Generic, Show)

-- constraints
data CollStageConstraints x deg r a =
  CollStageConstraints (J (JVec deg (JV r)) a) (J (JV x) a)
  deriving (Eq, Generic, Show)

-- | CollOcpConstraints using type families to compress type parameters
type CollOcpConstraints' ocp n deg = CollOcpConstraints (X ocp) (P ocp) (R ocp) (C ocp) (H ocp) n deg

data CollOcpConstraints x p r c h n deg a =
  CollOcpConstraints
  { coCollPoints :: J (JVec n (JVec deg (JV r))) a
  , coContinuity :: J (JVec n (JV x)) a
  , coPathC :: J (JVec n (JVec deg (JV h))) a
  , coBc :: J (JV c) a
  , coParams :: J (JVec n (JV p)) a
  , coTfs :: J (JVec n (JV Id)) a
  } deriving (Eq, Generic, Show)

data CollOcpCovConstraints ocp n deg sh shr sc a =
  CollOcpCovConstraints
  { cocNormal :: J (CollOcpConstraints' ocp n deg ) a
  , cocCovPathC :: J (JVec n sh) a
  , cocCovRobustPathC :: J (JVec n (JV shr)) a
  , cocSbc :: J sc a
  } deriving (Eq, Generic, Show)

-- View instances
instance (View x, View z, View u) => View (CollPoint x z u)
instance (View x, View z, View u, View p, Dim deg) => View (CollStage x z u p deg)
instance ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
         , Dim n, Dim deg
         ) =>  View (CollTraj x z u p n deg)
instance ( Vectorize sx, Vectorize x, Vectorize z, Vectorize u, Vectorize p
         , Dim n, Dim deg
         ) => View (CollTrajCov sx x z u p n deg)

instance (Vectorize x, Vectorize r, Dim deg) => View (CollStageConstraints x deg r)
instance ( Vectorize x, Vectorize p, Vectorize r, Vectorize c, Vectorize h
         , Dim n, Dim deg
         ) => View (CollOcpConstraints x p r c h n deg)
instance ( Vectorize (X ocp), Vectorize (P ocp), Vectorize (R ocp), Vectorize (C ocp), Vectorize (H ocp)
         , Dim n, Dim deg
         , View sh, Vectorize shr, View sc
         ) => View (CollOcpCovConstraints ocp n deg sh shr sc)

-- todo(greg): unit test to ensure this is the inverse of getXzus'
fromXzus :: forall x z u p n deg a
            . (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg)
            => a -> p a -> Vec n (x a, Vec deg (x a, z a, u a)) -> x a
            -> CollTraj x z u p n deg (Vector a)
fromXzus t' p' xzus xf = CollTraj t p (cat (JVec traj)) (catJV xf)
  where
    t = catJV (Id t')
    p = catJV p'

    traj :: Vec n (J (CollStage (JV x) (JV z) (JV u) (JV p) deg) (Vector a))
    traj = fmap (cat . toCollStage) xzus

    toCollStage :: (x a, Vec deg (x a, z a, u a)) -> CollStage (JV x) (JV z) (JV u) (JV p) deg (Vector a)
    toCollStage (x0, xzus') = CollStage (catJV x0) (cat (JVec (fmap toCollPoint xzus'))) p t

    toCollPoint :: (x a, z a, u a) -> J (CollPoint (JV x) (JV z) (JV u)) (Vector a)
    toCollPoint (x,z,u) = cat $ CollPoint (catJV x) (catJV z) (catJV u)

getXzus ::
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg)
  => CollTraj x z u p n deg (Vector a)
  -> (Vec n (Vec deg (x a, z a, u a)))
getXzus traj = fmap snd4 $ fst $ getXzus' traj
  where
    snd4 (_,x,_,_) = x

getXzus' ::
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg)
  => CollTraj x z u p n deg (Vector a)
  -> (Vec n (x a, Vec deg (x a, z a, u a), p a, a), x a)
getXzus' (CollTraj _ _ stages xf) =
  (fmap (getXzusFromStage . split) (unJVec (split stages)), splitJV xf)

getXzus'' ::
  forall x z u p n deg a
  . (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg)
  => CollTraj x z u p n deg (Vector a)
  -> ( Vec n (Vec deg (x a))
     , Vec n (Vec deg (z a))
     , Vec n (Vec deg (u a))
     )
getXzus'' traj = (fmap snd xs, zs, us)
  where
    ((xs,_),zs,us) = getXzus''' traj

getXzus''' ::
  forall x z u p n deg a
  . (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg)
  => CollTraj x z u p n deg (Vector a)
  -> ( ( Vec n (x a, Vec deg (x a))
       , x a
       )
     , Vec n (Vec deg (z a))
     , Vec n (Vec deg (u a))
     )
getXzus''' traj = ((xs, xf), zs, us)
  where
    (xzus, xf) = getXzus' traj
    (xs, zs, us) = TV.tvunzip3 $ fmap f xzus
      where
        f (x0, xzus', _, _) = ((x0,xs'), zs', us')
          where
            (xs',zs',us') = TV.tvunzip3 xzus'

getXzusFromStage :: (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim deg)
                    => CollStage (JV x) (JV z) (JV u) (JV p) deg (Vector a)
                    -> (x a, Vec deg (x a, z a, u a), p a, a)
getXzusFromStage (CollStage x0 xzus p tf) = (splitJV x0, fmap (f . split) (unJVec (split xzus)), splitJV p, unId (splitJV tf))
  where
    f (CollPoint x z u) = (splitJV x, splitJV z, splitJV u)


fillCollConstraints ::
  forall x p r c h n deg a .
  ( Vectorize x, Vectorize p, Vectorize r, Vectorize c, Vectorize h
  , Dim n, Dim deg )
  => x a -> p a -> r a -> c a -> h a -> a -> CollOcpConstraints x p r c h n deg (Vector a)
fillCollConstraints x p r c h tf =
  CollOcpConstraints
  { coCollPoints = jreplicate $ jreplicate $ catJV r
  , coContinuity = jreplicate $ catJV x
  , coPathC = jreplicate $ jreplicate $ catJV h
  , coBc = catJV c
  , coParams = jreplicate (catJV p)
  , coTfs = jreplicate (catJV (Id tf))
  }


fillCollTraj ::
  forall x z u p n deg a .
  ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Dim n, Dim deg )
  => x a -> z a -> u a -> p a -> a
  -> CollTraj x z u p n deg (Vector a)
fillCollTraj x = fillCollTraj' x x

-- | first x argument fills the non-collocation points
fillCollTraj' ::
  forall x z u p n deg a .
  ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Dim n, Dim deg )
  => x a -> x a -> z a -> u a -> p a -> a
  -> CollTraj x z u p n deg (Vector a)
fillCollTraj' x' x z u p t =
  fmapCollTraj'
  (const x')
  (const x)
  (const z)
  (const u)
  (const p)
  (const t)
  (split (jfill () :: J (CollTraj x z u p n deg) (Vector ())))

fmapCollTraj ::
  forall x0 z0 u0 p0 x1 z1 u1 p1 n deg a b .
  ( Vectorize x0, Vectorize x1
  , Vectorize z0, Vectorize z1
  , Vectorize u0, Vectorize u1
  , Vectorize p0, Vectorize p1
  , Dim n, Dim deg )
  => (x0 a -> x1 b)
  -> (z0 a -> z1 b)
  -> (u0 a -> u1 b)
  -> (p0 a -> p1 b)
  -> (a -> b)
  -> CollTraj x0 z0 u0 p0 n deg (Vector a)
  -> CollTraj x1 z1 u1 p1 n deg (Vector b)
fmapCollTraj fx = fmapCollTraj' fx fx

-- | first x argument maps over the non-collocation points
fmapCollTraj' ::
  forall x0 z0 u0 p0 x1 z1 u1 p1 n deg a b .
  ( Vectorize x0, Vectorize x1
  , Vectorize z0, Vectorize z1
  , Vectorize u0, Vectorize u1
  , Vectorize p0, Vectorize p1
  , Dim n, Dim deg )
  => (x0 a -> x1 b)
  -> (x0 a -> x1 b)
  -> (z0 a -> z1 b)
  -> (u0 a -> u1 b)
  -> (p0 a -> p1 b)
  -> (a -> b)
  -> CollTraj x0 z0 u0 p0 n deg (Vector a)
  -> CollTraj x1 z1 u1 p1 n deg (Vector b)
fmapCollTraj' fx' fx fz fu fp ft (CollTraj tf1 p stages1 xf) =
  CollTraj tf2 (fj fp p) stages2 (fj fx' xf)
  where
    tf2 :: S (Vector b)
    tf2 = catJV $ fmap ft (splitJV tf1)
    stages2 = cat $ fmapJVec (fmapStage fx' fx fz fu fp ft) (split stages1)

    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a) -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapJVec :: (View f, View g, Viewable a, Viewable b)
            => (f a -> g b) -> JVec deg f a -> JVec deg g b
fmapJVec f = JVec . fmap (cat . f . split) . unJVec

fmapStage :: forall x1 x2 z1 z2 u1 u2 p1 p2 deg a b .
             ( Vectorize x1, Vectorize x2
             , Vectorize z1, Vectorize z2
             , Vectorize u1, Vectorize u2
             , Vectorize p1, Vectorize p2
             , Dim deg )
             => (x1 a -> x2 b)
             -> (x1 a -> x2 b)
             -> (z1 a -> z2 b)
             -> (u1 a -> u2 b)
             -> (p1 a -> p2 b)
             -> (a -> b)
             -> CollStage (JV x1) (JV z1) (JV u1) (JV p1) deg (Vector a)
             -> CollStage (JV x2) (JV z2) (JV u2) (JV p2) deg (Vector b)
fmapStage fx' fx fz fu fp ft = fmapStageJ (fj fx') (fj fx) (fj fz) (fj fu) (fj fp) (catJV . fmap ft . splitJV)
  where
    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapStageJ :: forall x1 x2 z1 z2 u1 u2 p1 p2 deg a b .
              ( Viewable a, Viewable b
              , View x1, View x2
              , View z1, View z2
              , View u1, View u2
              , Dim deg )
              => (J x1 a -> J x2 b)
              -> (J x1 a -> J x2 b)
              -> (J z1 a -> J z2 b)
              -> (J u1 a -> J u2 b)
              -> (J p1 a -> J p2 b)
              -> (S a -> S b)
              -> CollStage x1 z1 u1 p1 deg a
              -> CollStage x2 z2 u2 p2 deg b
fmapStageJ fx' fx fz fu fp ftf (CollStage x0 points0 p0 tf0) = CollStage (fx' x0) points1 (fp p0) (ftf tf0)
  where
    points1 = cat $ fmapJVec (fmapCollPointJ fx fz fu) (split points0)

fmapCollPoint :: forall x1 x2 z1 z2 u1 u2 a b .
                 ( Vectorize x1, Vectorize x2
                 , Vectorize z1, Vectorize z2
                 , Vectorize u1, Vectorize u2 )
                 => (x1 a -> x2 b)
                 -> (z1 a -> z2 b)
                 -> (u1 a -> u2 b)
                 -> CollPoint (JV x1) (JV z1) (JV u1) (Vector a)
                 -> CollPoint (JV x2) (JV z2) (JV u2) (Vector b)
fmapCollPoint fx fz fu = fmapCollPointJ (fj fx) (fj fz) (fj fu)
  where
    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapCollPointJ :: forall x1 x2 z1 z2 u1 u2 a b .
                  ( View x1, View x2
                  , View z1, View z2
                  , View u1, View u2 )
                  => (J x1 a -> J x2 b)
                  -> (J z1 a -> J z2 b)
                  -> (J u1 a -> J u2 b)
                  -> CollPoint x1 z1 u1 a
                  -> CollPoint x2 z2 u2 b
fmapCollPointJ fx fz fu (CollPoint x z u) = CollPoint (fx x) (fz z) (fu u)

-- | for callbacks
data Quadratures q qo a =
  Quadratures
  { qLagrange :: a
  , qUser :: q a
  , qOutputs :: qo a
  } deriving (Functor, Generic, Generic1)
instance (Vectorize q, Vectorize qo) => Vectorize (Quadratures q qo)
instance (Vectorize q, Vectorize qo) => Applicative (Quadratures q qo) where
  pure = fill
  (<*>) = vapply
instance (Vectorize q, Vectorize qo) => Additive (Quadratures q qo) where
  zero = fill 0
instance (Lookup a, Lookup (q a), Lookup (qo a)) => Lookup (Quadratures q qo a)
instance (Binary a, Binary (q a), Binary (qo a)) => Binary (Quadratures q qo a)
instance (FromJSON a, FromJSON (q a), FromJSON (qo a)) => FromJSON (Quadratures q qo a)
instance (ToJSON a, ToJSON (q a), ToJSON (qo a)) => ToJSON (Quadratures q qo a)

-- | for callbacks
data StageOutputsPoint x o h q qo po a =
  StageOutputsPoint
  { sopT :: a
  , sopO :: J (JV o) (Vector a)
  , sopXDot :: J (JV x) (Vector a)
  , sopH :: J (JV h) (Vector a)
  , sopPo :: J (JV po) (Vector a)
  , sopQs :: Quadratures q qo a -- qs
  , sopQDots :: Quadratures q qo a -- qdots
  } deriving Generic
instance ( Binary a, Binary (q a), Binary (qo a)
         , Vectorize x, Vectorize o, Vectorize h, Vectorize po
         ) => (Binary (StageOutputsPoint x o h q qo po a))

-- | for callbacks
data StageOutputs x o h q qo po deg a =
  StageOutputs
  { soT0 :: Double
  , soX0 :: J (JV x) (Vector a)
  , soVec :: Vec deg (StageOutputsPoint x o h q qo po a)
  , soXNext :: J (JV x) (Vector a)
  , soQNext :: Quadratures q qo a
  } deriving Generic

type StageOutputs' ocp deg = StageOutputs (X ocp) (O ocp) (H ocp) (Q ocp) (QO ocp) (PO ocp) deg

instance ( Binary a, Binary (q a), Binary (qo a)
         , Vectorize x, Vectorize o, Vectorize h, Vectorize po
         , Dim deg
         ) => (Binary (StageOutputs x o h q qo po deg a))
