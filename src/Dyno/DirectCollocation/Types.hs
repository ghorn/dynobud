{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

module Dyno.DirectCollocation.Types
       ( CollTraj(..)
       , CollStage(..)
       , CollPoint(..)
       , CollStageConstraints(..)
       , CollOcpConstraints(..)
       , CollTrajCov(..)
       , CollOcpCovConstraints(..)
       , fillCollTraj
       , fmapCollTraj
       , fmapStage
       , fmapCollPoint
       , fillCollConstraints
       , getXzus
       ) where

import qualified Data.Foldable as F
import GHC.Generics ( Generic )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )

import Dyno.View ( View(..), J, JVec(..), jfill, jreplicate )
import Dyno.View.Cov ( Cov )
import Dyno.View.JV ( JV, splitJV, catJV )
import Dyno.Vectorize ( Vectorize(..), Id )


-- design variables
data CollTraj x z u p n deg a =
  CollTraj (J (JV Id) a) (J (JV p) a) (J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) a) (J (JV x) a)
  deriving (Eq, Generic, Show)
  -- endtime, params, coll stages, xf

-- design variables
data CollTrajCov sx x z u p n deg a =
  CollTrajCov (J (Cov (JV sx)) a) (J (CollTraj x z u p n deg) a)
  deriving (Eq, Generic, Show)
  -- endtime, params, coll stages, xf

data CollStage x z u deg a = CollStage (J x a) (J (JVec deg (CollPoint x z u)) a)
                           deriving (Eq, Generic, Show)

data CollPoint x z u a = CollPoint (J x a) (J z a) (J u a)
                       deriving (Eq, Generic, Show)

-- constraints
data CollStageConstraints x deg r a =
  CollStageConstraints (J (JVec deg (JV r)) a) (J (JV x) a)
  deriving (Eq, Generic, Show)

data CollOcpConstraints n deg x r c h a =
  CollOcpConstraints
  { coCollPoints :: J (JVec n (JVec deg (JV r))) a
  , coContinuity :: J (JVec n (JV x)) a
  , coPathC :: J (JVec n (JVec deg (JV h))) a
  , coBc :: J (JV c) a
  } deriving (Eq, Generic, Show)

data CollOcpCovConstraints n deg x r c h sh shr sc a =
  CollOcpCovConstraints
  { cocNormal :: J (CollOcpConstraints n deg x r c h) a
  , cocCovPathC :: J (JVec n sh) a
  , cocCovRobustPathC :: J (JVec n (JV shr)) a
  , cocSbc :: J sc a
  } deriving (Eq, Generic, Show)

-- View instances
instance (View x, View z, View u) => View (CollPoint x z u)
instance (View x, View z, View u, Dim deg) => View (CollStage x z u deg)
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg) =>
         View (CollTraj x z u p n deg)
instance (Vectorize sx, Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg) =>
         View (CollTrajCov sx x z u p n deg)

instance (Vectorize x, Vectorize r, Dim deg) => View (CollStageConstraints x deg r)
instance (Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c, Vectorize h) =>
         View (CollOcpConstraints n deg x r c h)
instance ( Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c, Vectorize h
         , View sh, Vectorize shr, View sc
         ) => View (CollOcpCovConstraints n deg x r c h sh shr sc)


getXzus ::
  (Vectorize x, Vectorize z, Vectorize u, Dim n, Dim deg)
  => CollTraj x z u p n deg (Vector a) -> ([[x a]], [[z a]], [[u a]])
getXzus (CollTraj _ _ stages xf) = (xs ++ [[splitJV xf]], zs, us)
  where
    (xs, zs, us) = unzip3 $ map (getXzus' . split) (F.toList (unJVec (split stages)))

getXzus' :: (Vectorize x, Vectorize z, Vectorize u, Dim deg)
            => CollStage (JV x) (JV z) (JV u) deg (Vector a) -> ([x a], [z a], [u a])
getXzus' (CollStage x0 xzus) = (splitJV x0 : xs, zs, us)
  where
    (xs, zs, us) = unzip3 $ map (f . split) (F.toList (unJVec (split xzus)))
    f (CollPoint x z u) = (splitJV x, splitJV z, splitJV u)

fillCollConstraints ::
  forall x r c h n deg a .
  (Vectorize x, Vectorize r, Vectorize h, Vectorize c,
   Dim n, Dim deg, Show a)
  => x a -> r a -> c a -> h a -> CollOcpConstraints n deg x r c h (Vector a)
fillCollConstraints x r c h =
  CollOcpConstraints
  { coCollPoints = jreplicate $ jreplicate $ catJV r
  , coContinuity = jreplicate $ catJV x
  , coPathC = jreplicate $ jreplicate $ catJV h
  , coBc = catJV c
  }


fillCollTraj ::
  forall x z u p n deg a .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Dim n, Dim deg, Show a)
  => x a -> z a -> u a -> p a -> a -> CollTraj x z u p n deg (Vector a)
fillCollTraj x z u p t =
  fmapCollTraj
  (const x)
  (const z)
  (const u)
  (const p)
  (const t)
  (split (jfill () :: J (CollTraj x z u p n deg) (Vector ())))

fmapCollTraj ::
  forall x1 x2 z1 z2 u1 u2 p1 p2 n deg a b .
  ( Vectorize x1, Vectorize x2
  , Vectorize z1, Vectorize z2
  , Vectorize u1, Vectorize u2
  , Vectorize p1, Vectorize p2
  , Dim n, Dim deg
  , Show a, Show b )
  => (x1 a -> x2 b)
  -> (z1 a -> z2 b)
  -> (u1 a -> u2 b)
  -> (p1 a -> p2 b)
  -> (a -> b)
  -> CollTraj x1 z1 u1 p1 n deg (Vector a)
  -> CollTraj x2 z2 u2 p2 n deg (Vector b)
fmapCollTraj fx fz fu fp ft (CollTraj tf1 p stages1 xf) = CollTraj tf2 (fj fp p) stages2 (fj fx xf)
  where
    tf2 :: J (JV Id) (Vector b)
    tf2 = catJV $ fmap ft (splitJV tf1)
    stages2 = cat $ fmapJVec (fmapStage fx fz fu) (split stages1)

    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a) -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapJVec :: (View f, View g, Show a, Show b)
            => (f (Vector a) -> g (Vector b)) -> JVec deg f (Vector a) -> JVec deg g (Vector b)
fmapJVec f = JVec . fmap (cat . f . split) . unJVec

fmapStage :: forall x1 x2 z1 z2 u1 u2 deg a b .
             ( Vectorize x1, Vectorize x2
             , Vectorize z1, Vectorize z2
             , Vectorize u1, Vectorize u2
             , Dim deg
             , Show a, Show b )
             => (x1 a -> x2 b)
             -> (z1 a -> z2 b)
             -> (u1 a -> u2 b)
             -> CollStage (JV x1) (JV z1) (JV u1) deg (Vector a)
             -> CollStage (JV x2) (JV z2) (JV u2) deg (Vector b)
fmapStage fx fz fu (CollStage x0 points0) = CollStage (fj fx x0) points1
  where
    points1 = cat $ fmapJVec (fmapCollPoint fx fz fu) (split points0)

    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapCollPoint :: forall x1 x2 z1 z2 u1 u2 a b .
                 ( Vectorize x1, Vectorize x2
                 , Vectorize z1, Vectorize z2
                 , Vectorize u1, Vectorize u2
                 , Show a, Show b )
                 => (x1 a -> x2 b)
                 -> (z1 a -> z2 b)
                 -> (u1 a -> u2 b)
                 -> CollPoint (JV x1) (JV z1) (JV u1) (Vector a)
                 -> CollPoint (JV x2) (JV z2) (JV u2) (Vector b)
fmapCollPoint fx fz fu (CollPoint x z u) = CollPoint (fj fx x) (fj fz z) (fj fu u)
  where
    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV
