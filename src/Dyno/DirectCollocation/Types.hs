{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveGeneric #-}

module Dyno.DirectCollocation.Types
       ( CollTraj(..)
       , CollStage(..)
       , CollPoint(..)
       , CollStageConstraints(..)
       , CollOcpConstraints(..)
       , CollTrajCov(..)
       , CollOcpCovConstraints(..)
       , getX
       , fmapCollTraj
       , fmapStage
       , fmapCollPoint
       ) where

import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )

import Dyno.View ( View(..), Viewable, J, JV, JVec(..), S, mkJ, unJ )
import Dyno.Vectorize ( Vectorize(..) )
import Dyno.Cov ( Cov )

--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)

-- design variables
data CollTraj x z u p n deg a =
  CollTraj (J S a) (J (JV p) a) (J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) a) (J (JV x) a)
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

data CollOcpCovConstraints n deg x r c h sh sc a =
  CollOcpCovConstraints
  { cocNormal :: J (CollOcpConstraints n deg x r c h) a
  , cocCovPathC :: J (JVec n sh) a
  , cocSbc :: J sc a
  } deriving (Eq, Generic, Show)

-- serialize instances
instance Serialize a => Serialize (CollPoint x z u a)
instance Serialize a => Serialize (CollStage x z u deg a)
instance Serialize a => Serialize (CollTraj x z u p n deg a)
instance Serialize a => Serialize (CollTrajCov sx x z u p n deg a)

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
instance (Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c, Vectorize h, View sh, View sc) =>
         View (CollOcpCovConstraints n deg x r c h sh sc)

-- getters
getX :: CollPoint x z u a -> J x a
getX (CollPoint x _ _) = x


fmapCollTraj ::
  forall x1 x2 z1 z2 u1 u2 p1 p2 n deg a .
  ( Vectorize x1, Vectorize x2
  , Vectorize z1, Vectorize z2
  , Vectorize u1, Vectorize u2
  , Vectorize p1, Vectorize p2
  , Dim n, Dim deg
  , Show a )
  => (x1 a -> x2 a)
  -> (z1 a -> z2 a)
  -> (u1 a -> u2 a)
  -> (p1 a -> p2 a)
  -> CollTraj x1 z1 u1 p1 n deg (Vector a)
  -> CollTraj x2 z2 u2 p2 n deg (Vector a)
fmapCollTraj fx fz fu fp (CollTraj tf p stages1 xf) = CollTraj tf (fj fp p) stages2 (fj fx xf)
  where
    stages2 = cat $ fmapJVec (fmapStage fx fz fu) (split stages1)

    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 a)
          -> J (JV f1) (Vector a) -> J (JV f2) (Vector a)
    fj f = mkJ . vectorize . f . devectorize . unJ

fmapJVec :: (View f, View g, Viewable a)
            => (f a -> g a) -> JVec deg f a -> JVec deg g a
fmapJVec f = JVec . fmap (cat . f . split) . unJVec

fmapStage :: forall x1 x2 z1 z2 u1 u2 deg a .
             ( Vectorize x1, Vectorize x2
             , Vectorize z1, Vectorize z2
             , Vectorize u1, Vectorize u2
             , Dim deg
             , Show a )
             => (x1 a -> x2 a)
             -> (z1 a -> z2 a)
             -> (u1 a -> u2 a)
             -> CollStage (JV x1) (JV z1) (JV u1) deg (Vector a)
             -> CollStage (JV x2) (JV z2) (JV u2) deg (Vector a)
fmapStage fx fz fu (CollStage x0 points0) = CollStage (fj fx x0) points1
  where
    points1 = cat $ fmapJVec (fmapCollPoint fx fz fu) (split points0)

    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 a)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector a)
    fj f = mkJ . vectorize . f . devectorize . unJ

fmapCollPoint :: forall x1 x2 z1 z2 u1 u2 a .
                 ( Vectorize x1, Vectorize x2
                 , Vectorize z1, Vectorize z2
                 , Vectorize u1, Vectorize u2
                 , Show a )
                 => (x1 a -> x2 a)
                 -> (z1 a -> z2 a)
                 -> (u1 a -> u2 a)
                 -> CollPoint (JV x1) (JV z1) (JV u1) (Vector a)
                 -> CollPoint (JV x2) (JV z2) (JV u2) (Vector a)
fmapCollPoint fx fz fu (CollPoint x z u) = CollPoint (fj fx x) (fj fz z) (fj fu u)
  where
    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 a)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector a)
    fj f = mkJ . vectorize . f . devectorize . unJ
