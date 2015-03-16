{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Types
       ( CollTraj(..)
       , CollStage(..)
       , CollPoint(..)
       , CollStageConstraints(..)
       , CollOcpConstraints(..)
       , CollTrajCov(..)
       , CollOcpCovConstraints(..)
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
       ) where

import GHC.Generics ( Generic )

import qualified Data.Foldable as F
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )

import Dyno.Ocp
import Dyno.View.Viewable ( Viewable )
import Dyno.View.View ( View(..), J, jfill )
import Dyno.View.JVec ( JVec(..), jreplicate )
import Dyno.View.Cov ( Cov )
import Dyno.View.JV ( JV, splitJV, catJV )
import Dyno.Vectorize ( Vectorize(..), Id )


-- design variables
data CollTraj ocp n deg a =
  CollTraj
  { ctTf :: J (JV Id) a
  , ctP :: J (JV (P ocp)) a
  , ctStages :: J (JVec n (CollStage (JV (X ocp)) (JV (Z ocp)) (JV (U ocp)) deg)) a
  , ctXf :: J (JV (X ocp)) a
  } deriving (Eq, Generic, Show)

-- design variables
data CollTrajCov sx ocp n deg a =
  CollTrajCov (J (Cov (JV sx)) a) (J (CollTraj ocp n deg) a)
  deriving (Eq, Generic, Show)

data CollStage x z u deg a =
  CollStage (J x a) (J (JVec deg (CollPoint x z u)) a)
  deriving (Eq, Generic, Show)

data CollPoint x z u a =
  CollPoint (J x a) (J z a) (J u a)
  deriving (Eq, Generic, Show)

-- constraints
data CollStageConstraints x deg r a =
  CollStageConstraints (J (JVec deg (JV r)) a) (J (JV x) a)
  deriving (Eq, Generic, Show)

data CollOcpConstraints ocp n deg a =
  CollOcpConstraints
  { coCollPoints :: J (JVec n (JVec deg (JV (R ocp)))) a
  , coContinuity :: J (JVec n (JV (X ocp))) a
  , coPathC :: J (JVec n (JVec deg (JV (H ocp)))) a
  , coBc :: J (JV (C ocp)) a
  } deriving (Eq, Generic, Show)

data CollOcpCovConstraints ocp n deg sh shr sc a =
  CollOcpCovConstraints
  { cocNormal :: J (CollOcpConstraints ocp n deg ) a
  , cocCovPathC :: J (JVec n sh) a
  , cocCovRobustPathC :: J (JVec n (JV shr)) a
  , cocSbc :: J sc a
  } deriving (Eq, Generic, Show)

-- View instances
instance (View x, View z, View u) => View (CollPoint x z u)
instance (View x, View z, View u, Dim deg) => View (CollStage x z u deg)
instance ( Vectorize (X ocp), Vectorize (Z ocp), Vectorize (U ocp), Vectorize (P ocp)
         , Dim n, Dim deg
         ) =>  View (CollTraj ocp n deg)
instance ( Vectorize (X ocp), Vectorize (Z ocp), Vectorize (U ocp), Vectorize (P ocp)
         , Vectorize sx
         , Dim n, Dim deg
         ) => View (CollTrajCov sx ocp n deg)

instance (Vectorize x, Vectorize r, Dim deg) => View (CollStageConstraints x deg r)
instance ( Vectorize (X ocp), Vectorize (R ocp), Vectorize (C ocp), Vectorize (H ocp)
         , Dim n, Dim deg
         ) => View (CollOcpConstraints ocp n deg)
instance ( Vectorize (X ocp), Vectorize (R ocp), Vectorize (C ocp), Vectorize (H ocp)
         , Dim n, Dim deg
         , View sh, Vectorize shr, View sc
         ) => View (CollOcpCovConstraints ocp n deg sh shr sc)


getXzus ::
  (Vectorize (X ocp), Vectorize (Z ocp), Vectorize (U ocp), Dim n, Dim deg)
  => CollTraj ocp n deg (Vector a) -> ([[X ocp a]], [[Z ocp a]], [[U ocp a]])
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
  forall ocp n deg a .
  ( Vectorize (X ocp), Vectorize (R ocp), Vectorize (H ocp), Vectorize (C ocp)
  , Dim n, Dim deg )
  => X ocp a -> R ocp a -> C ocp a -> H ocp a -> CollOcpConstraints ocp n deg (Vector a)
fillCollConstraints x r c h =
  CollOcpConstraints
  { coCollPoints = jreplicate $ jreplicate $ catJV r
  , coContinuity = jreplicate $ catJV x
  , coPathC = jreplicate $ jreplicate $ catJV h
  , coBc = catJV c
  }


fillCollTraj ::
  forall ocp n deg a .
  ( Vectorize (X ocp), Vectorize (Z ocp), Vectorize (U ocp), Vectorize (P ocp)
  , Dim n, Dim deg )
  => X ocp a -> Z ocp a -> U ocp a -> P ocp a -> a
  -> CollTraj ocp n deg (Vector a)
fillCollTraj x = fillCollTraj' x x

-- | first x argument fills the non-collocation points
fillCollTraj' ::
  forall ocp n deg a .
  ( Vectorize (X ocp), Vectorize (Z ocp), Vectorize (U ocp), Vectorize (P ocp)
  , Dim n, Dim deg )
  => X ocp a -> X ocp a -> Z ocp a -> U ocp a -> P ocp a -> a
  -> CollTraj ocp n deg (Vector a)
fillCollTraj' x' x z u p t =
  fmapCollTraj'
  (const x')
  (const x)
  (const z)
  (const u)
  (const p)
  (const t)
  (split (jfill () :: J (CollTraj ocp n deg) (Vector ())))

fmapCollTraj ::
  forall ocp1 ocp2 n deg a b .
  ( Vectorize (X ocp1), Vectorize (X ocp2)
  , Vectorize (Z ocp1), Vectorize (Z ocp2)
  , Vectorize (U ocp1), Vectorize (U ocp2)
  , Vectorize (P ocp1), Vectorize (P ocp2)
  , Dim n, Dim deg )
  => (X ocp1 a -> X ocp2 b)
  -> (Z ocp1 a -> Z ocp2 b)
  -> (U ocp1 a -> U ocp2 b)
  -> (P ocp1 a -> P ocp2 b)
  -> (a -> b)
  -> CollTraj ocp1 n deg (Vector a)
  -> CollTraj ocp2 n deg (Vector b)
fmapCollTraj fx = fmapCollTraj' fx fx

-- | first x argument maps over the non-collocation points
fmapCollTraj' ::
  forall ocp1 ocp2 n deg a b .
  ( Vectorize (X ocp1), Vectorize (X ocp2)
  , Vectorize (Z ocp1), Vectorize (Z ocp2)
  , Vectorize (U ocp1), Vectorize (U ocp2)
  , Vectorize (P ocp1), Vectorize (P ocp2)
  , Dim n, Dim deg )
  => (X ocp1 a -> X ocp2 b)
  -> (X ocp1 a -> X ocp2 b)
  -> (Z ocp1 a -> Z ocp2 b)
  -> (U ocp1 a -> U ocp2 b)
  -> (P ocp1 a -> P ocp2 b)
  -> (a -> b)
  -> CollTraj ocp1 n deg (Vector a)
  -> CollTraj ocp2 n deg (Vector b)
fmapCollTraj' fx' fx fz fu fp ft (CollTraj tf1 p stages1 xf) =
  CollTraj tf2 (fj fp p) stages2 (fj fx' xf)
  where
    tf2 :: J (JV Id) (Vector b)
    tf2 = catJV $ fmap ft (splitJV tf1)
    stages2 = cat $ fmapJVec (fmapStage fx' fx fz fu) (split stages1)

    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a) -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapJVec :: (View f, View g, Viewable a, Viewable b)
            => (f a -> g b) -> JVec deg f a -> JVec deg g b
fmapJVec f = JVec . fmap (cat . f . split) . unJVec

fmapStage :: forall x1 x2 z1 z2 u1 u2 deg a b .
             ( Vectorize x1, Vectorize x2
             , Vectorize z1, Vectorize z2
             , Vectorize u1, Vectorize u2
             , Dim deg )
             => (x1 a -> x2 b)
             -> (x1 a -> x2 b)
             -> (z1 a -> z2 b)
             -> (u1 a -> u2 b)
             -> CollStage (JV x1) (JV z1) (JV u1) deg (Vector a)
             -> CollStage (JV x2) (JV z2) (JV u2) deg (Vector b)
fmapStage fx' fx fz fu = fmapStageJ (fj fx') (fj fx) (fj fz) (fj fu)
  where
    fj :: (Vectorize f1, Vectorize f2)
          => (f1 a -> f2 b)
          -> J (JV f1) (Vector a)
          -> J (JV f2) (Vector b)
    fj f = catJV . f . splitJV

fmapStageJ :: forall x1 x2 z1 z2 u1 u2 deg a b .
              ( Viewable a, Viewable b
              , View x1, View x2
              , View z1, View z2
              , View u1, View u2
              , Dim deg )
              => (J x1 a -> J x2 b)
              -> (J x1 a -> J x2 b)
              -> (J z1 a -> J z2 b)
              -> (J u1 a -> J u2 b)
              -> CollStage x1 z1 u1 deg a
              -> CollStage x2 z2 u2 deg b
fmapStageJ fx' fx fz fu (CollStage x0 points0) = CollStage (fx' x0) points1
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
