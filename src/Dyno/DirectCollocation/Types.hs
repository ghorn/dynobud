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
       ) where

import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Linear.V ( Dim(..) )

import Dyno.View ( View, J, JV, JVec, S )
import Dyno.Vectorize ( Vectorize )
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
