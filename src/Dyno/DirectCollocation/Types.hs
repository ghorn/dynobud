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
       , getX
       , ctDeg
       , ctN
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Linear.V ( Dim(..) )

import Dyno.View ( View, J, JV, JVec, S )
import Dyno.Vectorize ( Vectorize )
import Dyno.Cov ( Cov )

--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)

-- design variables
data CollTraj x z u p s n deg a =
  CollTraj (J S a) (J (Cov s) a) (J (JV p) a) (J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) a) (J (JV x) a)
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

data CollOcpConstraints n deg x r c h sh sc a =
  CollOcpConstraints
  { coCollPoints :: J (JVec n (JVec deg (JV r))) a
  , coContinuity :: J (JVec n (JV x)) a
  , coPathC :: J (JVec n (JVec deg (JV h))) a
  , coCovPathC :: J (JVec n sh) a
  , coBc :: J (JV c) a
  , coSbc :: J sc a
  } deriving (Eq, Generic, Show)

-- serialize instances
instance Serialize a => Serialize (CollPoint x z u a)
instance Serialize a => Serialize (CollStage x z u deg a)
instance Serialize a => Serialize (CollTraj x z u p s n deg a)

-- View instances
instance (View x, View z, View u) => View (CollPoint x z u)
instance (View x, View z, View u, Dim deg) => View (CollStage x z u deg)
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p, View (Cov s), Dim n, Dim deg) =>
         View (CollTraj x z u p s n deg)

instance (Vectorize x, Vectorize r, Dim deg) => View (CollStageConstraints x deg r)
instance (Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c, Vectorize h, View sh, View sc) =>
         View (CollOcpConstraints n deg x r c h sh sc)

-- getters
getX :: CollPoint x z u a -> J x a
getX (CollPoint x _ _) = x

ctDeg :: forall x z u p s n deg a . Dim deg => CollTraj x z u p s n deg a -> Int
ctDeg _ = reflectDim (Proxy :: Proxy deg)

ctN :: forall x z u p s n deg a . Dim n => CollTraj x z u p s n deg a -> Int
ctN _ = reflectDim (Proxy :: Proxy n)
