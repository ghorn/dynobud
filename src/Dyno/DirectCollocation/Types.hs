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

import Dyno.View
import Dyno.Cov ( Cov )

--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)

-- design variables
data CollTraj x z u p s n deg a =
  CollTraj (J S a) (J (Cov s) a) (J p a) (J (JVec n (CollStage x z u deg)) a) (J x a)
  deriving (Eq, Generic, Show)
  -- endtime, params, coll stages, xf

data CollStage x z u deg a = CollStage (J x a) (J (JVec deg (CollPoint x z u)) a)
                           deriving (Eq, Generic, Show)

data CollPoint x z u a = CollPoint (J x a) (J z a) (J u a)
                       deriving (Eq, Generic, Show)

-- constraints
data CollStageConstraints x deg r sh a =
  CollStageConstraints (J (JVec deg r) a) (J x a) (J sh a)
  deriving (Eq, Generic, Show)

data CollOcpConstraints n deg x r c h sh sc a =
  CollOcpConstraints
  { coStages :: J (JVec n (CollStageConstraints x deg r sh)) a
  , coPathC :: J (JVec n (JVec deg h)) a
  , coBc :: J c a
  , coSbc :: J sc a
  } deriving (Eq, Generic, Show)

-- serialize instances
instance (Serialize (J x a), Serialize (J z a), Serialize (J u a)) =>
         Serialize (CollPoint x z u a)
instance (Serialize a, Serialize (J x a), Serialize (J z a), Serialize (J u a)) =>
         Serialize (CollStage x z u deg a)
instance (Serialize (x a), Serialize (z a), Serialize (u a), Serialize (p a), Serialize (Cov s a), Serialize a) =>
         Serialize (CollTraj x z u p s n deg a)

-- View instances
instance (View x, View z, View u) => View (CollPoint x z u)
instance (View x, View z, View u, Dim deg) => View (CollStage x z u deg)
instance (View x, View z, View u, View p, View (Cov s), Dim n, Dim deg) =>
         View (CollTraj x z u p s n deg)

instance (View x, View r, View sh, Dim deg) => View (CollStageConstraints x deg r sh)
instance (View x, View r, Dim n, Dim deg, View c, View h, View sh, View sc) =>
         View (CollOcpConstraints n deg x r c h sh sc)

-- getters
getX :: CollPoint x z u a -> J x a
getX (CollPoint x _ _) = x

ctDeg :: forall x z u p s n deg a . Dim deg => CollTraj x z u p s n deg a -> Int
ctDeg _ = reflectDim (Proxy :: Proxy deg)

ctN :: forall x z u p s n deg a . Dim n => CollTraj x z u p s n deg a -> Int
ctN _ = reflectDim (Proxy :: Proxy n)
