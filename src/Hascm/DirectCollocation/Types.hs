{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Hascm.DirectCollocation.Types
       ( CollTraj(..)
       , CollStage(..)
       , CollPoint(..)
       , CollTrajConstraints(..)
       , CollDynConstraint(..)
       , CollStageConstraints(..)
       , CollOcpConstraints(..)
       , getX
       , ctDeg
       , ctN
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Linear.V

import Hascm.Vectorize
import Hascm.TypeVecs ( Vec )
import Hascm.Cov ( Cov )

--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)

-- design variables
data CollTraj x z u p s n deg a = CollTraj a (Cov s a) (p a) (Vec n (CollStage x z u deg a)) (x a)
                                deriving (Eq, Functor, Generic, Generic1)
                                         -- endtime, params, coll stages, xf

data CollStage x z u deg a = CollStage (x a) (Vec deg (CollPoint x z u a))
                           deriving (Eq, Functor, Generic, Generic1)

data CollPoint x z u a = CollPoint (x a) (z a) (u a)
                       deriving (Eq, Functor, Generic, Generic1)

-- constraints
data CollDynConstraint deg r a = CollDynConstraint (Vec deg (r a))
                               deriving (Eq, Functor, Generic1)

data CollStageConstraints x deg r sh a = CollStageConstraints (CollDynConstraint deg r a) (x a) (sh a)
                                    deriving (Eq, Functor, Generic1)

data CollTrajConstraints n x deg r sh a =
  CollTrajConstraints (Vec n (CollStageConstraints x deg r sh a))
  deriving (Eq, Functor, Generic1)

data CollOcpConstraints n deg x r c h sh sc a =
  CollOcpConstraints
  { coStages :: CollTrajConstraints n x deg r sh a
  , coPathC :: Vec n (Vec deg (h a))
  , coBc :: c a
  , coSbc :: sc a
  } deriving (Eq, Functor, Generic1)

-- serialize instances
instance (Serialize (x a), Serialize (z a), Serialize (u a)) =>
         Serialize (CollPoint x z u a)
instance (Serialize (x a), Serialize (z a), Serialize (u a)) =>
         Serialize (CollStage x z u deg a)
instance (Serialize (x a), Serialize (z a), Serialize (u a), Serialize (p a), Serialize (Cov s a), Serialize a) =>
         Serialize (CollTraj x z u p s n deg a)

-- vectorize instances
instance (Vectorize x, Vectorize z, Vectorize u) => Vectorize (CollPoint x z u)
instance (Vectorize x, Vectorize z, Vectorize u, Dim deg) => Vectorize (CollStage x z u deg)
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize (Cov s), Dim n, Dim deg) =>
         Vectorize (CollTraj x z u p s n deg)

instance (Vectorize r, Dim deg) => Vectorize (CollDynConstraint deg r)
instance (Vectorize x, Vectorize r, Vectorize sh, Dim deg) =>
         Vectorize (CollStageConstraints x deg r sh)
instance (Vectorize x, Vectorize r, Vectorize sh, Dim n, Dim deg) =>
         Vectorize (CollTrajConstraints n x deg r sh)
instance (Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c,
          Vectorize h, Vectorize sh, Vectorize sc) =>
         Vectorize (CollOcpConstraints n deg x r c h sh sc)

-- getters
getX :: CollPoint x z u a -> x a
getX (CollPoint x _ _) = x

ctDeg :: forall x z u p s n deg a . Dim deg => CollTraj x z u p s n deg a -> Int
ctDeg _ = reflectDim (Proxy :: Proxy deg)

ctN :: forall x z u p s n deg a . Dim n => CollTraj x z u p s n deg a -> Int
ctN _ = reflectDim (Proxy :: Proxy n)
