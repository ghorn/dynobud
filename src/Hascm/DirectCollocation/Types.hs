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

--import Dvda
--data RorL = Radau | Legendre deriving (Eq, Show)


data CollTraj x z u p n deg a = CollTraj a (p a) (Vec n (CollStage x z u deg a)) (x a) deriving (Eq, Functor, Generic, Generic1) -- endtime, params, coll stages, xf
data CollStage x z u deg a = CollStage (x a) (Vec deg (CollPoint x z u a)) deriving (Eq, Functor, Generic, Generic1)
data CollPoint x z u a = CollPoint (x a) (z a) (u a) deriving (Eq, Functor, Generic, Generic1)

instance (Serialize (x a), Serialize (z a), Serialize (u a), Serialize (p a), Serialize a) => Serialize (CollTraj x z u p n deg a)
instance (Serialize (x a), Serialize (z a), Serialize (u a)) => Serialize (CollStage x z u deg a)
instance (Serialize (x a), Serialize (z a), Serialize (u a)) => Serialize (CollPoint x z u a)

getX :: CollPoint x z u a -> x a
getX (CollPoint x _ _) = x

ctDeg :: forall x z u p n deg a . Dim deg => CollTraj x z u p n deg a -> Int
ctDeg _ = reflectDim (Proxy :: Proxy deg)

ctN :: forall x z u p n deg a . Dim n => CollTraj x z u p n deg a -> Int
ctN _ = reflectDim (Proxy :: Proxy n)


instance (Vectorize x, Vectorize z, Vectorize u) => Vectorize (CollPoint x z u)
instance (Vectorize x, Vectorize z, Vectorize u, Dim deg) => Vectorize (CollStage x z u deg)
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Dim n, Dim deg) =>
         Vectorize (CollTraj x z u p n deg)

data CollDynConstraint deg r a =
  CollDynConstraint (Vec deg (r a))
  deriving (Eq, Functor, Generic1)
instance (Vectorize r, Dim deg) =>
         Vectorize (CollDynConstraint deg r)

data CollStageConstraints x deg r a =
  CollStageConstraints (CollDynConstraint deg r a) (x a)
  deriving (Eq, Functor, Generic1)
instance (Vectorize x, Vectorize r, Dim deg) =>
         Vectorize (CollStageConstraints x deg r)

data CollTrajConstraints n x deg r a =
  CollTrajConstraints (Vec n (CollStageConstraints x deg r a))
  deriving (Eq, Functor, Generic1)
instance (Vectorize x, Vectorize r, Dim n, Dim deg) =>
         Vectorize (CollTrajConstraints n x deg r)

data CollOcpConstraints n deg x r c h a =
  CollOcpConstraints
  { coDynamics :: CollTrajConstraints n x deg r a
  , coPathC :: Vec n (Vec deg (h a))
  , coBc :: c a
  } deriving (Eq, Functor, Generic1)
instance (Vectorize x, Vectorize r, Dim n, Dim deg, Vectorize c, Vectorize h) =>
         Vectorize (CollOcpConstraints n deg x r c h)
