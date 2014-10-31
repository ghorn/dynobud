{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Dyno.Nlp
       ( Bounds
       , Nlp(..),  NlpOut(..)
       , Nlp'(..), NlpOut'(..)
       ) where

import GHC.Generics ( Generic, Generic1 )
import qualified Data.Vector as V
import Data.Serialize ( Serialize(..) )

import Dyno.Vectorize ( Vectorize(..) )
import Dyno.View.View ( View(..), J, S, unJ, mkJ )

type Bounds = (Maybe Double, Maybe Double)

-- | user-friendly NLP
--
-- >  minimize         f(x,p)
-- >     x
-- >
-- > subject to   xlb <=  x   <= xub
-- >              glb <= g(x) <= gub
--
-- where p is some parameter
--
data Nlp x p g a =
  Nlp
  { nlpFG :: x a -> p a -> (a, g a)
  , nlpBX :: x Bounds
  , nlpBG :: g Bounds
  , nlpX0 :: x Double
  , nlpP  :: p Double
  , nlpLamX0 :: Maybe (x Double)
  , nlpLamG0 :: Maybe (g Double)
  , nlpScaleF :: Maybe Double
  , nlpScaleX :: Maybe (x Double)
  , nlpScaleG :: Maybe (g Double)
  }

data NlpOut x g a =
  NlpOut
  { fOpt :: a
  , xOpt :: x a
  , gOpt :: g a
  , lambdaXOpt :: x a
  , lambdaGOpt :: g a
  } deriving (Eq, Show, Functor, Generic, Generic1)
instance (Vectorize x, Vectorize g) => Vectorize (NlpOut x g)
instance (Vectorize x, Vectorize g, Serialize a) => Serialize (NlpOut x g a) where
  put = put . V.toList . vectorize
  get = fmap (devectorize . V.fromList) get

-- | NLP using Views
data NlpOut' x g a =
  NlpOut'
  { fOpt' :: J S a
  , xOpt' :: J x a
  , gOpt' :: J g a
  , lambdaXOpt' :: J x a
  , lambdaGOpt' :: J g a
  } deriving (Eq, Show, Generic)
instance (View x, View g) => View (NlpOut' x g)
instance (View x, View g, Serialize a) => Serialize (NlpOut' x g (V.Vector a)) where
  put = put . V.toList . unJ . cat
  get = fmap (split . mkJ . V.fromList) get


data Nlp' x p g a =
  Nlp'
  { nlpFG' :: J x a -> J p a -> (J S a, J g a)
  , nlpBX' :: J x (V.Vector Bounds)
  , nlpBG' :: J g (V.Vector Bounds)
  , nlpX0' :: J x (V.Vector Double)
  , nlpP'  :: J p (V.Vector Double)
  , nlpLamX0' :: Maybe (J x (V.Vector Double))
  , nlpLamG0' :: Maybe (J g (V.Vector Double))
  , nlpScaleF' :: Maybe Double
  , nlpScaleX' :: Maybe (J x (V.Vector Double))
  , nlpScaleG' :: Maybe (J g (V.Vector Double))
  }
