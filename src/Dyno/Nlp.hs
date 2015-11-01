{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Dyno.Nlp
       ( Bounds
       , Nlp(..),  NlpOut(..)
       , KKT(..)
       ) where

import GHC.Generics ( Generic )

import Casadi.DMatrix ( DMatrix )
import qualified Data.Vector as V
import Data.Binary ( Binary )
import Data.Serialize ( Serialize )
import Casadi.Viewable ( Viewable )

import Dyno.View.View ( View(..), J, S )
import Dyno.View.M ( M )

type Bounds = (Maybe Double, Maybe Double)

-- | nonlinear program (NLP)
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
  { nlpFG :: J x a -> J p a -> (S a, J g a)
  , nlpBX :: J x (V.Vector Bounds)
  , nlpBG :: J g (V.Vector Bounds)
  , nlpX0 :: J x (V.Vector Double)
  , nlpP  :: J p (V.Vector Double)
  , nlpLamX0 :: Maybe (J x (V.Vector Double))
  , nlpLamG0 :: Maybe (J g (V.Vector Double))
  , nlpScaleF :: Maybe Double
  , nlpScaleX :: Maybe (J x (V.Vector Double))
  , nlpScaleG :: Maybe (J g (V.Vector Double))
  }

-- | NLP output
data NlpOut x g a =
  NlpOut
  { fOpt :: S a
  , xOpt :: J x a
  , gOpt :: J g a
  , lambdaXOpt :: J x a
  , lambdaGOpt :: J g a
  } deriving (Eq, Show, Generic)
instance (View x, View g, Binary a, Viewable a) => Binary (NlpOut x g a)
instance (View x, View g, Serialize a, Viewable a) => Serialize (NlpOut x g a)


-- | Karush–Kuhn–Tucker (KKT) matrix
data KKT x g =
  KKT
  { kktHessLag :: M x x DMatrix -- ^ unscaled version only valid at solution
  , kktHessF :: M x x DMatrix
  , kktHessLambdaG :: M x x DMatrix -- ^ unscaled version only valid at solution
  , kktJacG :: M g x DMatrix
  , kktG :: J g DMatrix
  , kktGradF :: J x DMatrix
  , kktF :: S DMatrix
  } deriving (Generic, Eq, Show)
instance (View x, View g) => Binary (KKT x g)
instance (View x, View g) => Serialize (KKT x g)
