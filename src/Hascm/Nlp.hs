{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Hascm.Nlp ( Nlp(..), NlpInputs(..), NlpFun(..), NlpOut(..), Multipliers(..) ) where

import Hascm.Vectorize

data NlpOut x g a = NlpOut { fOpt :: a
                           , xOpt :: x a
                           , gOpt :: g a
                           , lambdaOpt :: Multipliers x g a
                           } deriving (Eq, Show, Functor, Generic1)

data Multipliers x g a = Multipliers { lambdaX :: x a
                                     , lambdaG :: g a
                                     } deriving (Eq, Show, Functor, Generic1)

data NlpFun g a = NlpFun a (g a) deriving (Eq, Show, Functor, Generic1)
instance Vectorize g => Vectorize (NlpFun g)

data NlpInputs x p a = NlpInputs (x a) (p a) deriving (Eq, Show, Functor, Generic1)
instance (Vectorize x, Vectorize p) => Vectorize (NlpInputs x p)

data Nlp x p g =
  Nlp { nlpFG :: Floating a => NlpInputs x p a -> NlpFun g a
      , nlpBX :: x (Maybe Double, Maybe Double)
      , nlpBG :: g (Maybe Double, Maybe Double)
      }
