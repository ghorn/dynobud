{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Hascm.Nlp ( Nlp(..), NlpInputs(..), NlpFun(..) ) where

import Hascm.Vectorize

data NlpFun g a = NlpFun a (g a) deriving (Show, Functor, Generic1)
instance Vectorize g => Vectorize (NlpFun g)

data NlpInputs x p a = NlpInputs (x a) (p a) deriving (Show, Functor, Generic1)
instance (Vectorize x, Vectorize p) => Vectorize (NlpInputs x p)

data Nlp x p g =
  Nlp { nlpFG :: Floating a => NlpInputs x p a -> NlpFun g a
      , nlpBX :: x (Maybe Double, Maybe Double)
      , nlpBG :: g (Maybe Double, Maybe Double)
      }
