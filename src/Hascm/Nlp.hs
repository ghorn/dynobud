{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Hascm.Nlp ( Nlp(..), NlpFun(..) ) where

import Hascm.Vectorize

data NlpFun g a = NlpFun a (g a) deriving (Show, Functor, Generic1)
instance Vectorize g => Vectorize (NlpFun g)

data Nlp x g =
  Nlp { nlpFG :: Floating a => x a -> NlpFun g a
      , nlpBX :: x (Maybe Double, Maybe Double)
      , nlpBG :: g (Maybe Double, Maybe Double)
      }
