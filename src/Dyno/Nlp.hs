{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Dyno.Nlp ( Nlp(..), NlpOut(..), Multipliers(..), Bounds ) where

import GHC.Generics ( Generic )
import qualified Data.Vector as V

import Dyno.View.View ( J, S )

data NlpOut x g a = NlpOut { fOpt :: J S a
                           , xOpt :: J x a
                           , gOpt :: J g a
                           , lambdaOpt :: Multipliers x g a
                           } deriving (Eq, Show, Generic)

data Multipliers x g a = Multipliers { lambdaX :: J x a
                                     , lambdaG :: J g a
                                     } deriving (Eq, Show, Generic)

type Bounds = (Maybe Double, Maybe Double)
data Nlp x p g a =
  Nlp { nlpFG :: (J x a, J p a) -> (J S a, J g a)
      , nlpBX :: J x (V.Vector Bounds)
      , nlpBG :: J g (V.Vector Bounds)
      , nlpX0 :: J x (V.Vector Double)
      , nlpP  :: J p (V.Vector Double)
      }
