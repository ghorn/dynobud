{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Dyno.Nlp
       ( Bounds
       , Nlp(..), NlpOut(..), Multipliers(..)
       , Nlp'(..), NlpOut'(..), Multipliers'(..)
       ) where

import GHC.Generics ( Generic )
import qualified Data.Vector as V

import Dyno.View.View ( J, S )

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
  }

data NlpOut x g a =
  NlpOut
  { fOpt :: a
  , xOpt :: x a
  , gOpt :: g a
  , lambdaOpt :: Multipliers x g a
  } deriving (Eq, Show, Generic)

data Multipliers x g a =
  Multipliers
  { lambdaX :: x a
  , lambdaG :: g a
  } deriving (Eq, Show, Generic)


-- | NLP using Views
data NlpOut' x g a =
  NlpOut'
  { fOpt' :: J S a
  , xOpt' :: J x a
  , gOpt' :: J g a
  , lambdaOpt' :: Multipliers' x g a
  } deriving (Eq, Show, Generic)

data Multipliers' x g a =
  Multipliers'
  { lambdaX' :: J x a
  , lambdaG' :: J g a
  } deriving (Eq, Show, Generic)

data Nlp' x p g a =
  Nlp'
  { nlpFG' :: J x a -> J p a -> (J S a, J g a)
  , nlpBX' :: J x (V.Vector Bounds)
  , nlpBG' :: J g (V.Vector Bounds)
  , nlpX0' :: J x (V.Vector Double)
  , nlpP'  :: J p (V.Vector Double)
  }
