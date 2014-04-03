{-# OPTIONS_GHC -Wall -ddump-splices #-}

module Dyno.Interface.Types
       ( Constraint(..)
       , Objective(..)
       , HomotopyParam(..)
       , NlpMonadState(..)
       , OcpState(..)
       , DaeState(..)
       , daeX
       , daeXDot
       , daeZ
       , daeU
       , daeP
       , daeO
       ) where

import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import qualified Data.Map as M
import Control.Lens
import Data.Functor ( (<$>) )

import Dyno.Casadi.SXElement ( SXElement )

data Constraint a = Eq2 a a
                  | Ineq2 a a
                  | Ineq3 a (Double, Double)

data Objective a = ObjectiveUnset | Objective a
data HomotopyParam a = HomotopyParamUnset | HomotopyParam a

data NlpMonadState =
  NlpMonadState
  { nlpX :: S.Seq (String, SXElement)
  , nlpXSet :: HS.HashSet String
  , nlpConstraints :: S.Seq (Constraint SXElement)
  , nlpObj :: Objective SXElement
  , nlpHomoParam :: HomotopyParam SXElement
  }

data OcpState = OcpState { ocpPathConstraints :: S.Seq (Constraint SXElement)
                         , ocpLagrangeObj :: Objective SXElement
                         , ocpHomoParam :: HomotopyParam SXElement
                         }

data DaeState = DaeState { _daeXDot :: S.Seq SXElement
                         , _daeX :: S.Seq SXElement
                         , _daeZ :: S.Seq SXElement
                         , _daeU :: S.Seq SXElement
                         , _daeP :: S.Seq SXElement
                         , _daeO :: M.Map String SXElement
                         , daeNameSet :: HS.HashSet String
                         , daeConstraints :: S.Seq (SXElement, SXElement)
                         }

--makeLenses ''DaeState
daeXDot :: Lens' DaeState (S.Seq SXElement)
daeXDot f (DaeState xdot' x z u p o ss c) =
  (\xdot -> DaeState xdot x z u p o ss c) <$> f xdot'
{-# INLINE daeXDot #-}

daeX :: Lens' DaeState (S.Seq SXElement)
daeX f (DaeState xdot x' z u p o ss c) =
  (\x -> DaeState xdot x z u p o ss c) <$> f x'
{-# INLINE daeX #-}

daeZ :: Lens' DaeState (S.Seq SXElement)
daeZ f (DaeState xdot x z' u p o ss c) =
  (\z -> DaeState xdot x z u p o ss c) <$> f z'
{-# INLINE daeZ #-}

daeU :: Lens' DaeState (S.Seq SXElement)
daeU f (DaeState xdot x z u' p o ss c) =
  (\u -> DaeState xdot x z u p o ss c) <$> f u'
{-# INLINE daeU #-}

daeP :: Lens' DaeState (S.Seq SXElement)
daeP f (DaeState xdot x z u p' o ss c) =
  (\p -> DaeState xdot x z u p o ss c) <$> f p'
{-# INLINE daeP #-}

daeO :: Lens' DaeState (M.Map String SXElement)
daeO f (DaeState xdot x z u p o' ss c) =
  (\o -> DaeState xdot x z u p o ss c) <$> f o'
{-# INLINE daeO #-}
