{-# OPTIONS_GHC -Wall -ddump-splices #-}

module ExampleDsl.Types
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

import Casadi.SX ( SX )
import Dyno.View.View ( J )
import Dyno.View.JV ( JV )
import Dyno.Vectorize ( Id )

data Constraint a = Eq2 a a
                  | Ineq2 a a
                  | Ineq3 a (Double, Double)

data Objective a = ObjectiveUnset | Objective a
data HomotopyParam a = HomotopyParamUnset | HomotopyParam a

type SXElement = J (JV Id) SX

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

data DaeState = DaeState { _daeXDot :: S.Seq (String, SXElement)
                         , _daeX :: S.Seq (String, SXElement)
                         , _daeZ :: S.Seq (String, SXElement)
                         , _daeU :: S.Seq (String, SXElement)
                         , _daeP :: S.Seq (String, SXElement)
                         , _daeO :: M.Map String SXElement
                         , daeNameSet :: HS.HashSet String
                         , daeConstraints :: S.Seq (SXElement, SXElement)
                         }

--makeLenses ''DaeState
daeXDot :: Lens' DaeState (S.Seq (String, SXElement))
daeXDot f (DaeState xdot' x z u p o ss c) =
  (\xdot -> DaeState xdot x z u p o ss c) `fmap` f xdot'
{-# INLINE daeXDot #-}

daeX :: Lens' DaeState (S.Seq (String, SXElement))
daeX f (DaeState xdot x' z u p o ss c) =
  (\x -> DaeState xdot x z u p o ss c) `fmap` f x'
{-# INLINE daeX #-}

daeZ :: Lens' DaeState (S.Seq (String, SXElement))
daeZ f (DaeState xdot x z' u p o ss c) =
  (\z -> DaeState xdot x z u p o ss c) `fmap` f z'
{-# INLINE daeZ #-}

daeU :: Lens' DaeState (S.Seq (String, SXElement))
daeU f (DaeState xdot x z u' p o ss c) =
  (\u -> DaeState xdot x z u p o ss c) `fmap` f u'
{-# INLINE daeU #-}

daeP :: Lens' DaeState (S.Seq (String, SXElement))
daeP f (DaeState xdot x z u p' o ss c) =
  (\p -> DaeState xdot x z u p o ss c) `fmap` f p'
{-# INLINE daeP #-}

daeO :: Lens' DaeState (M.Map String SXElement)
daeO f (DaeState xdot x z u p o' ss c) =
  (\o -> DaeState xdot x z u p o ss c) `fmap` f o'
{-# INLINE daeO #-}
