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

import Casadi.MX ( MX )
import Dyno.View.View ( S )

data Constraint a = Eq2 a a
                  | Ineq2 a a
                  | Ineq3 a (Double, Double)

data Objective a = ObjectiveUnset | Objective a
data HomotopyParam a = HomotopyParamUnset | HomotopyParam a

type MXElement = S MX

data NlpMonadState =
  NlpMonadState
  { nlpX :: S.Seq (String, MXElement)
  , nlpXSet :: HS.HashSet String
  , nlpConstraints :: S.Seq (Constraint MXElement)
  , nlpObj :: Objective MXElement
  , nlpHomoParam :: HomotopyParam MXElement
  }

data OcpState = OcpState { ocpPathConstraints :: S.Seq (Constraint MXElement)
                         , ocpLagrangeObj :: Objective MXElement
                         , ocpHomoParam :: HomotopyParam MXElement
                         }

data DaeState = DaeState { _daeXDot :: S.Seq (String, MXElement)
                         , _daeX :: S.Seq (String, MXElement)
                         , _daeZ :: S.Seq (String, MXElement)
                         , _daeU :: S.Seq (String, MXElement)
                         , _daeP :: S.Seq (String, MXElement)
                         , _daeO :: M.Map String MXElement
                         , daeNameSet :: HS.HashSet String
                         , daeConstraints :: S.Seq (MXElement, MXElement)
                         }

--makeLenses ''DaeState
daeXDot :: Lens' DaeState (S.Seq (String, MXElement))
daeXDot f (DaeState xdot' x z u p o ss c) =
  (\xdot -> DaeState xdot x z u p o ss c) `fmap` f xdot'
{-# INLINE daeXDot #-}

daeX :: Lens' DaeState (S.Seq (String, MXElement))
daeX f (DaeState xdot x' z u p o ss c) =
  (\x -> DaeState xdot x z u p o ss c) `fmap` f x'
{-# INLINE daeX #-}

daeZ :: Lens' DaeState (S.Seq (String, MXElement))
daeZ f (DaeState xdot x z' u p o ss c) =
  (\z -> DaeState xdot x z u p o ss c) `fmap` f z'
{-# INLINE daeZ #-}

daeU :: Lens' DaeState (S.Seq (String, MXElement))
daeU f (DaeState xdot x z u' p o ss c) =
  (\u -> DaeState xdot x z u p o ss c) `fmap` f u'
{-# INLINE daeU #-}

daeP :: Lens' DaeState (S.Seq (String, MXElement))
daeP f (DaeState xdot x z u p' o ss c) =
  (\p -> DaeState xdot x z u p o ss c) `fmap` f p'
{-# INLINE daeP #-}

daeO :: Lens' DaeState (M.Map String MXElement)
daeO f (DaeState xdot x z u p o' ss c) =
  (\o -> DaeState xdot x z u p o ss c) `fmap` f o'
{-# INLINE daeO #-}
