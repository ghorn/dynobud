{-# OPTIONS_GHC -Wall -ddump-splices #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Hascm.Interface.Types
       ( Constraint(..)
       , Objective(..)
       , HomotopyParam(..)
       , NlpState(..)
       , OcpState(..)
       , DaeState(..)
       , daeX
       , daeXDot
       , daeZ
       , daeU
       , daeP
       ) where

import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Control.Lens
import Data.Functor ( (<$>) )

import Dvda.Expr ( Expr(..), Sym(..) )

data Constraint a = Eq2 a a
                  | Ineq2 a a
--                  | Ineq3 a a a

data Objective a = ObjectiveUnset | Objective a
data HomotopyParam a = HomotopyParamUnset | HomotopyParam a

data NlpState = NlpState { nlpX :: S.Seq Sym
                         , nlpXSet :: HS.HashSet Sym
                         , nlpConstraints :: S.Seq (Constraint (Expr Double))
                         , nlpObj :: Objective (Expr Double)
                         , nlpHomoParam :: HomotopyParam (Expr Double)
                         }

data OcpState = OcpState { ocpPathConstraints :: S.Seq (Constraint (Expr Double))
                         , ocpLagrangeObj :: Objective (Expr Double)
                         , ocpHomoParam :: HomotopyParam (Expr Double)
                         }

data DaeState = DaeState { _daeXDot :: S.Seq Sym
                         , _daeX :: S.Seq Sym
                         , _daeZ :: S.Seq Sym
                         , _daeU :: S.Seq Sym
                         , _daeP :: S.Seq Sym
                         --, daeOutputs :: HS.HashMap String (Expr Double)
                         , daeNameSet :: HS.HashSet String
                         , daeConstraints :: S.Seq (Expr Double, Expr Double)
                         }

--makeLenses ''DaeState
daeXDot :: Lens' DaeState (S.Seq Sym)
daeXDot f (DaeState xdot' x z u p ss c) =
  (\xdot -> DaeState xdot x z u p ss c) <$> (f xdot')
{-# INLINE daeXDot #-}

daeX :: Lens' DaeState (S.Seq Sym)
daeX f (DaeState xdot x' z u p ss c) =
  (\x -> DaeState xdot x z u p ss c) <$> (f x')
{-# INLINE daeX #-}

daeZ :: Lens' DaeState (S.Seq Sym)
daeZ f (DaeState xdot x z' u p ss c) =
  (\z -> DaeState xdot x z u p ss c) <$> (f z')
{-# INLINE daeZ #-}

daeU :: Lens' DaeState (S.Seq Sym)
daeU f (DaeState xdot x z u' p ss c) =
  (\u -> DaeState xdot x z u p ss c) <$> (f u')
{-# INLINE daeU #-}

daeP :: Lens' DaeState (S.Seq Sym)
daeP f (DaeState xdot x z u p' ss c) =
  (\p -> DaeState xdot x z u p ss c) <$> (f p')
{-# INLINE daeP #-}
