{-# OPTIONS_GHC -Wall -ddump-splices #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Hascm.Interface.Types
       ( Constraint(..)
       , Objective(..)
       , HomotopyParam(..)
       , NlpState(..)
       ) where

import qualified Data.HashSet as HS
import qualified Data.Sequence as S

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
