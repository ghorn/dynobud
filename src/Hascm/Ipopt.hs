{-# OPTIONS_GHC -Wall #-}

module Hascm.Ipopt ( ipoptSolver ) where

import qualified Casadi.Wrappers.Classes.IpoptSolver as IS

import Hascm.NlpSolver ( NlpSolverStuff(..), Opt(..) )

ipoptSolver :: NlpSolverStuff IS.IpoptSolver
ipoptSolver =
  NlpSolverStuff
  { nlpConstructor = IS.ipoptSolver''
  , defaultOptions = [ ("linear_solver", Opt "ma57")
--                     , ("hessian_approximation", Opt "limited-memory")
--                     , ("fixed_variable_treatment", Opt "make_constraint") -- causes segfaults?
--                     , ("fixed_variable_treatment", Opt "make_parameter")
                     , ("max_iter", Opt (3000 :: Int))
                     , ("tol", Opt (1e-9 :: Double))
                     ]
  , solverInterruptCode = 1
  }

