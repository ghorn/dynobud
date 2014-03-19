{-# OPTIONS_GHC -Wall #-}

module Dyno.Ipopt ( IS.IpoptSolver, ipoptSolver ) where

import qualified Casadi.Wrappers.Classes.IpoptSolver as IS

import Dyno.NlpSolver ( NlpSolverStuff(..), Opt(..) )

ipoptSolver :: NlpSolverStuff IS.IpoptSolver
ipoptSolver =
  NlpSolverStuff
  { nlpConstructor = IS.ipoptSolver''
  , defaultOptions = [ ("max_iter", Opt (3000 :: Int))
                     , ("tol", Opt (1e-9 :: Double))
--                     , ("linear_solver", Opt "ma27")
--                     , ("linear_solver", Opt "ma57")
                     , ("linear_solver", Opt "ma86")
--                     , ("linear_solver", Opt "ma97")
--                     , ("hessian_approximation", Opt "limited-memory")
--                     , ("fixed_variable_treatment", Opt "make_constraint") -- causes segfaults?
--                     , ("fixed_variable_treatment", Opt "make_parameter")
                     ]
  , solverInterruptCode = 1
  , successCodes = ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
  }

