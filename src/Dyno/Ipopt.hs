{-# OPTIONS_GHC -Wall #-}

module Dyno.Ipopt ( IS.IpoptSolver, ipoptSolver ) where

import qualified Casadi.IpoptInterface.Classes.IpoptSolver as IS

import Dyno.NlpSolver ( NlpSolverStuff(..), Opt(..) )

ipoptSolver :: NlpSolverStuff IS.IpoptSolver
ipoptSolver =
  NlpSolverStuff
  { nlpConstructor = IS.ipoptSolver__0
  , defaultOptions = [ ("max_iter", Opt (3000 :: Int))
                     , ("tol", Opt (1e-9 :: Double))
--                     , ("hessian_approximation", Opt "limited-memory")
--                     , ("expand", Opt True)
--                     , ("linear_solver", Opt "ma27")
--                     , ("linear_solver", Opt "ma57")
--                     , ("linear_solver", Opt "ma86")
--                     , ("linear_solver", Opt "ma97")
--                     , ("fixed_variable_treatment", Opt "make_constraint") -- causes segfaults?
--                     , ("fixed_variable_treatment", Opt "make_parameter")
                     ]
  , options = []
  , solverInterruptCode = 1
  , successCodes = ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
  }

