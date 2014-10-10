{-# OPTIONS_GHC -Wall #-}

module Dyno.Solvers ( NlpSolverStuff(..), ipoptSolver, snoptSolver ) where

--import qualified Data.Vector as V

import Dyno.NlpSolver ( NlpSolverStuff(..), Opt(..) )

snoptSolver :: NlpSolverStuff
snoptSolver =
  NlpSolverStuff
  { solverName = "snopt"
  , defaultOptions = [ -- ("_iprint", Opt (0::Int))
--                       , ("_isumm", Opt (6::Int))
--                       , ("_scale_option", Opt (0::Int))
--                       , ("_major_iteration_limit", Opt (3 :: Int))
--                       , ("_minor_iteration_limit", Opt (2000 :: Int))
--                       , ("_verify_level", Opt (2 :: Int))
--                       , ("_optimality_tolerance", Opt (1e-1 :: Double))
--                       , ("_feasibility_tolerance", Opt (1e-1 :: Double))
--                       , ("detect_linear", Opt False)
--                       , ("monitor", Opt (V.fromList ["setup_nlp"]) )
--                       , ("_start", Opt "Warm")
                     ]
  , options = []
  , solverInterruptCode = -2
  , successCodes = ["1"]
  }

ipoptSolver :: NlpSolverStuff
ipoptSolver =
  NlpSolverStuff
  { solverName = "ipopt"
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
