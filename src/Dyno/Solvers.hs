{-# OPTIONS_GHC -Wall #-}

module Dyno.Solvers ( Solver(..)
                    , Opt(..)
                    , ipoptSolver, snoptSolver
                    ) where

import Casadi.Core.Classes.Function ( Function )
import Casadi.Option ( Opt(..) )

data Solver =
  Solver
  { solverName :: String
  , defaultOptions :: [(String,Opt)]
  , options :: [(String,Opt)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  , functionOptions :: [(String, Opt)]
  , functionCall :: Function -> IO ()
  }

snoptSolver :: Solver
snoptSolver =
  Solver
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
  , functionOptions = []
  , functionCall = const (return ())
  }

ipoptSolver :: Solver
ipoptSolver =
  Solver
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
  , functionOptions = []
  , functionCall = const (return ())
  }
