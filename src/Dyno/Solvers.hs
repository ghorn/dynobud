{-# OPTIONS_GHC -Wall #-}

module Dyno.Solvers ( Solver(options, functionOptions, functionCall)
                    , Opt(..)
                    , ipoptSolver, snoptSolver, worhpSolver
                    , getSolverInternal
                    ) where

import Casadi.Core.Classes.Function ( Function )
import Casadi.Option ( Opt(..) )

import Dyno.SolverInternal ( SolverInternal(..) )

data Solver =
  Solver
  { options :: [(String,Opt)]
  , functionOptions :: [(String, Opt)]
  , functionCall :: Function -> IO ()
  , solverInternal :: SolverInternal
  }

-- | get the read-only part
getSolverInternal :: Solver -> SolverInternal
getSolverInternal = solverInternal

snoptSolver :: Solver
snoptSolver =
  Solver
  { options = []
  , functionOptions = []
  , functionCall = const (return ())
  , solverInternal =
       SolverInternal
       { solverName = "snopt"
       , defaultOptions = [ -- ("_iprint", Opt (0::Int))
--                            , ("_isumm", Opt (6::Int))
--                            , ("_scale_option", Opt (0::Int))
--                            , ("_major_iteration_limit", Opt (3 :: Int))
--                            , ("_minor_iteration_limit", Opt (2000 :: Int))
--                            , ("_verify_level", Opt (2 :: Int))
--                            , ("_optimality_tolerance", Opt (1e-1 :: Double))
--                            , ("_feasibility_tolerance", Opt (1e-1 :: Double))
--                            , ("detect_linear", Opt False)
--                            , ("monitor", Opt (V.fromList ["setup_nlp"]) )
--                            , ("_start", Opt "Warm")
                     ]
       , solverInterruptCode = -2
       , successCodes = ["1"]
       }
  }

ipoptSolver :: Solver
ipoptSolver =
  Solver
  { options = []
  , functionOptions = []
  , functionCall = const (return ())
  , solverInternal =
       SolverInternal
       { solverName = "ipopt"
       , defaultOptions = [ ("max_iter", Opt (3000 :: Int))
                          , ("tol", Opt (1e-9 :: Double))
--                          , ("hessian_approximation", Opt "limited-memory")
--                          , ("expand", Opt True)
--                          , ("linear_solver", Opt "ma27")
--                          , ("linear_solver", Opt "ma57")
--                          , ("linear_solver", Opt "ma86")
--                          , ("linear_solver", Opt "ma97")
--                          , ("fixed_variable_treatment", Opt "make_constraint") -- causes segfaults?
--                          , ("fixed_variable_treatment", Opt "make_parameter")
                          ]
       , solverInterruptCode = 1
       , successCodes = ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
       }
  }

worhpSolver :: Solver
worhpSolver =
  Solver
  { options = []
  , functionOptions = []
  , functionCall = const (return ())
  , solverInternal =
       SolverInternal
       { solverName = "worhp"
       , defaultOptions = []
       , solverInterruptCode = 1
       , successCodes = [ "OptimalSolution"
                        , "LowPassFilterOptimal"
                        ]
       }
  }



