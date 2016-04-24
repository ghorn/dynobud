{-# OPTIONS_GHC -Wall #-}

module Dyno.Solvers ( Solver(options, runnerOptions), RunNlpOptions(..)
                    , GType(..)
                    , ipoptSolver, snoptSolver, worhpSolver
                    , getSolverInternal
                    ) where

import Casadi.GenericType ( GType(..) )

import Dyno.SolverInternal ( SolverInternal(..) )

data Solver =
  Solver
  { options :: [(String, GType)]
  , runnerOptions :: RunNlpOptions
  , solverInternal :: SolverInternal
  }

defaultRunnerOptions :: RunNlpOptions
defaultRunnerOptions =
  RunNlpOptions
  { verbose = False
  }

data RunNlpOptions =
  RunNlpOptions
  { verbose :: Bool
  }

-- | get the read-only part
getSolverInternal :: Solver -> SolverInternal
getSolverInternal = solverInternal

snoptSolver :: Solver
snoptSolver =
  Solver
  { options = []
  , runnerOptions = defaultRunnerOptions
  , solverInternal =
       SolverInternal
       { solverName = "snopt"
       , defaultSolverOptions =
             [ -- ("_iprint", Opt (0::Int))
--             , ("_isumm", Opt (6::Int))
--             , ("_scale_option", Opt (0::Int))
--             , ("_major_iteration_limit", Opt (3 :: Int))
--             , ("_minor_iteration_limit", Opt (2000 :: Int))
--             , ("_verify_level", Opt (2 :: Int))
--             , ("_optimality_tolerance", Opt (1e-1 :: Double))
--             , ("_feasibility_tolerance", Opt (1e-1 :: Double))
--             , ("detect_linear", Opt False)
--             , ("monitor", Opt (V.fromList ["setup_nlp"]) )
--             , ("_start", Opt "Warm")
             ]
       , solverInterruptCode = -2
       , successCodes = ["1"]
       }
  }

ipoptSolver :: Solver
ipoptSolver =
  Solver
  { options = []
  , runnerOptions = defaultRunnerOptions
  , solverInternal =
       SolverInternal
       { solverName = "ipopt"
       , defaultSolverOptions =
             [
--             , ("hessian_approximation", Opt "limited-memory")
--             , ("expand", Opt True)
--             , ("linear_solver", Opt "ma27")
--             , ("linear_solver", Opt "ma57")
--             , ("linear_solver", Opt "ma86")
--             , ("linear_solver", Opt "ma97")
--             , ("fixed_variable_treatment", Opt "make_constraint") -- causes segfaults?
--             , ("fixed_variable_treatment", Opt "make_parameter")
             ]
       , solverInterruptCode = 1
       , successCodes = ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
       }
  }

worhpSolver :: Solver
worhpSolver =
  Solver
  { options = []
  , runnerOptions = defaultRunnerOptions
  , solverInternal =
       SolverInternal
       { solverName = "worhp"
       , defaultSolverOptions = []
       , solverInterruptCode = 1
       , successCodes = [ "OptimalSolution"
                        , "LowPassFilterOptimal"
                        ]
       }
  }







--_sqpSolver :: Solver
--_sqpSolver =
--  Solver
--  { solverName = "sqpmethod"
--  , defaultOptions = [ ("qp_solver", Opt "nlp")
--                     , ("qp_solver_options"
--                       , Opt [ ( "nlp_solver", Opt "ipopt")
--                             , ( "nlp_solver_options"
--                               , Opt [ ("tol", Opt (1e-12 :: Double))
--                                     , ("linear_solver", Opt "ma86")
--                                     , ("ma86_order", Opt "metis")
--                                     , ("print_level", Opt (0 :: Int))
--                                     , ("print_time", Opt False)
--                                     ]
--                               )
--                             ]
--                       )
--                     ]
--  , options = []
--  , solverInterruptCode = 1
--  , successCodes = [""]
--  }
--
