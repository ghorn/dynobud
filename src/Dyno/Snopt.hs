{-# OPTIONS_GHC -Wall #-}

module Dyno.Snopt ( snoptSolver ) where

--import qualified Data.Vector as V

import Dyno.NlpSolver ( NlpSolverStuff(..), Opt(..) )

snoptSolver :: NlpSolverStuff
snoptSolver =
  NlpSolverStuff
  { solverName = "snopt"
  , defaultOptions = [ ("_iprint", Opt (0::Int))
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
