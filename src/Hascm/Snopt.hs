{-# OPTIONS_GHC -Wall #-}

module Hascm.Snopt ( snoptSolver ) where

import qualified Casadi.Wrappers.Classes.SnoptSolver as SS

import Hascm.NlpSolver ( NlpSolverStuff(..), Opt(..) )

snoptSolver :: NlpSolverStuff SS.SnoptSolver
snoptSolver =
  NlpSolverStuff
  { nlpConstructor = SS.snoptSolver''
  , defaultOptions = [ --("major iterations", Opt (2000 :: Int))
                     --, ("_verify_level", Opt (2 :: Int))
--                     , ("_optimality_tolerance", Opt (1e-7 :: Double))
--                     , ("_feasibility_tol", Opt (1e-7 :: Double))
                     --, ("detect_linear", Opt True)
                     --, ("monitor", V.fromList ["setup_nlp"])
                     ]
  , solverInterruptCode = -2
  }

