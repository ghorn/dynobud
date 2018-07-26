{-# OPTIONS_GHC -Wall #-}

module Dyno.SolverInternal
       ( SolverInternal(..)
       ) where

import Casadi.GenericType ( GType )

data SolverInternal =
  SolverInternal
  { solverName :: String
  , defaultSolverOptions :: [(String, GType)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  }
