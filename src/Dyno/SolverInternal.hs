{-# OPTIONS_GHC -Wall #-}

module Dyno.SolverInternal
       ( SolverInternal(..)
       ) where

import Casadi.Option ( Opt(..) )

data SolverInternal =
  SolverInternal
  { solverName :: String
  , defaultOptions :: [(String,Opt)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  }