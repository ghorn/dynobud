{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Test.Framework ( Test, defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck.Property

import Dyno.Ipopt ( IpoptSolver, ipoptSolver )
import Dyno.Nats
import Dyno.NlpSolver

import DummyTest ( dummyTests )
import SolveLinearOcp ( feasibleOcpIsFeasible )
import LinearOcp ( FeasibleLinearOcp )

quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver = ipoptSolver { options = [ ("max_iter", Opt (100 :: Int))
                                           , ("print_time", Opt False)
                                           , ("print_level", Opt (0 :: Int))
                                           ] }

-- a group of tests
ocpTests :: [Test]
ocpTests =
  [ testGroup "linear ocp tests"
    [ testProperty "feasible is solvable"
      (feasibleOcpIsFeasible quietIpoptSolver :: FeasibleLinearOcp D3 D2 -> Property)
--    , testProperty "infeasible is not solvable"
--      (infeasibleOcpIsInfeasible quietIpoptSolver :: InfeasibleLinearOcp n m -> Property)
    ]
  ]

-- this uses test-framework to run all the tests
main :: IO ()
main = defaultMain [ testGroup "the dummy tests" dummyTests
                   , testGroup "ocp tests" ocpTests
                   ]
