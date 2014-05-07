{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Test.Framework ( Test, defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck.Property

import Dyno.Ipopt ( IpoptSolver, ipoptSolver )
import Dyno.Nats
import Dyno.NlpSolver

import DummyTest ( dummyTests )
import Ocps.SolveLinearOcp ( feasibleOcpIsFeasible )
import Ocps.LinearOcp ( FeasibleLinearOcp )
import Qps.Lp ( Lp, matchesGlpk )

quietIpoptSolver :: Double -> NlpSolverStuff IpoptSolver
quietIpoptSolver tol =
  ipoptSolver { options = [ ("max_iter", Opt (100 :: Int))
                          , ("print_time", Opt False)
                          , ("print_level", Opt (0 :: Int))
                          , ("tol", Opt tol)
                          ] }

-- a group of tests
ocpTests :: [Test]
ocpTests =
  [ testGroup "linear ocp tests"
    [ testProperty "feasible is solvable"
      (feasibleOcpIsFeasible (quietIpoptSolver 1e-9) :: FeasibleLinearOcp D3 D2 -> Property)
--    , testProperty "infeasible is not solvable"
--      (infeasibleOcpIsInfeasible quietIpoptSolver :: InfeasibleLinearOcp n m -> Property)
    ]
  ]

lpTests :: [Test]
lpTests =
  [ testGroup "lps"
    [ testProperty "ipopt solves lps"
      (matchesGlpk (quietIpoptSolver 1e-10) :: Lp D3 D2 -> Property)
    ]
  ]

-- this uses test-framework to run all the tests
main :: IO ()
main = defaultMain [ testGroup "the dummy tests" dummyTests
                   , testGroup "ocp tests" ocpTests
                   , testGroup "lps/qps" lpTests
                   ]
