{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Data.Monoid ( mempty )
import Test.QuickCheck hiding ( Result, reason )

import Test.Framework ( Test, ColorMode(..), RunnerOptions'(..), TestOptions'(..)
                      , defaultMainWithOpts, plusTestOptions, testGroup
                      )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Dyno.Ipopt ( IpoptSolver, ipoptSolver )
import Dyno.Nats
import Dyno.NlpSolver

import DummyTest ( dummyTests )
import Ocps.SolveLinearOcp ( feasibleOcpIsFeasible )
import Ocps.LinearOcp ( FeasibleLinearOcp )
import Qps.Lp ( FLp, ILp, glpkSolved, glpkUnsolved, ipoptSolved, ipoptUnsolved, matchesGlpk' )

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
      (feasibleOcpIsFeasible (quietIpoptSolver 1e-10) :: FeasibleLinearOcp D3 D2 -> Property)
--    , testProperty "infeasible is not solvable"
--      (infeasibleOcpIsInfeasible quietIpoptSolver :: InfeasibleLinearOcp n m -> Property)
    ]
  ]

lpTests :: [Test]
lpTests =
  [ testGroup "lps with glpk"
    [ testProperty "feasible is solvable"
      (glpkSolved :: FLp D2 D1 -> Property)
    , testProperty "infeasible is not solvable"
      (glpkUnsolved :: ILp D2 D1 -> Property)
    ]
  , testGroup "lps with ipopt"
    [ testProperty "feasible is solvable"
      (ipoptSolved (quietIpoptSolver 1e-10) :: FLp D2 D1 -> Property)
    , testProperty "infeasible is not solvable"
      (ipoptUnsolved (quietIpoptSolver 1e-10) :: ILp D2 D1 -> Property)
    ]
  , testGroup "compare lp solving"
    [ testProperty "feasible is solvable by glpk and ipopt"
      (matchesGlpk' (quietIpoptSolver 1e-10) :: FLp D2 D1 -> Property)
    ]
  ]

-- this uses test-framework to run all the tests
main :: IO ()
main = do
  defaultMainWithOpts
    [ testGroup "the dummy tests" dummyTests
    , testGroup "ocp tests" ocpTests
    , mempty { topt_maximum_generated_tests = Just 3000
             , topt_maximum_unsuitable_generated_tests = Just 10000
             } `plusTestOptions`
      testGroup "lps/qps" lpTests
    ]
    mempty { ropt_color_mode = Just ColorAlways }
