{-# OPTIONS_GHC -Wall #-}

--module Main ( main ) where

import Data.Monoid ( mempty )
import Test.QuickCheck hiding ( Result, reason )

import Test.Framework ( Test, defaultMainWithOpts, testGroup )
import Test.Framework.Runners.Options ( RunnerOptions, RunnerOptions'(..) )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Options ( TestOptions, TestOptions'(..) )
--import Test.Framework.Providers.HUnit
--import Test.HUnit hiding ( Test )

import Dyno.Nats
import Dyno.NlpSolver
import Dyno.Ipopt
--import Dyno.Snopt

import Qps.Lp

-- NOTES
-- chooseNonzero restricts the tests
-- x0 currently hardcoded to 0
-- constraint offset currently hardcoded to 0
-- lb strictly < ub (until ipopt segfault is fixed)
-- when ipopt fails but snopt succeeds, i reject the test, but this should be reported for this LP case
-- tighten the bounds from 0.4 to 1e-6 or so, 0.4 is just to get a smoking gun for Elizabeth

--quietSnoptSolver :: NlpSolverStuff SnoptSolver
--quietSnoptSolver =
--  snoptSolver { defaultOptions =
--                   defaultOptions snoptSolver ++
--                   [ ("detect_linear", Opt True)
--                   , ("_isumm",Opt (0::Int))
--                   , ("_scale_option", Opt (0::Int))
--                   ]
--              }
--
--quietNonlinearSnoptSolver :: NlpSolverStuff SnoptSolver
--quietNonlinearSnoptSolver =
--  snoptSolver { defaultOptions =
--                   defaultOptions snoptSolver ++
--                   [ ("detect_linear", Opt False)
--                   , ("_isumm",Opt (0::Int))
--                   , ("_scale_option", Opt (0::Int))
--                   , ("_optimality_tolerance", Opt (1e-9 :: Double))
--                   ]
--              }
quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver =
  ipoptSolver { options =
                   [ ("max_iter", Opt (300 :: Int))
                   , ("print_time", Opt False)
                   , ("print_level", Opt (0 :: Int))
                   ]
              }

main' :: IO ()
main' = do
  quickCheckWith (stdArgs {maxSuccess = 1000})
    (matchesGlpk quietIpoptSolver :: Lp D2 D1 -> Property)
--  quickCheckWith (stdArgs {maxSuccess = 10000})
--    (matchesGlpk quietSnoptSolver :: Lp D2 D1 -> Property)
--  quickCheckWith (stdArgs {maxSuccess = 10000})
--    (matchesGlpk quietNonlinearSnoptSolver :: Lp D2 D1 -> Property)
  return ()

main :: IO ()
main = defaultMainWithOpts tests runnerOpts

runnerOpts :: RunnerOptions
runnerOpts = emptyRunnerOpts { ropt_test_options = Just runnerOpts' }
  where
    emptyRunnerOpts = mempty :: RunnerOptions
    emptyTestOpts = mempty :: TestOptions

    runnerOpts' = emptyTestOpts {topt_maximum_generated_tests = Just 5000}


tests :: [Test]
tests = [ testGroup "run NLP solvers on LPs"
          [ --testProperty "snopt nonlinear"
--            (matchesGlpk quietNonlinearSnoptSolver :: Lp D2 D1 -> Property)
--          , testProperty "snopt linear"
--            (matchesGlpk quietSnoptSolver :: Lp D2 D1 -> Property)
            testProperty "ipopt"
            (matchesGlpk quietIpoptSolver :: Lp D2 D1 -> Property)
          ]
        ]

--toPython :: (KnownNat nx, KnownNat ng) => Params nx ng -> String
--toPython (Params x0' bx' bg' goffset' objCoeffs' jacCoeffs') =
--  init $ unlines $
--  [ "import casadi as c"
--  , "#import numpy as np"
--  , ""
--  , "x = c.SX.sym('x'," ++ show n ++ ",1)"
--  , myShowList (map xname ks) ++ " = x"
--  , ""
--  , "def nl(y): return y + 1e-14*y*y"
--  , ""
--  , "obj = sum([ " ++ myShowList (zipWith showCoeff objCoeffs ks) ++ " ])"
--  , ""
--  , "g = ["
--  ] ++ (zipWith (\coeffs goff -> "    sum([ " ++ myShowList (zipWith showCoeff coeffs ks) ++ " ]) + " ++ show goff ++ ",") jacCoeffs goffset) ++
--  [ "    ]"
--  , ""
--  , ""
--  , "def solve(solver, options):"
--  , "    f = c.SXFunction([x,c.SX.sym('y',0,0)],[obj,c.veccat(g)])"
--  , "    f.init()"
--  , "    solver = solver(f)"
--  , "    for key,value in options.iteritems():"
--  , "        solver.setOption(key,value)"
--  , "    #solver.setOption('_scale_option',0)"
--  , "    solver.setOption('_verify_level',3)"
--  , "    #solver.setOption('monitor',['setup_nlp'])"
--  , "    solver.init()"
--  , "    "
--  , "    solver.setInput(" ++ show (map fst bg) ++ ", 'lbg')"
--  , "    solver.setInput(" ++ show (map snd bg) ++ ", 'ubg')"
--  , "    solver.setInput(" ++ show (map fst bx) ++ ", 'lbx')"
--  , "    solver.setInput(" ++ show (map snd bx) ++ ", 'ubx')"
--  , "    solver.setInput(" ++ show x0 ++ ", 'x0')"
--  , "    solver.evaluate()"
--  , ""
--  , "    return (solver.getOutput('x'), solver.getOutput('f'))"
--  , ""
--  , "#(x0,f0) = solve(c.IpoptSolver, {'linear_solver':'ma57'})"
--  , "(x1,f1) = solve(c.SnoptSolver, {'_iprint':0,'_isumm':6,'detect_linear':False})"
--  , "(x2,f2) = solve(c.SnoptSolver, {'_iprint':0,'_isumm':6,'detect_linear':True})"
--  , ""
--  , "print ''"
--  , "print 'xopt:'"
--  , "#print x0"
--  , "print x1"
--  , "print x2"
--  , ""
--  , "print ''"
--  , "print 'fopt:'"
--  , "#print f0"
--  , "print f1"
--  , "print f2"
--  , ""
--  , ""
--  , "import glpk"
--  , ""
--  , "n = " ++ show n
--  , "m = " ++ show m
--  , "lp = glpk.glp_create_prob()"
--  , "glpk.glp_set_obj_dir(lp, glpk.GLP_MIN)"
--  , "glpk.glp_add_rows(lp, m)"
--  , "for (k,(lb,ub)) in " ++ show (zip [(0::Int)..] bg) ++ ":"
--  , "    glpk.glp_set_row_bnds(lp, k+1, glpk.GLP_DB, lb, ub)"
--  , "glpk.glp_add_cols(lp, n)"
--  , ""
--  , "for (k,coeff) in " ++ show (zip [(0::Int)..] (map asNumber objCoeffs)) ++ ":"
--  , "    glpk.glp_set_obj_coef(lp, k+1, coeff)"
--  , ""
--  , "for (k,(lb,ub)) in " ++ show (zip [(0::Int)..] bx) ++ ":"
--  , "    glpk.glp_set_col_bnds(lp, k+1, glpk.GLP_DB, lb, ub)"
--  , ""
--  , "ia = glpk.intArray(1+n*m)"
--  , "ja = glpk.intArray(1+n*m)"
--  , "ar = glpk.doubleArray(1+n*m)"
--  , "vals = " ++ show (map (map asNumber) jacCoeffs)
--  , "k = 1"
--  , "for row in range(0,m):"
--  , "    for col in range(0,n):"
--  , "        ia[k] = row+1"
--  , "        ja[k] = col+1"
--  , "        ar[k] = vals[row][col]"
--  , "        k += 1"
--  , ""
--  , "glpk.glp_load_matrix(lp, n*m, ia, ja, ar)"
--  , ""
--  , "glpk.glp_simplex(lp, None);"
--  , "#  /* recover and display results */"
--  , "fObj = glpk.glp_get_obj_val(lp);"
--  , "xOpt = [glpk.glp_get_col_prim(lp, k+1) for k in range(0,n)]"
--  , ""
--  , "print 'fObj:'"
--  , "print fObj"
--  , "print 'xOpt:'"
--  , "print xOpt"
--  , "#  /* housekeeping */"
--  , "glpk.glp_delete_prob(lp)"
--  , "glpk.glp_free_env()"
--  ]
--  where
--    x0 = V.toList (vectorize x0')
--    bx = V.toList (vectorize bx')
--    bg = V.toList (vectorize bg')
--    goffset = V.toList (vectorize goffset')
--    objCoeffs = V.toList (vectorize objCoeffs')
--    jacCoeffs = V.toList (vectorize (fmap (V.toList . vectorize) jacCoeffs'))
--
--    n = length x0
--    m = length bg
--    ks = take n [0..]
--    showCoeff (Linear x) k = show x ++ "*" ++ xname k
--    showCoeff (Nonlinear x) k = show x ++ "*nl(" ++ xname k ++ ")"
--    showCoeff Zero _ = "0"
--
--    fstIndex = 0 :: Int
--    xname k = "x" ++ show (k + fstIndex)
--    myShowList :: [String] -> String
--    myShowList = concat . (intersperse ", ")
