{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language MultiWayIf #-}

module Main ( main ) where

import Control.Monad ( unless )
import Data.Monoid ( mempty )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck hiding ( Result, reason )
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic

import Test.Framework ( Test, defaultMainWithOpts, testGroup )
import Test.Framework.Runners.Options ( RunnerOptions, RunnerOptions'(..) )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Options ( TestOptions, TestOptions'(..) )
--import Test.Framework.Providers.HUnit
--import Test.HUnit hiding ( Test )

import qualified Numeric.LinearProgramming as GLPK
import qualified Data.Vector as V
import Data.List ( intersperse )
import Data.Maybe

import qualified Data.Foldable as F

import Hascm.Vectorize
import Hascm.Nats
import Hascm.TypeVecs -- ( Vec(..), mkVec', tvlength )
import Hascm.Nlp
import Hascm.NlpSolver
import Hascm.Ipopt
import Hascm.Snopt
import Hascm.Casadi.SXElement

-- NOTES
-- chooseNonzero restricts the tests
-- x0 currently hardcoded to 0
-- constraint offset currently hardcoded to 0
-- lb strictly < ub (until ipopt segfault is fixed)
-- when ipopt fails but snopt succeeds, i reject the test, but this should be reported for this LP case
-- tighten the bounds from 0.4 to 1e-6 or so, 0.4 is just to get a smoking gun for Elizabeth

quietSnoptSolver :: NlpSolverStuff SnoptSolver
quietSnoptSolver =
  snoptSolver { defaultOptions =
                   defaultOptions snoptSolver ++
                   [ ("detect_linear", Opt True)
                   , ("_isumm",Opt (0::Int))
                   , ("_scale_option", Opt (0::Int))
                   ]
              }

quietNonlinearSnoptSolver :: NlpSolverStuff SnoptSolver
quietNonlinearSnoptSolver =
  snoptSolver { defaultOptions =
                   defaultOptions snoptSolver ++
                   [ ("detect_linear", Opt False)
                   , ("_isumm",Opt (0::Int))
                   , ("_scale_option", Opt (0::Int))
                   , ("_optimality_tolerance", Opt (1e-9 :: Double))
                   ]
              }
quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver =
  ipoptSolver { defaultOptions =
                   defaultOptions ipoptSolver ++
                   [ ("print_time", Opt False)
                   , ("print_level", Opt (0 :: Int))
                   ]
              }

main' :: IO ()
main' = do
--  quickCheckWith (stdArgs {maxSuccess = 1000})
--    (matchesGlpk quietIpoptSolver :: NlpTest D2 D1 -> Property)
--  quickCheckWith (stdArgs {maxSuccess = 10000})
--    (matchesGlpk quietSnoptSolver :: NlpTest D2 D1 -> Property)
  quickCheckWith (stdArgs {maxSuccess = 10000})
    (matchesGlpk quietNonlinearSnoptSolver :: NlpTest D2 D1 -> Property)

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
          [ testProperty "snopt nonlinear"
            (matchesGlpk quietNonlinearSnoptSolver :: NlpTest D2 D1 -> Property)
--          , testProperty "snopt linear"
--            (matchesGlpk quietSnoptSolver :: NlpTest D2 D1 -> Property)
          --, testProperty "ipopt"
          --  (matchesGlpk quietIpoptSolver :: NlpTest D2 D1 -> Property)
          ]
        ]

instance (Arbitrary a, Dim n) => Arbitrary (Vec n a) where
  arbitrary = do
    let n = tvlength (undefined :: Vec n a)
    contents <- vector n
    return $ devectorize (V.fromList contents)

data Coef a = Linear a
            | Nonlinear a
            | Zero
            deriving (Functor, Show, Eq)
asNumber :: Num a => Coef a -> a
asNumber Zero = 0
asNumber (Linear x) = x
asNumber (Nonlinear x) = x

chooseNonzero :: (Int,Int) -> Gen Int
chooseNonzero range = do
  k <- choose range
  if k==0 then chooseNonzero range else return k

instance (Num a, Arbitrary a) => Arbitrary (Coef a) where
  arbitrary =
    oneof [ do x <- chooseNonzero (-9,9)
               return (Nonlinear (fromIntegral x))
          , do x <- chooseNonzero (-9,9) --- THIS RESTRICTS OUR TESTS
               return (Linear (fromIntegral x))
          , return Zero
          ]
instance Arbitrary SXElement where
  arbitrary = do
    x <- arbitrary :: Gen Double
    return (realToFrac x)

absnl :: Num a => Coef a -> Coef a
absnl (Nonlinear x) = Nonlinear (abs x)
absnl x = x

runSum :: (F.Foldable f, Fractional a) => f (a, Coef a) -> a
runSum = runSum' 0 . F.toList

runSum' :: Fractional a => a -> [(a, Coef a)] -> a
runSum' acc [] = acc
runSum' acc ((_,Zero):xs) = runSum' acc xs
runSum' acc ((x,Linear c):xs) = runSum' (acc + c*x) xs
runSum' acc ((x,Nonlinear c):xs) = runSum' (acc + c*(x + 1e-140*x*x)) xs

newtype Bnd = Bnd { unBound :: (Double,Double) }
instance Arbitrary Bnd where
  arbitrary = do
    let range = (-9,9::Int)
    k0 <- fmap fromIntegral (choose range)
    k1 <- fmap fromIntegral (choose range)
    return $ if k0 == k1
             then Bnd (k0, k1 + 1)
             else if k0 <= k1
                  then Bnd (k0,k1)
                  else Bnd (k1,k0)

data Params nx ng = Params { px0 :: Vec nx Double
                           , pbx :: Vec nx (Double, Double)
                           , pbg :: Vec ng (Double, Double)
                           , pgoffset:: Vec ng Double
                           , pobjCoeffs :: Vec nx (Coef Double)
                           , pjacCoeffs :: Vec ng (Vec nx (Coef Double))
                           } --deriving Show
instance (Dim nx, Dim ng) => Show (Params nx ng) where
  show = prettyPrint

data Valid = Valid { vfErr :: Double
                   , vxErr :: Double
                   , vgErr :: Double
                   } deriving Show
validSol :: forall nx ng . (Dim nx, Dim ng) =>
            Params nx ng -> Vec nx Double -> Double -> Valid
validSol params xopt fopt =
  Valid { vfErr = abs (fopt - fopt')
        , vxErr = maximum (F.toList satisfiesBnds)
        , vgErr = maximum (F.toList satisfiesConstraints)
        }
  where
    fopt' = sum (F.toList (vzipWith (*) (fmap asNumber (pobjCoeffs params)) xopt))

    satisfiesBnds :: Vec nx Double
    satisfiesBnds = vzipWith satisfiesBnd (pbx params) xopt

    satisfiesBnd :: (Double,Double) -> Double -> Double
    satisfiesBnd (lb,ub) x = max (max (lb - x) 0) (max (x - ub) 0)

    satisfiesConstraints :: Vec ng Double
    satisfiesConstraints = vzipWith3 satisfiesConstraint
                           (pbg params) (pjacCoeffs params) (pgoffset params)

    satisfiesConstraint :: (Double, Double) -> Vec nx (Coef Double) -> Double -> Double
    satisfiesConstraint (lbg,ubg) coeffs goffset =
      max (max (lbg - f) 0) (max (f - ubg) 0)
      where
        f = sum (F.toList (vzipWith (*) xopt (fmap asNumber coeffs))) + goffset


solveWithGlpk :: (Dim nx, Dim ng) => Params nx ng -> GLPK.Solution
solveWithGlpk params = GLPK.simplex prob constraints bounds
  where
    -- unpack params
    jacCoeffs :: [[Coef Double]]
    jacCoeffs = F.toList (fmap F.toList (pjacCoeffs params))

    bgs :: [(Double,Double)]
    bgs = F.toList (pbg params)

    bxs :: [(Double,Double)]
    bxs = F.toList (pbx params)

    goffsets :: [Double]
    goffsets = F.toList (pgoffset params)

    objCoeffs :: [Double]
    objCoeffs = map asNumber (F.toList (pobjCoeffs params))

    -- set up problem
    prob = GLPK.Minimize objCoeffs

    constraints = GLPK.Dense $ zipWith3 f jacCoeffs bgs goffsets
      where
        f jcs (lb,ub) (goffset) = (map asNumber jcs) GLPK.:&: (lb-goffset,ub-goffset)

    bounds = zipWith (GLPK.:&:) [0..] bxs


instance (Dim nx, Dim ng) => Arbitrary (Params nx ng) where
  arbitrary = do
    --x0 <- arbitrary :: Gen (Vec nx Double)
    let x0 = fill 0

    bx <- fmap (fmap unBound) arbitrary :: Gen (Vec nx (Double,Double))

    bg <- fmap (fmap unBound) arbitrary :: Gen (Vec ng (Double,Double))

    goffset <- fmap (devectorize . V.fromList . map fromIntegral) $
               vectorOf (tvlength bg) (choose (0,0::Int))
--               vectorOf (tvlength bg) (choose (-2,2::Int))

    objCoeffs'' <- arbitrary :: Gen (Vec nx (Coef Double))
    let objCoeffs' = fmap absnl objCoeffs''
        makeNonzero objCoeffs0
          | V.any (/= Zero) objCoeffs0 = return (devectorize objCoeffs0)
          | otherwise = do
            k <- choose (0, V.length objCoeffs0 - 1)
            newCoeff <- arbitrary
            makeNonzero (objCoeffs0 V.// [(k,newCoeff)])
    objCoeffs <- makeNonzero (vectorize objCoeffs')

    jacCoeffs <- arbitrary :: Gen (Vec ng (Vec nx (Coef Double)))

    return $ Params { px0 = x0
                    , pbx = bx
                    , pbg = bg
                    , pgoffset = goffset
                    , pobjCoeffs = objCoeffs
                    , pjacCoeffs = jacCoeffs
                    }
  shrink params
    | V.all (==0) (vectorize (pgoffset params)) = []
    | otherwise = [params { pgoffset = fill 0 }]

justs :: (a, a) -> (Maybe a, Maybe a)
justs (x,y) = (Just x, Just y)

data NlpTest nx ng = NlpTest (Params nx ng) (Nlp (Vec nx) None (Vec ng))
instance (Dim nx, Dim ng) => Show (NlpTest nx ng) where
  show (NlpTest parms _) = show parms

instance (Dim nx, Dim ng) => Arbitrary (NlpTest nx ng) where
  arbitrary = do
    parms <- arbitrary
    return (testOfParms parms)
  shrink (NlpTest parms _) = map testOfParms (shrink parms)

testOfParms :: forall nx ng . (Dim nx, Dim ng) => Params nx ng -> NlpTest nx ng
testOfParms parms@(Params x0 bx bg goffset objCoeffs jacCoeffs) =
  NlpTest parms $
    Nlp { nlpFG = fg
        , nlpBX = fmap justs bx
        , nlpBG = fmap justs bg
        , nlpX0 = x0
        , nlpP = None
        }
  where
    fg :: forall a . Floating a => NlpInputs (Vec nx) None a -> NlpFun (Vec ng) a
    fg (NlpInputs xs _) = NlpFun f g
      where
        f = runSum (tvzip xs (fmap (fmap realToFrac) objCoeffs))
        g' = fmap (runSum . tvzip xs) (fmap (fmap (fmap realToFrac)) jacCoeffs)
        g = tvzipWith (+) g' (fmap realToFrac goffset)

matchesGlpk :: (Dim nx, Dim ng, NLPSolverClass nlp)
               => NlpSolverStuff nlp -> NlpTest nx ng -> Property
matchesGlpk solver (NlpTest params nlp) = monadicIO $ do
  (fopt,xopt) <- case solveWithGlpk params of
    GLPK.Unbounded -> stop (rejected {reason = "unbounded"})
    GLPK.NoFeasible -> stop (rejected {reason = "NoFeasible"})
    GLPK.Infeasible _ -> stop (rejected {reason = "Infeasible"})
    GLPK.Undefined -> stop (failed {reason = "glpk result Undefined"})
    GLPK.Feasible _ -> stop (failed {reason = "glpk result Feasible, should be optimal"})
    GLPK.Optimal opt -> return opt
  let v@(Valid fe xe ge) = validSol params (devectorize (V.fromList xopt)) fopt
  unless (and [fe <= 1e-6, xe <= 1e-10, ge <= 1e-10]) $
    stop (failed {reason = "glpk gave invalid solution: " ++ show v})

  (ret,nlpOut) <- run $ solveNlp solver nlp Nothing
  let xerr = maximum (map abs (zipWith (-) xopt (F.toList (xOpt nlpOut))))
      ferr = abs (fopt - fOpt nlpOut)
      v'@(Valid fe' xe' ge') = validSol params (xOpt nlpOut) (fOpt nlpOut)

      summary = unlines
               [ "design vars"
               , "  glpk: " ++ show xopt
               , "  nlp:  " ++ show (F.toList (xOpt nlpOut))
               , "objective"
               , "  glpk: " ++ show fopt
               , "  nlp:  " ++ show (fOpt nlpOut)
               , ""
               ]
  case ret of
    Left "3" -> stop $ rejected {reason = "nlp solver got code 3"}
    Left code -> do
      --run $ writeFile "counterexample.py" (toPython params)
      stop $ failed {reason = "====== nlp solver failed with code " ++ show code ++ " =====\n"++summary}
    Right _ ->
      if | or [fe' > 1e-5, xe' > 1e-6, ge' > 1e-4] ->
            stop $ failed {reason = "returned invalid solution: " ++ show v'}
         | xerr <= 0.4 && ferr <= 0.4 -> stop $ succeeded {reason = "solutions match"}
         | ferr <= 1e-6 -> stop $ rejected {reason =
             "two valid solutions match objective, don't match decision vars"}
         | otherwise -> do
             run $ writeFile "counterexample.py" (toPython params)
             stop $ failed { reason = "======== solution doesn't match glpk! ========\n" ++ summary }

prettyPrint :: (Dim nx, Dim ng) => Params nx ng -> String
prettyPrint (Params x0' bx' bg' goffset' objCoeffs' jacCoeffs') =
  init $ unlines $
  [ "minimize:"
  , "    " ++ myShowList (zipWith showCoeff objCoeffs ks)
  , "subject to:"
  ] ++ (zipWith3 (\coeffs goff (lb,ub) -> "    " ++ show lb ++ " <= " ++ myShowList (zipWith showCoeff coeffs ks) ++ " + " ++ show goff  ++ " <= " ++ show ub) jacCoeffs goffset bg) ++
  ["subject to (bounds):"] ++
  zipWith (\k (lb,ub) -> "    " ++ show lb ++ " <= " ++ xname k ++ " <= " ++ show ub) ks bx
  where
    --maybeOffset
    x0 = V.toList (vectorize x0')
    bx = V.toList (vectorize bx')
    bg = V.toList (vectorize bg')
    goffset = V.toList (vectorize goffset')
    objCoeffs = V.toList (vectorize objCoeffs')
    jacCoeffs = V.toList (vectorize (fmap (V.toList . vectorize) jacCoeffs'))

    n = length x0
    --m = length bg
    ks = take n [1..]
    showCoeff (Linear x) k = Just (show x ++ "*" ++ xname k)
    showCoeff (Nonlinear x) k = Just (show x ++ "*{" ++ xname k ++ "}")
    showCoeff Zero _ = Nothing

    fstIndex = 0 :: Int
    xname k = "x" ++ show (k + fstIndex)
    myShowList :: [Maybe String] -> String
    myShowList = concat . (intersperse " + ") . catMaybes


toPython :: (Dim nx, Dim ng) => Params nx ng -> String
toPython (Params x0' bx' bg' goffset' objCoeffs' jacCoeffs') =
  init $ unlines $
  [ "import casadi as c"
  , "#import numpy as np"
  , ""
  , "x = c.SX.sym('x'," ++ show n ++ ",1)"
  , myShowList (map xname ks) ++ " = x"
  , ""
  , "def nl(y): return y + 1e-14*y*y"
  , ""
  , "obj = sum([ " ++ myShowList (zipWith showCoeff objCoeffs ks) ++ " ])"
  , ""
  , "g = ["
  ] ++ (zipWith (\coeffs goff -> "    sum([ " ++ myShowList (zipWith showCoeff coeffs ks) ++ " ]) + " ++ show goff ++ ",") jacCoeffs goffset) ++
  [ "    ]"
  , ""
  , ""
  , "def solve(solver, options):"
  , "    f = c.SXFunction([x,c.SX.sym('y',0,0)],[obj,c.veccat(g)])"
  , "    f.init()"
  , "    solver = solver(f)"
  , "    for key,value in options.iteritems():"
  , "        solver.setOption(key,value)"
  , "    #solver.setOption('_scale_option',0)"
  , "    solver.setOption('_verify_level',3)"
  , "    #solver.setOption('monitor',['setup_nlp'])"
  , "    solver.init()"
  , "    "
  , "    solver.setInput(" ++ show (map fst bg) ++ ", 'lbg')"
  , "    solver.setInput(" ++ show (map snd bg) ++ ", 'ubg')"
  , "    solver.setInput(" ++ show (map fst bx) ++ ", 'lbx')"
  , "    solver.setInput(" ++ show (map snd bx) ++ ", 'ubx')"
  , "    solver.setInput(" ++ show x0 ++ ", 'x0')"
  , "    solver.evaluate()"
  , ""
  , "    return (solver.getOutput('x'), solver.getOutput('f'))"
  , ""
  , "#(x0,f0) = solve(c.IpoptSolver, {'linear_solver':'ma57'})"
  , "(x1,f1) = solve(c.SnoptSolver, {'_iprint':0,'_isumm':6,'detect_linear':False})"
  , "(x2,f2) = solve(c.SnoptSolver, {'_iprint':0,'_isumm':6,'detect_linear':True})"
  , ""
  , "print ''"
  , "print 'xopt:'"
  , "#print x0"
  , "print x1"
  , "print x2"
  , ""
  , "print ''"
  , "print 'fopt:'"
  , "#print f0"
  , "print f1"
  , "print f2"
  , ""
  , ""
  , "import glpk"
  , ""
  , "n = " ++ show n
  , "m = " ++ show m
  , "lp = glpk.glp_create_prob()"
  , "glpk.glp_set_obj_dir(lp, glpk.GLP_MIN)"
  , "glpk.glp_add_rows(lp, m)"
  , "for (k,(lb,ub)) in " ++ show (zip [(0::Int)..] bg) ++ ":"
  , "    glpk.glp_set_row_bnds(lp, k+1, glpk.GLP_DB, lb, ub)"
  , "glpk.glp_add_cols(lp, n)"
  , ""
  , "for (k,coeff) in " ++ show (zip [(0::Int)..] (map asNumber objCoeffs)) ++ ":"
  , "    glpk.glp_set_obj_coef(lp, k+1, coeff)"
  , ""
  , "for (k,(lb,ub)) in " ++ show (zip [(0::Int)..] bx) ++ ":"
  , "    glpk.glp_set_col_bnds(lp, k+1, glpk.GLP_DB, lb, ub)"
  , ""
  , "ia = glpk.intArray(1+n*m)"
  , "ja = glpk.intArray(1+n*m)"
  , "ar = glpk.doubleArray(1+n*m)"
  , "vals = " ++ show (map (map asNumber) jacCoeffs)
  , "k = 1"
  , "for row in range(0,m):"
  , "    for col in range(0,n):"
  , "        ia[k] = row+1"
  , "        ja[k] = col+1"
  , "        ar[k] = vals[row][col]"
  , "        k += 1"
  , ""
  , "glpk.glp_load_matrix(lp, n*m, ia, ja, ar)"
  , ""
  , "glpk.glp_simplex(lp, None);"
  , "#  /* recover and display results */"
  , "fObj = glpk.glp_get_obj_val(lp);"
  , "xOpt = [glpk.glp_get_col_prim(lp, k+1) for k in range(0,n)]"
  , ""
  , "print 'fObj:'"
  , "print fObj"
  , "print 'xOpt:'"
  , "print xOpt"
  , "#  /* housekeeping */"
  , "glpk.glp_delete_prob(lp)"
  , "glpk.glp_free_env()"
  ]
  where
    x0 = V.toList (vectorize x0')
    bx = V.toList (vectorize bx')
    bg = V.toList (vectorize bg')
    goffset = V.toList (vectorize goffset')
    objCoeffs = V.toList (vectorize objCoeffs')
    jacCoeffs = V.toList (vectorize (fmap (V.toList . vectorize) jacCoeffs'))

    n = length x0
    m = length bg
    ks = take n [0..]
    showCoeff (Linear x) k = show x ++ "*" ++ xname k
    showCoeff (Nonlinear x) k = show x ++ "*nl(" ++ xname k ++ ")"
    showCoeff Zero _ = "0"

    fstIndex = 0 :: Int
    xname k = "x" ++ show (k + fstIndex)
    myShowList :: [String] -> String
    myShowList = concat . (intersperse ", ")
