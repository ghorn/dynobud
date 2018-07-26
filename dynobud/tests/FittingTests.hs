{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module FittingTests
       ( fittingTests
       ) where

import Casadi.GenericType ( GType(..) )
import Casadi.Overloading ( ArcTan2 )
import qualified Data.Map as M
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Text.Printf ( printf )

import Dyno.Fitting ( l1Fit, l2Fit, lInfFit )
import Dyno.Nlp ( Bounds )
import Dyno.Solvers ( Solver(..), ipoptSolver )
import Dyno.TypeVecs ( Vec )
import Dyno.View.MapFun ( MapStrategy(..) )
import qualified Dyno.TypeVecs as TV
import Dyno.Vectorize

toHUnit :: IO (Maybe String) -> HUnit.Assertion
toHUnit f = HUnit.assert $ do
  r <- f
  case r of
    Just msg -> return (HUnit.assertString msg)
    Nothing -> return (HUnit.assertBool "LGTM" True)

solver :: Solver
solver =
  ipoptSolver
  { options = [("ipopt.tol", GDouble 1e-9)]
  }

-- Our data set is [1, 2, 1]
--
--  y    ^
-- 2.0 - |    *
-- 1.5 - |
-- 1.0 - | *     *
-- 0.5 - |
-- 0.0 - |
--       +------------>
--                    x
--
-- The model is f(c, x) = c
-- So the L1   minimum should be 1
--        L2   minimum should be 4/3
--        Linf minimum should be 3/2

fitModel :: Id a -> None a -> Id a
fitModel c None = c

qbounds :: Id Bounds
qbounds = Id (Nothing, Nothing)

gbounds :: None Bounds
gbounds = None

fitData :: Vec 3 (None Double, Id Double)
fitData = fmap (\x -> (None, Id x)) $ TV.mkVec' [1, 2, 1]

testFit ::
  Double
  -> (Double
      -> Solver
      -> (forall a . (Floating a, ArcTan2 a) => Id a -> None a -> Id a)
      -> (forall a . (Floating a, ArcTan2 a) => Id a -> None a)
      -> Maybe (Id Double)
      -> Id Bounds
      -> None Bounds
      -> MapStrategy
      -> M.Map String GType
      -> Vec 3 (None Double, Id Double)
      -> IO (Either String (Id Double))
     )
  -> MapStrategy
  -> HUnit.Assertion
testFit expectedValue fit mapStrategy = toHUnit $ do
  ret <- fit 0.0 solver fitModel (const None) Nothing qbounds gbounds mapStrategy mempty fitData
  return $ case ret of
    Left msg -> Just msg
    Right (Id x)
      | abs (x - expectedValue) <= 1e-9 -> Nothing
      | otherwise -> Just $ printf "expected %.4f, got %.4f, error: %.2g" expectedValue x (abs (expectedValue - x))

fittingTests :: Test
fittingTests =
  testGroup "fitting tests"
  [ testGroup (show mapStrat)
    [ testCase "L1 fit" (testFit 1 l1Fit mapStrat)
    , testCase "L2 fit" (testFit (4/3) l2Fit mapStrat)
    , testCase "L-infinity fit" (testFit (3/2) lInfFit mapStrat)
    ]
  | mapStrat <- [Unroll, Serial, OpenMP]
  ]
