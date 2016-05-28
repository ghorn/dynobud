{-# OPTIONS_GHC -Wall #-}

module LinearizeTests
       ( linearizeTests
       ) where

import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Linear ( V2(..), V3(..), V4(..) )

import Dyno.View.Vectorize ( Id(..) )
import Dyno.Linearize ( linearize', linearize )

testLinearize' :: HUnit.Assertion
testLinearize' = HUnit.assert $ do
  let fun :: Floating a => V2 a -> Id a -> (V3 a, V4 a)
      fun (V2 f0 f1) (Id p) = (V3 (f0*f0*p) 0 (42*f1), V4 p (2*p) (3*f0) (4*f1))

  funjac <- linearize' fun

  (dfdg, g, h) <- funjac (V2 10 20) (Id 7)

  let trueDfdg :: V3 (V2 Double)
      trueDfdg =
        V3
        (V2 (2*10*7) 0)
        (V2 0 0)
        (V2 0 42)

      trueG :: V3 Double
      trueG = V3 (10*10*7) 0 (42*20)

      trueH :: V4 Double
      trueH = V4 7 14 (3*10) (4*20)

  HUnit.assertEqual "dfdg" trueDfdg dfdg
  HUnit.assertEqual "g" trueG g
  HUnit.assertEqual "h" trueH h


testLinearize :: HUnit.Assertion
testLinearize = HUnit.assert $ do
  let fun :: Floating a => V2 a -> V3 a
      fun (V2 f0 f1) = V3 (f0*f0) 0 (42*f1)

  funjac <- linearize fun

  (dfdg, g) <- funjac (V2 10 20)

  let trueDfdg :: V3 (V2 Double)
      trueDfdg =
        V3
        (V2 (2*10) 0)
        (V2 0 0)
        (V2 0 42)

      trueG :: V3 Double
      trueG = V3 100 0 (42*20)

  HUnit.assertEqual "dfdg" trueDfdg dfdg
  HUnit.assertEqual "g" trueG g


linearizeTests :: Test
linearizeTests =
  testGroup "linearize tests"
  [ testCase "simple linearize test" testLinearize
  , testCase "simple linearize' test" testLinearize'
  ]
