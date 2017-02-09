{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearizeTests
       ( linearizeTests
       ) where

import Data.Proxy ( Proxy(..) )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Linear ( V2(..), V3(..), V4(..) )

import Casadi.MX ( MX )
import Casadi.SX ( SX )
import qualified Casadi.Matrix as CM
import Dyno.View ( S, Id(..) )
import Dyno.Linearize ( linearize', linearize )

testLinearize' :: forall a . CM.SMatrix a => Proxy a -> HUnit.Assertion
testLinearize' _ = HUnit.assert $ do
  let fun :: V2 (S a) -> Id (S a) -> (V3 (S a), V4 (S a))
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


testLinearize :: forall a . CM.SMatrix a => Proxy a -> HUnit.Assertion
testLinearize _ = HUnit.assert $ do
  let fun :: V2 (S a) -> V3 (S a)
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
  [ linearizeTests' "SX" (Proxy :: Proxy SX)
  , linearizeTests' "MX" (Proxy :: Proxy MX)
  ]


linearizeTests' :: CM.SMatrix a => String -> Proxy a -> Test
linearizeTests' name p =
  testGroup name
  [ testCase ("simple linearize test " ++ name) (testLinearize p)
  , testCase ("simple linearize' test " ++ name) (testLinearize' p)
  ]
