{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InterpolantTests
       ( interpolantTests
       ) where

import Control.Compose ( (:.)(..) )
import qualified Data.Vector as V
import Linear ( V2(..), V3(..) )

import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

--import Casadi.DM ( DM )
import Dyno.View

call1 :: Fun S S -> Double -> IO Double
call1 f x = (unId . splitJV) <$> callV f (catJV (Id x))

call2 :: Fun (J (JV V2)) S -> Double -> Double -> IO Double
call2 f x0 x1 = (unId . splitJV) <$> callV f (catJV (V2 x0 x1))

test_knots_interpolant1 :: HUnit.Assertion
test_knots_interpolant1 = HUnit.assert $ do
  let gridAndValues = V.fromList [(1, 10), (2, 20), (3, 30)]
      f = interpolant1 "woo" "linear" gridAndValues mempty
  newValues <- mapM (call1 f) (fmap fst gridAndValues)

  return $ HUnit.assertEqual "" (fmap snd gridAndValues) newValues

test_in_between_interpolant1 :: HUnit.Assertion
test_in_between_interpolant1 = HUnit.assert $ do
  let gridAndValues = V.fromList [(1, 10), (2, 20), (3, 30)]
      f = interpolant1 "woo" "linear" gridAndValues mempty

      expected = V.fromList [(1.5, 15), (1.7, 17), (2.5, 25), (3.2, 32)]

  newValues <- mapM (call1 f) (fmap fst expected)

  return $ HUnit.assertEqual "" (fmap snd expected) newValues


test_knots_interpolant2 :: HUnit.Assertion
test_knots_interpolant2 = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      values :: V2 (V3 Double)
      values = V2 (V3 10 11 12) (V3 20 21 22)

      f = interpolant2 "woo" "linear" grid0 grid1 values mempty

      xs :: V2 (V3 Double)
      xs = fmap pure grid0

      ys :: V2 (V3 Double)
      ys = pure grid1

  O newValues <- sequenceA (call2 f <$> O xs <*> O ys)

  return $ HUnit.assertEqual "" values newValues

test_in_between_interpolant2 :: HUnit.Assertion
test_in_between_interpolant2 = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      values = V2 (V3 10 11 12) (V3 20 21 22)

      f = interpolant2 "woo" "linear" grid0 grid1 values mempty

      expected = V.fromList [((12.5, 1), 15), ((15, 4), 21.5)]

  newValues <- mapM (uncurry (call2 f)) (fmap fst expected)

  return $ HUnit.assertEqual "" (fmap snd expected) newValues


interpolantTests :: Test
interpolantTests =
  testGroup "interpolant tests"
  [ testCase "interp1 knots" test_knots_interpolant1
  , testCase "interp1 in between" test_in_between_interpolant1
  , testCase "interp2 knots" test_knots_interpolant2
  , testCase "interp2 in between" test_in_between_interpolant2
  ]
