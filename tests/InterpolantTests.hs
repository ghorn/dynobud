{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module InterpolantTests
       ( interpolantTests
       ) where

import GHC.Generics ( Generic, Generic1 )

import Control.Compose ( (:.)(..) )
import qualified Data.Vector as V
import Linear ( V2(..), V3(..), V4(..) )

import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

--import Casadi.DM ( DM )
import Dyno.View

call1 :: Fun S S -> Double -> IO Double
call1 f x = (unId . splitJV) <$> callV f (catJV (Id x))

call2 :: Fun (J (JV V2)) S -> Double -> Double -> IO Double
call2 f x0 x1 = (unId . splitJV) <$> callV f (catJV (V2 x0 x1))

call3 :: Fun (J (JV V3)) S -> Double -> Double -> Double -> IO Double
call3 f x0 x1 x2 = (unId . splitJV) <$> callV f (catJV (V3 x0 x1 x2))

call4 :: Fun (J (JV V4)) S -> Double -> Double -> Double -> Double -> IO Double
call4 f x0 x1 x2 x3 = (unId . splitJV) <$> callV f (catJV (V4 x0 x1 x2 x3))

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


test_knots_interpolant3 :: HUnit.Assertion
test_knots_interpolant3 = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      grid2 :: V4 Double
      grid2 = V4 100 200 300 400

      values :: V2 (V3 (V4 Double))
      values =
        V2
        (V3 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12))
        (V3 (V4 13 14 15 16) (V4 17 18 19 20) (V4 21 22 23 34))

      f = interpolant3 "woo" "linear" grid0 grid1 grid2 values mempty

      xs :: V2 (V3 (V4 Double))
      xs = fmap (fmap pure) $ fmap pure grid0

      ys :: V2 (V3 (V4 Double))
      ys = pure $ fmap pure grid1

      zs :: V2 (V3 (V4 Double))
      zs = pure $ pure grid2

  O (O newValues) <- sequenceA (call3 f <$> O (O xs) <*> O (O ys) <*> O (O zs))

  return $ HUnit.assertEqual "" values newValues


data V5 a = V5 a a a a a deriving (Functor, Foldable, Traversable, Eq, Show, Generic, Generic1)
instance Applicative V5 where
  pure x = V5 x x x x x
  V5 f0 f1 f2 f3 f4 <*> V5 x0 x1 x2 x3 x4 = V5 (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4)
instance Vectorize V5

v2345 :: V2 (V3 (V4 (V5 Double)))
v2345 = r
  where
    O (O (O r)) = realToFrac <$> vcountUp

test_knots_interpolant4 :: HUnit.Assertion
test_knots_interpolant4 = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      grid2 :: V4 Double
      grid2 = V4 100 200 300 400

      grid3 :: V5 Double
      grid3 = V5 0.3 0.6 0.9 1.2 1.5

      values :: V2 (V3 (V4 (V5 Double)))
      values = v2345

      f = interpolant4 "woo" "linear" grid0 grid1 grid2 grid3 values mempty

      xs :: V2 (V3 (V4 (V5 Double)))
      xs = fmap (fmap (fmap pure)) $ fmap (fmap pure) $ fmap pure grid0

      ys :: V2 (V3 (V4 (V5 Double)))
      ys = pure $ fmap (fmap pure) $ fmap pure grid1

      zs :: V2 (V3 (V4 (V5 Double)))
      zs = pure $ pure $ fmap pure grid2

      ws :: V2 (V3 (V4 (V5 Double)))
      ws = pure $ pure $ pure grid3

  O (O (O newValues)) <- sequenceA (call4 f <$> O (O (O xs)) <*> O (O (O ys)) <*> O (O (O zs)) <*> O (O (O ws)))

  return $ HUnit.assertEqual "" values newValues


interpolantTests :: Test
interpolantTests =
  testGroup "interpolant tests"
  [ testCase "interp1 in between" test_in_between_interpolant1
  , testCase "interp2 in between" test_in_between_interpolant2
  , testCase "interp1 knots" test_knots_interpolant1
  , testCase "interp2 knots" test_knots_interpolant2
  , testCase "interp3 knots" test_knots_interpolant3
  , testCase "interp4 knots" test_knots_interpolant4
  ]
