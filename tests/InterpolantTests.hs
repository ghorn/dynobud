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

import Dyno.View ( Vectorize, Fun, J, JV, S, callV, vcountUp, catJV, splitJV, unId )
import Dyno.View.Interpolant ( Interps(..), casadiInterps, ffiInterps )


test_knots_interpolant1 :: (V.Vector (Double, Double) -> IO (Double -> IO Double))
                        -> HUnit.Assertion
test_knots_interpolant1 mkInterp1 = HUnit.assert $ do
  let gridAndValues = V.fromList [(1, 10), (2, 20), (3, 30)]
  f <- mkInterp1 gridAndValues
  newValues <- mapM f (fmap fst gridAndValues)

  return $ HUnit.assertEqual "" (fmap snd gridAndValues) newValues

test_in_between_interpolant1 :: (V.Vector (Double, Double) -> IO (Double -> IO Double))
                             -> HUnit.Assertion
test_in_between_interpolant1 mkInterp1 = HUnit.assert $ do
  let gridAndValues = V.fromList [(1, 10), (2, 20), (3, 30)]
  f <- mkInterp1 gridAndValues

  let expected = V.fromList [(1.5, 15), (1.7, 17), (2.5, 25), (3.2, 32)]

  newValues <- mapM f (fmap fst expected)

  return $ HUnit.assertEqual "" (fmap snd expected) newValues


test_knots_interpolant2 :: (V2 Double -> V3 Double -> V2 (V3 Double)
                            -> IO (V2 Double -> IO Double))
                        -> HUnit.Assertion
test_knots_interpolant2 mkInterpolant = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      values :: V2 (V3 Double)
      values = V2 (V3 10 11 12) (V3 20 21 22)

  f <- mkInterpolant grid0 grid1 values

  let xs :: V2 (V3 Double)
      xs = fmap pure grid0

      ys :: V2 (V3 Double)
      ys = pure grid1

  O newValues <- mapM f (V2 <$> O xs <*> O ys)

  return $ HUnit.assertEqual "" values newValues


test_in_between_interpolant2 :: (V2 Double -> V3 Double -> V2 (V3 Double)
                                -> IO (V2 Double -> IO Double))
                             -> HUnit.Assertion
test_in_between_interpolant2 mkInterpolant = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      values = V2 (V3 10 11 12) (V3 20 21 22)

  f <- mkInterpolant grid0 grid1 values

  let expected :: V.Vector (V2 Double, Double)
      expected = V.fromList [(V2 12.5 1, 15), (V2 15 4, 21.5)]

  newValues <- mapM f (fmap fst expected)

  return $ HUnit.assertEqual "" (fmap snd expected) newValues

test_knots_interpolant3 :: (V2 Double -> V3 Double -> V4 Double -> V2 (V3 (V4 Double))
                            -> IO (V3 Double -> IO Double)
                           )
                        -> HUnit.Assertion
test_knots_interpolant3 mkInterpolant = HUnit.assert $ do
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

  f <- mkInterpolant grid0 grid1 grid2 values

  let xs :: V2 (V3 (V4 Double))
      xs = fmap (fmap pure) $ fmap pure grid0

      ys :: V2 (V3 (V4 Double))
      ys = pure $ fmap pure grid1

      zs :: V2 (V3 (V4 Double))
      zs = pure $ pure grid2

  O (O newValues) <- mapM f (V3 <$> O (O xs) <*> O (O ys) <*> O (O zs))

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

test_knots_interpolant4 :: (V2 Double -> V3 Double -> V4 Double -> V5 Double -> V2 (V3 (V4 (V5 Double)))
                             -> IO (V4 Double -> IO Double))
                        -> HUnit.Assertion
test_knots_interpolant4 mkInterp4 = HUnit.assert $ do
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

  f <- mkInterp4 grid0 grid1 grid2 grid3 values

  let xs :: V2 (V3 (V4 (V5 Double)))
      xs = fmap (fmap (fmap pure)) $ fmap (fmap pure) $ fmap pure grid0

      ys :: V2 (V3 (V4 (V5 Double)))
      ys = pure $ fmap (fmap pure) $ fmap pure grid1

      zs :: V2 (V3 (V4 (V5 Double)))
      zs = pure $ pure $ fmap pure grid2

      ws :: V2 (V3 (V4 (V5 Double)))
      ws = pure $ pure $ pure grid3

  O (O (O newValues)) <- mapM f (V4 <$> O (O (O xs)) <*> O (O (O ys)) <*> O (O (O zs)) <*> O (O (O ws)))

  return $ HUnit.assertEqual "" values newValues


callF :: Vectorize f => Fun (J (JV f)) S -> f Double -> IO Double
callF f x = (unId . splitJV) <$> callV f (catJV x)


interpolantTests :: Test
interpolantTests =
  testGroup "interpolant tests" $
  [ testGroup name
    [ testCase "interp1 in between" (test_in_between_interpolant1 mkInterp1)
    , testCase "interp2 in between" (test_in_between_interpolant2 mkInterp2)
    , testCase "interp1 knots" (test_knots_interpolant1 mkInterp1)
    , testCase "interp2 knots" (test_knots_interpolant2 mkInterp2)
    , testCase "interp3 knots" (test_knots_interpolant3 mkInterp3)
    , testCase "interp4 knots" (test_knots_interpolant4 mkInterp4)
    ]
  | (name, Interps mkInterp1 mkInterp2 mkInterp3 mkInterp4) <-
      [ ("casadi", casadiInterps callF)
      , ("ffi", ffiInterps)
      ]
  ]
