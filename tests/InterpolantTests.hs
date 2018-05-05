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

import Control.Compose ( (:.)(..), unO )
import qualified Data.Vector as V
import Linear ( V2(..), V3(..), V4(..) )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.DM ( DM )
import Casadi.MX ( MX )
import qualified Casadi.Matrix as CM

import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Dyno.View ( Vectorize, Fun, J, JV, S, Id(..), None(..)
                 , toFun, callDM, vcountUp, catJV, splitJV, unId, d2v, v2d, vcat )
import Dyno.View.Interpolant ( Interpolant(..) )
import Dyno.View.Unsafe ( M(UnsafeM) )

sndId :: (a, b) -> (a, Id b)
sndId (x, y) = (x, Id y)

test_knots_interpolant1 :: forall a .
                           Interpolant a
                        => String -> (a -> Double) -> (Double -> a)
                        -> HUnit.Assertion
test_knots_interpolant1 solver toDouble fromDouble = HUnit.assert $ do
  let gridAndValues :: V.Vector (Double, Double)
      gridAndValues = V.fromList [(1, 10), (2, 20), (3, 30)]

  f <- makeInterpolant1 "test_knots_interpolant1" solver (fmap sndId gridAndValues)
       :: IO (a -> Id a)

  let newValues :: V.Vector Double
      newValues = fmap (toDouble . unId . f . fromDouble) (fmap fst gridAndValues)

  return $ HUnit.assertEqual "" (fmap snd gridAndValues) newValues

test_in_between_interpolant1 :: forall a .
                                Interpolant a
                             => String -> (a -> Double) -> (Double -> a)
                             -> HUnit.Assertion
test_in_between_interpolant1 solver toDouble fromDouble = HUnit.assert $ do
  let gridAndValues :: V.Vector (Double, Double)
      gridAndValues = V.fromList [(1, 10), (2, 20), (3, 30)]

  f <- makeInterpolant1 "test_in_between_interpolant1" solver (fmap sndId gridAndValues)
       :: IO (a -> Id a)

  let expected :: V.Vector (Double, Double)
      expected = V.fromList [(1.5, 15), (1.7, 17), (2.5, 25), (3.2, 32)]

      newValues :: V.Vector Double
      newValues = fmap (toDouble . unId . f . fromDouble) (fmap fst expected)

  return $ HUnit.assertEqual "" (fmap snd expected) newValues


test_knots_interpolant2 :: forall a .
                           Interpolant a
                        => String -> (a -> Double) -> (Double -> a)
                        -> HUnit.Assertion
test_knots_interpolant2 solver toDouble fromDouble = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      values :: V2 (V3 Double)
      values = V2 (V3 10 11 12) (V3 20 21 22)

  f <- makeInterpolant2 "test_knots_interpolant2" solver grid0 grid1 ((unO . fmap Id . O) values)
       :: IO (V2 a -> Id a)

  let xs :: V2 (V3 Double)
      xs = fmap pure grid0

      ys :: V2 (V3 Double)
      ys = pure grid1

      newValues :: V2 (V3 Double)
      O newValues = fmap (toDouble . unId . f . fmap fromDouble) (V2 <$> O xs <*> O ys)

  return $ HUnit.assertEqual "" values newValues


test_in_between_interpolant2 :: forall a .
                                Interpolant a
                             => String -> (a -> Double) -> (Double -> a)
                             -> HUnit.Assertion
test_in_between_interpolant2 solver toDouble fromDouble = HUnit.assert $ do
  let grid0 :: V2 Double
      grid0 = V2 10 15

      grid1 :: V3 Double
      grid1 = V3 1 3 5

      values = V2 (V3 10 11 12) (V3 20 21 22)

  f <- makeInterpolant2 "test_in_between_interpolant2" solver grid0 grid1 ((unO . fmap Id . O) values)
       :: IO (V2 a -> Id a)

  let expected :: V.Vector (V2 Double, Double)
      expected = V.fromList [(V2 12.5 1, 15), (V2 15 4, 21.5)]

      newValues :: V.Vector Double
      newValues = fmap (toDouble . unId . f . fmap fromDouble) (fmap fst expected)

  return $ HUnit.assertEqual "" (fmap snd expected) newValues

test_knots_interpolant3 :: forall a .
                           Interpolant a
                        => String -> (a -> Double) -> (Double -> a)
                        -> HUnit.Assertion
test_knots_interpolant3 solver toDouble fromDouble = HUnit.assert $ do
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

  f <- makeInterpolant3 "test_knots_interpolant3" solver grid0 grid1 grid2 ((unO . unO . fmap Id . O . O) values)
       :: IO (V3 a -> Id a)

  let xs :: V2 (V3 (V4 Double))
      xs = fmap (fmap pure) $ fmap pure grid0

      ys :: V2 (V3 (V4 Double))
      ys = pure $ fmap pure grid1

      zs :: V2 (V3 (V4 Double))
      zs = pure $ pure grid2

      newValues :: V2 (V3 (V4 Double))
      O (O newValues) = fmap (toDouble . unId . f . fmap fromDouble) (V3 <$> O (O xs) <*> O (O ys) <*> O (O zs))

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

test_knots_interpolant4 :: forall a .
                           Interpolant a
                        => String -> (a -> Double) -> (Double -> a)
                        -> HUnit.Assertion
test_knots_interpolant4 solver toDouble fromDouble = HUnit.assert $ do
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

  f <- makeInterpolant4 "test_knots_interpolant4" solver grid0 grid1 grid2 grid3
       ((unO . unO . unO . fmap Id . O . O . O) values)
       :: IO (V4 a -> Id a)

  let xs :: V2 (V3 (V4 (V5 Double)))
      xs = fmap (fmap (fmap pure)) $ fmap (fmap pure) $ fmap pure grid0

      ys :: V2 (V3 (V4 (V5 Double)))
      ys = pure $ fmap (fmap pure) $ fmap pure grid1

      zs :: V2 (V3 (V4 (V5 Double)))
      zs = pure $ pure $ fmap pure grid2

      ws :: V2 (V3 (V4 (V5 Double)))
      ws = pure $ pure $ pure grid3

      O (O (O newValues)) = fmap (toDouble . unId . f . fmap fromDouble) (V4 <$> O (O (O xs)) <*> O (O (O ys)) <*> O (O (O zs)) <*> O (O (O ws)))

  return $ HUnit.assertEqual "" values newValues


interpolantTests :: Test
interpolantTests =
  testGroup "interpolant tests" $
  [ testGroup "ffi" (toTests id id)
  , testGroup "DM" (toTests dmToDouble dmFromDouble)
  , testGroup "MX" (toTests mxToDouble mxFromDouble)
  ]
  where
    dmToDouble :: S DM -> Double
    dmToDouble = unId . splitJV . d2v

    dmFromDouble :: Double -> S DM
    dmFromDouble = v2d . catJV . Id

    mxToDouble :: S MX -> Double
    mxToDouble x = unsafePerformIO $ do
      fun <- toFun "eval_mx" (const x) mempty :: IO (Fun (J (JV None)) S)
      o <- callDM fun (vcat None)
      return (dmToDouble o)

    mxFromDouble :: Double -> S MX
    mxFromDouble = vcat . Id . UnsafeM . CM.fromDouble

toTests :: Interpolant a => (a -> Double) -> (Double -> a) -> [Test]
toTests to from =
  [ testCase "interp1 in between" (test_in_between_interpolant1 solver to from)
  , testCase "interp2 in between" (test_in_between_interpolant2 solver to from)
  , testCase "interp1 knots"  (test_knots_interpolant1 solver to from)
  , testCase "innterp2 knots" (test_knots_interpolant2 solver to from)
  , testCase "interp3 knots"  (test_knots_interpolant3 solver to from)
  , testCase "interp4 knots"  (test_knots_interpolant4 solver to from)
  ]
  where
    solver = "linear"
