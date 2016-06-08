{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module ConditionalTests
       ( conditionalTests
       ) where

import Data.Proxy ( Proxy(..) )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Casadi.DM ( DM )
--import Casadi.MX ( MX )
import Casadi.SX ( SX )

import Dyno.View.Conditional
import Dyno.View.View ( S )

data SymOrds where
  SymOrds :: (SymOrd a, Eq a, Fractional a, Show a) => String -> Proxy a -> SymOrds

conditionalTests :: Test
conditionalTests =
  testGroup "conditional tests"
  [ symordSwitchTests
  , logicTests
  ]

symordSwitchTests :: Test
symordSwitchTests =
  testGroup "SymOrd switches"
  [ testGroup whichSymord
    [ toGroup p "<=" leq
      [ (1.0, 2.0, True)
      , (1.5, 2.0, True)
      , (2.0, 2.0, True)
      , (2.5, 2.0, False)
      , (3.0, 2.0, False)
      ]
    , toGroup p "<" lt
      [ (1.0, 2.0, True)
      , (1.5, 2.0, True)
      , (2.0, 2.0, False)
      , (2.5, 2.0, False)
      , (3.0, 2.0, False)
      ]
    , toGroup p ">=" geq
      [ (1.0, 2.0, False)
      , (1.5, 2.0, False)
      , (2.0, 2.0, True)
      , (2.5, 2.0, True)
      , (3.0, 2.0, True)
      ]
    , toGroup p ">" gt
      [ (1.0, 2.0, False)
      , (1.5, 2.0, False)
      , (2.0, 2.0, False)
      , (2.5, 2.0, True)
      , (3.0, 2.0, True)
      ]
    , toGroup p "==" eq
      [ (1.0, 2.0, False)
      , (1.5, 2.0, False)
      , (2.0, 2.0, True)
      , (2.5, 2.0, False)
      , (3.0, 2.0, False)
      ]
    ]
  | SymOrds whichSymord p <-
      [ SymOrds "SX" (Proxy :: Proxy (S SX))
      , SymOrds "DM" (Proxy :: Proxy (S DM))
--      , SymOrds "MX" (Proxy :: Proxy (S MX))
      , SymOrds "Double" (Proxy :: Proxy Double)
      , SymOrds "Float" (Proxy :: Proxy Float)
      ]
  ]
  where
    toGroup ::
      forall a
      . (SymOrd a, Eq a, Fractional a, Show a)
      => Proxy a -> String -> (a -> a -> Switch Bool a) -> [(Double, Double, Bool)] -> Test
    toGroup _ opName op tests =
      testGroup ("(" ++ opName ++ ")")
      [ testCase (show x ++ " " ++ opName ++ " " ++ show y ++ " == " ++ show expectedBool) $
        test_symord_switch (Proxy :: Proxy a) op (realToFrac x) (realToFrac y) expectedBool
      | (x, y, expectedBool) <- tests
      ]

test_symord_switch ::
  forall a
  . (SymOrd a, Eq a, Fractional a, Show a)
  => Proxy a -> (a -> a -> Switch Bool a) -> a -> a -> Bool -> HUnit.Assertion
test_symord_switch _ op x y expectedBool = HUnit.assertEqual "" expectedSwitch actualSwitch
  where
    actualSwitch :: Switch Bool a
    actualSwitch = op x y

    expectedSwitch :: Switch Bool a
    expectedSwitch = toSwitch expectedBool


data Conditionals where
  Conditionals :: (Conditional a, Fractional a, Eq a, Show a) => String -> Proxy a -> Conditionals

logicTests :: Test
logicTests =
  testGroup "logic switches"
  [ testGroup whichConditional
    [ toGroup p "&&" and' (&&)
    ]
  | Conditionals whichConditional p <-
      [ Conditionals "SX" (Proxy :: Proxy (S SX))
      , Conditionals "DM" (Proxy :: Proxy (S DM))
--      , Conditionals "MX" (Proxy :: Proxy (S MX))
      , Conditionals "Double" (Proxy :: Proxy Double)
      , Conditionals "Float" (Proxy :: Proxy Float)
      ]
  ]
  where
    toGroup ::
      forall a
      . (Fractional a, Eq a, Show a)
      => Proxy a -> String
      -> (Switch Bool a -> Switch Bool a -> Switch Bool a)
      -> (Bool -> Bool -> Bool)
      -> Test
    toGroup _ opName conditionalOp haskellOp =
      testGroup ("(" ++ opName ++ ")")
      [ testCase (show x ++ " " ++ opName ++ " " ++ show y ++ " == " ++ show expectedBool) $
        HUnit.assertEqual "" expectedSwitch actualSwitch
      | x <- [False, True]
      , y <- [False, True]
      , let expectedBool = haskellOp x y
            expectedSwitch = toSwitch expectedBool
            actualSwitch = conditionalOp (toSwitch x) (toSwitch y)
      ]
