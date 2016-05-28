{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FunctionTests
       ( functionTests
       ) where

import qualified Casadi.Function as C
import Casadi.SX ( SX, ssym )
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Dyno.View.Vectorize ( Id(..) )
import Dyno.View

simpleFun :: Floating a => a -> a
simpleFun x = 42 * x

simpleFun' :: Floating a => Id a -> Id a
simpleFun' = Id . simpleFun . unId

simpleFun'' :: J (JV Id) SX -> J (JV Id) SX
simpleFun'' = vcat . simpleFun' . vsplit

testSXFun :: Test
testSXFun = testCase "SXFun" $ HUnit.assert $ do
  f <- toSXFun "simple_sx_fun" simpleFun''
  out <- callDM f 2.2
  return $ HUnit.assertEqual "simple function" (42 * 2.2) out

testSXFunction :: Test
testSXFunction = testCase "SXFunction" $ HUnit.assert $ do
  x <- ssym "x"
  let y = simpleFun x
  f <- C.sxFunction "simple_sx_function" (V.singleton x) (V.singleton y) M.empty
  out <- C.callDM f (V.singleton 2.2)
  return $ HUnit.assertEqual "simple function" (V.fromList [42 * 2.2]) out

functionTests :: Test
functionTests =
  testGroup
  "Function tests"
  [ testSXFun
  , testSXFunction
  ]
