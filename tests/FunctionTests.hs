{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FunctionTests
       ( functionTests
       ) where

import qualified Casadi.Function as C
import Casadi.SX ( SX, ssym )
import qualified Data.Map as M
import qualified Data.Vector as V
import Linear ( V2(..), V3(..) )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Dyno.View

simpleFun :: Floating a => a -> a
simpleFun x = 42 * x

simpleFun' :: Floating a => Id a -> Id a
simpleFun' = Id . simpleFun . unId

simpleFun'' :: J (JV Id) SX -> J (JV Id) SX
simpleFun'' = vcat . simpleFun' . vsplit

testSXFun :: Test
testSXFun = testCase "SXFun" $ HUnit.assert $ do
  f <- toFun "simple_sx_fun" simpleFun'' mempty
  out <- callDM f 2.2
  return $ HUnit.assertEqual "simple function" (42 * 2.2) out

testSXFunction :: Test
testSXFunction = testCase "SXFunction" $ HUnit.assert $ do
  x <- ssym "x"
  let y = simpleFun x
  f <- C.sxFunction "simple_sx_function" (V.singleton x) (V.singleton y) M.empty
  out <- C.callDM f (V.singleton 2.2)
  return $ HUnit.assertEqual "simple function" (V.fromList [42 * 2.2]) out

testFunJac :: Test
testFunJac = testCase "toFunJac" $ HUnit.assert $ do
  let f :: JacIn (JV V2) (J (JV V3)) SX -> JacOut (JV V2) (J (JV V3)) SX
      f (JacIn xj x) = JacOut (2 * xj) (3 * x)
  sxf <- toFun "f" f mempty
  fj <- toFunJac sxf
  out <- callDM fj (JacIn (vcat (V2 2 3)) (vcat (V3 4 5 6)))

  return $ HUnit.assertEqual "toFunJac" "Jac \n[[2, 00], \n [00, 2]] [4, 6] [12, 15, 18]" (show out)

testFunHess :: Test
testFunHess = testCase "toFunHess" $ HUnit.assert $ do
  let f :: JacIn (JV V2) (J (JV V3)) SX -> HessOut (J (JV V3)) SX
      f (JacIn xj x) = HessOut (0.5 * sum1 (xj * xj)) (2 * x)
  sxf <- toFun "f" f mempty
  fj <- toFunHess sxf
  print fj
  out <- callDM fj (JacIn (vcat (V2 1 2)) (vcat (V3 1 2 3)))

  return $ HUnit.assertEqual "toFunHess" "Hess \n[[1, 00], \n [00, 1]] [1, 2] 2.5 [2, 4, 6]" (show out)


functionTests :: Test
functionTests =
  testGroup
  "Function tests"
  [ testSXFun
  , testSXFunction
  , testFunJac
  , testFunHess
  ]
