{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module FunctionTests
       ( functionTests
       ) where

import           Casadi.DM ( DM )
import           Casadi.MX ( MX )
import           Casadi.SX ( SX )
import qualified Casadi.Function as C
import qualified Casadi.Matrix as CM
import qualified Data.Map as M
import           Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import           Linear ( V2(..), V3(..) )
import qualified Test.HUnit.Base as HUnit
import           Test.Framework ( Test, testGroup )
import           Test.Framework.Providers.HUnit ( testCase )

import           Dyno.View


testFun :: forall a . CM.SMatrix a => String -> Proxy a -> Test
testFun name _ = testCase ("simple fun " ++ name) $ HUnit.assert $ do
  f <- toFun "simple_fun" ((42 *) :: S a -> S a) mempty
  out <- callDM f 2.2
  return $ HUnit.assertEqual "" (42 * 2.2) out


testFunction :: forall a . CM.SMatrix a => String -> Proxy a -> Test
testFunction name _ = testCase ("simple function " ++ name) $ HUnit.assert $ do
  x <- CM.sym "x" 1 1 :: IO a
  let y = 42 * x
  f <- CM.toFunction "simple_sx_function" (V.singleton x) (V.singleton y) M.empty
  out <- C.callDM f (V.singleton 2.2)
  return $ HUnit.assertEqual "" (V.fromList [42 * 2.2] :: V.Vector DM) out


testJacobian :: forall a . CM.SMatrix a => String -> Proxy a -> Test
testJacobian name _ = testCase ("jacobian " ++ name) $ HUnit.assert $ do
  let f :: (J (JV V2) :*: J (JV V3)) a
        -> (M (JTuple (JV V2) (JV V2)) (JV V2) :*: J (JV V2) :*: J (JV V3)) a
      f (xj :*: x) = (jacobian obj xj) :*: (2 * xj) :*: (3 * x)
        where
          obj :: J (JTuple (JV V2) (JV V2)) a
          obj = cat $ JTuple (2 * xj) (3*xj)

  fun <- toFun "f" f mempty
  o0 :*: o1 :*: o2 <- callDM fun (vcat (V2 2 3) :*: vcat (V3 4 5 6))

  return $ HUnit.assertEqual "" "(\n[[2, 00], \n [00, 2], \n [3, 00], \n [00, 3]],[4, 6],[12, 15, 18])" (show (o0, o1, o2))


testHessian :: forall a . CM.SMatrix a => String -> Proxy a -> Test
testHessian name _ = testCase ("hessian " ++ name) $ HUnit.assert $ do
  let f :: (J (JV V2) :*: J (JV V3)) a
        -> (M (JV V2) (JV V2) :*: J (JV V2) :*: S :*: J (JV V3)) a
      f (xj :*: x) = hess :*: grad :*: obj :*: (2 * x)
        where
          (hess, grad) = hessian obj xj
          obj = 0.5 * sum1 (xj * xj)

  fun <- toFun "f" f mempty
  print fun
  o0 :*: o1 :*: o2 :*: o3 <- callDM fun (vcat (V2 1 2) :*: vcat (V3 1 2 3))

  return $ HUnit.assertEqual "" "(\n[[1, 00], \n [00, 1]],[1, 2],2.5,[2, 4, 6])" (show (o0, o1, o2, o3))


testGradient :: forall a . CM.SMatrix a => String -> Proxy a -> Test
testGradient name _ = testCase ("gradient " ++ name) $ HUnit.assert $ do
  let f :: (J (JV V2) :*: J (JV V3)) a
        -> (J (JV V2) :*: J (JV V3)) a
      f (xj :*: x) = gradient obj xj :*: (2 * x)
        where
          obj = 0.5 * sum1 (xj * xj)

  fun <- toFun "f" f mempty
  print fun
  o0 :*: o1 <- callDM fun (vcat (V2 1 2) :*: vcat (V3 1 2 3))

  return $ HUnit.assertEqual "" "([1, 2],[2, 4, 6])" (show (o0, o1))


functionTests :: Test
functionTests =
  testGroup
  "Function tests"
  [ functionTests' "SX" (Proxy :: Proxy SX)
  , functionTests' "MX" (Proxy :: Proxy MX)
  ]

functionTests' :: CM.SMatrix a => String -> Proxy a -> Test
functionTests' name p =
  testGroup
  name
  [ testFunction name p
  , testFun name p
  , testGradient name p
  , testJacobian name p
  , testHessian name p
  ]
