{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MapTests
       ( mapTests
       ) where

import Casadi.CMatrix ( trans, vertcat, fromDVector )
import Casadi.DMatrix ( DMatrix )
import Casadi.Option ( Opt )
import Casadi.SX ( SX )
import qualified Data.Map as M
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import Linear
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Text.Printf ( printf )

import Dyno.Vectorize
import Dyno.View.Fun
import Dyno.View.HList
import Dyno.View.M ( M, hcat, hsplit, vcat, vsplit )
import Dyno.View.MapFun
import Dyno.View.JV
import Dyno.View.JVec
import Dyno.View.View
import Dyno.View.Unsafe ( mkM )

toHUnit :: IO (Maybe String) -> HUnit.Assertion
toHUnit f = HUnit.assert $ do
  r <- f
  case r of
    Just msg -> return (HUnit.assertString msg)
    Nothing -> return (HUnit.assertBool "LGTM" True)

blockCat :: [[Double]] -> DMatrix
blockCat x0 = vertcat (V.fromList x1)
  where
    x1 :: [DMatrix]
    x1 = map (trans . fromDVector . V.fromList) x0

testFun0 ::
  (Proxy 4 -> String -> SXFun (J (JV V2)) (J (JV V3)) -> M.Map String Opt
   -> IO (Fun
          (M (JV V2) (JVec 4 (JV Id)))
          (M (JV V3) (JVec 4 (JV Id)))
         )
  )
  -> HUnit.Assertion
testFun0 theMapFun = toHUnit $ do
  let f :: J (JV V2) SX -> J (JV V3) SX
      f x = catJV' $ V3 (10*x0) (100*x1) (1000*x1)
        where
          V2 x0 x1 = splitJV' x

  fun <- toSXFun "v2_in_v3_out" f :: IO (SXFun (J (JV V2)) (J (JV V3)))
  mapF <- theMapFun Proxy "map_v2_in_v3_out" fun M.empty

  let input :: M (JV V2) (JVec 4 (JV Id)) DMatrix
      input = mkM $ blockCat [ [1, 3, 5, 7]
                             , [2, 4, 6, 8]
                             ]

  out <- eval mapF input :: IO (M (JV V3) (JVec 4 (JV Id)) DMatrix)
  let expectedOut = mkM $ blockCat
                    [ [  10,   30,   50,   70]
                    , [ 200,  400,  600,  800]
                    , [2000, 4000, 6000, 8000]
                    ]

  return $
    if out == expectedOut
    then Nothing
    else Just $ printf "expected: %s\nactual: %s" (show expectedOut) (show out)

testFun1 ::
  (Proxy 4 -> String -> SXFun (J (JV V2) :*: J (JV Id)) (J (JV V3) :*: J (JV Id))
   -> M.Map String Opt
   -> IO (Fun
          (M (JV V2) (JVec 4 (JV Id)) :*: M (JV Id) (JVec 4 (JV Id)))
          (M (JV V3) (JVec 4 (JV Id)) :*: M (JV Id) (JVec 4 (JV Id)))
         )
  )
  -> HUnit.Assertion
testFun1 theMapFun = toHUnit $ do
  let f :: (J (JV V2) :*: J (JV Id)) SX -> (J (JV V3) :*: J (JV Id)) SX
      f (x :*: y) = o0 :*: o1
        where
          o0 = catJV' $ V3 (10*x0) (100*x1) (1000*x1)
          o1 = catJV' $ Id (2*y0)
          V2 x0 x1 = splitJV' x
          Id y0 = splitJV' y

  fun <- toSXFun "v2id_in_v3id_out" f
  mapF <- theMapFun Proxy "map_v2id_in_v3id_out" fun M.empty

  let input0 :: M (JV V2) (JVec 4 (JV Id)) DMatrix
      input0 = mkM $ blockCat [ [1, 3, 5, 7]
                              , [2, 4, 6, 8]
                              ]
      input1 :: M (JV Id) (JVec 4 (JV Id)) DMatrix
      input1 = mkM $ blockCat [ [1, 2, 3, 4]
                              ]

  out0 :*: out1 <- eval mapF (input0 :*: input1)
  let expectedOut0 = mkM $ blockCat
                     [ [  10,   30,   50,   70]
                     , [ 200,  400,  600,  800]
                     , [2000, 4000, 6000, 8000]
                     ]
      expectedOut1 = mkM $ blockCat [[2, 4, 6, 8]]

      msg0 = printf "output 0\nexpected: %s\nactual: %s" (show expectedOut0) (show out0)
      msg1 = printf "output 1\nexpected: %s\nactual: %s" (show expectedOut1) (show out1)
  return $ case (out0 == expectedOut0, out1 == expectedOut1) of
    (True, True) -> Nothing
    (False, True) -> Just msg0
    (True, False) -> Just msg1
    (False, False) -> Just (msg0 ++ "\n" ++ msg1)


testFun2 ::
  (Proxy 2 -> String -> SXFun (M (JV V2) (JV V3)) (M (JV V3) (JV V4))
   -> M.Map String Opt
   -> IO (Fun
          (M (JV V2) (JVec 2 (JV V3)))
          (M (JV V3) (JVec 2 (JV V4)))
         )
  )
  -> HUnit.Assertion
testFun2 theMapFun = toHUnit $ do
  let f :: M (JV V2) (JV V3) SX -> M (JV V3) (JV V4) SX
      f x = vcat (V3 o0 o1 o2)
        where
          V2 x0 x1 = vsplit x
          V3 x00 x01 x02 = hsplit x0
          V3 x10 x11 x12 = hsplit x1

          o0 = hcat $ V4 (x00) (2*x01) (3*x02) 8
          o1 = hcat $ V4 (x10) (2*x11) (3*x12) 9
          o2 = hcat $ V4 (4*x00) (5*x01) (6*x02) 10

  fun <- toSXFun "f" f
  mapF <- theMapFun Proxy "map_f" fun M.empty

  let input :: M (JV V2) (JVec 2 (JV V3)) DMatrix
      input = mkM $ blockCat [ [1, 3, 5, 10, 12, 14]
                             , [2, 4, 6, 11, 13, 15]
                             ]

  out <- eval mapF input
  let expectedOut = mkM $ blockCat
                    [ [1, 6, 15, 8, 10, 24, 42, 8]
                    , [2, 8, 18, 9, 11, 26, 45, 9]
                    , [4, 15, 30, 10, 40, 60, 84, 10]
                    ]
  return $
    if out == expectedOut
    then Nothing
    else Just $ printf "expected: %s\nactual: %s" (show expectedOut) (show out)

testFunNonRepeated :: HUnit.Assertion
testFunNonRepeated = toHUnit $ do
  let f :: (J (JV V2) :*: J (JV Id)) SX -> (J (JV V3) :*: J (JV Id)) SX
      f (x :*: y) = o0 :*: o1
        where
          o0 = catJV' $ V3 (10*x0) (100*x1) (1000*x1)
          o1 = catJV' $ Id (2*y0)
          V2 x0 x1 = splitJV' x
          Id y0 = splitJV' y

  fun <- toSXFun "f" f
  mapF <- mapFun' (Proxy :: Proxy 5) "map_f" fun M.empty

  let input0 :: M (JV V2) (JV Id) DMatrix
      input0 = mkM $ blockCat [ [1]
                              , [2]
                              ]
      input1 :: M (JV Id) (JVec 5 (JV Id)) DMatrix
      input1 = mkM $ blockCat [ [1, 2, 3, 4, 5]
                              ]

  out0 :*: out1 <- eval mapF (input0 :*: input1)
  let expectedOut0 ::M (JV V3) (JV Id) DMatrix
      expectedOut0 = mkM $ blockCat
                     [ [   50]
                     , [ 1000]
                     , [10000]
                     ]
      expectedOut1 ::M (JV Id) (JVec 5 (JV Id)) DMatrix
      expectedOut1 = mkM $ blockCat [[2, 4, 6, 8 ,10]]

      msg0 = printf "output 0\nexpected: %s\nactual: %s" (show expectedOut0) (show out0)
      msg1 = printf "output 1\nexpected: %s\nactual: %s" (show expectedOut1) (show out1)
  return $ case (out0 == expectedOut0, out1 == expectedOut1) of
    (True, True) -> Nothing
    (False, True) -> Just msg0
    (True, False) -> Just msg1
    (False, False) -> Just (msg0 ++ "\n" ++ msg1)


mapTests :: Test
mapTests =
  testGroup "map tests"
  [ testGroup "V2 in, V3 out"
    [ testCase "mapFun"  $ testFun0 mapFun
    , testCase "mapFun'" $ testFun0 mapFun'
    ]
  , testGroup "(V2 :*: Id) in, (V3 :*: Id) out"
    [ testCase "mapFun"  $ testFun1 mapFun
    , testCase "mapFun'" $ testFun1 mapFun'
    ]
  , testGroup "(M V2 V3) in, (M V3 V4) out"
    [ testCase "mapFun"  $ testFun2 mapFun
    , testCase "mapFun'" $ testFun2 mapFun'
    ]
  , testCase "non-repeated" testFunNonRepeated
  ]
