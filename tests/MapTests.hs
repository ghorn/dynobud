{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MapTests
       ( mapTests
       ) where

import qualified Casadi.Matrix as CM
import Casadi.DM ( DM )
import Casadi.GenericType ( GType )
import Casadi.MX ( MX )
import Casadi.SX ( SX )
import qualified Data.Map as M
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import Linear
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Text.Printf ( printf )

import Dyno.View.Vectorize
import Dyno.View.Fun
import Dyno.View.HList
import Dyno.View.M ( M, hcat, hsplit, vcat, vsplit )
import Dyno.View.MapFun
import Dyno.View.JVec
import Dyno.View.View
import Dyno.View.Unsafe ( mkM )

toHUnit :: IO (Maybe String) -> HUnit.Assertion
toHUnit f = HUnit.assert $ do
  r <- f
  case r of
    Just msg -> return (HUnit.assertString msg)
    Nothing -> return (HUnit.assertBool "LGTM" True)

blockcat' :: [[DM]] -> DM
blockcat' = CM.blockcat . fmap V.fromList . V.fromList

testFun0 ::
  forall a
  . CM.SMatrix a
  => Proxy a
  -> (Proxy 4 -> Fun (J (JV V2)) (J (JV V3)) -> String
      -> MapStrategy -> M.Map String GType
      -> IO (Fun
             (M (JV V2) (JVec 4 (JV Id)))
             (M (JV V3) (JVec 4 (JV Id)))
            )
     )
  -> HUnit.Assertion
testFun0 _ theMapFun = toHUnit $ do
  let f :: J (JV V2) a -> J (JV V3) a
      f x = vcat $ V3 (10*x0) (100*x1) (1000*x1)
        where
          V2 x0 x1 = vsplit x

  fun <- toFun "v2_in_v3_out" f mempty :: IO (Fun (J (JV V2)) (J (JV V3)))
  mapF <- theMapFun Proxy fun "map_v2_in_v3_out" Serial mempty

  let input :: M (JV V2) (JVec 4 (JV Id)) DM
      input = mkM $ blockcat'
              [ [1, 3, 5, 7]
              , [2, 4, 6, 8]
              ]

  out <- callDM mapF input :: IO (M (JV V3) (JVec 4 (JV Id)) DM)
  let expectedOut = mkM $ blockcat'
                    [ [  10,   30,   50,   70]
                    , [ 200,  400,  600,  800]
                    , [2000, 4000, 6000, 8000]
                    ]

  return $
    if out == expectedOut
    then Nothing
    else Just $ printf "expected: %s\nactual: %s" (show expectedOut) (show out)

testFun1 ::
  forall a
  . CM.SMatrix a
  => Proxy a
  -> (Proxy 4 -> Fun (J (JV V2) :*: S) (J (JV V3) :*: S) -> String
      -> MapStrategy -> M.Map String GType
      -> IO (Fun
             (M (JV V2) (JVec 4 (JV Id)) :*: M (JV Id) (JVec 4 (JV Id)))
             (M (JV V3) (JVec 4 (JV Id)) :*: M (JV Id) (JVec 4 (JV Id)))
            )
     )
  -> HUnit.Assertion
testFun1 _ theMapFun = toHUnit $ do
  let f :: (J (JV V2) :*: S) a -> (J (JV V3) :*: S) a
      f (x :*: y) = o0 :*: o1
        where
          o0 = vcat $ V3 (10*x0) (100*x1) (1000*x1)
          o1 = vcat $ Id (2*y0)
          V2 x0 x1 = vsplit x
          Id y0 = vsplit y

  fun <- toFun "v2id_in_v3id_out" f mempty
  mapF <- theMapFun Proxy fun "map_v2id_in_v3id_out" Serial mempty

  let input0 :: M (JV V2) (JVec 4 (JV Id)) DM
      input0 = mkM $ blockcat'
               [ [1, 3, 5, 7]
               , [2, 4, 6, 8]
               ]
      input1 :: M (JV Id) (JVec 4 (JV Id)) DM
      input1 = mkM $ blockcat'
               [ [1, 2, 3, 4]
               ]

  out0 :*: out1 <- callDM mapF (input0 :*: input1)
  let expectedOut0 = mkM $ blockcat'
                     [ [  10,   30,   50,   70]
                     , [ 200,  400,  600,  800]
                     , [2000, 4000, 6000, 8000]
                     ]
      expectedOut1 = mkM $ blockcat' [[2, 4, 6, 8]]

      msg0 = printf "output 0\nexpected: %s\nactual: %s" (show expectedOut0) (show out0)
      msg1 = printf "output 1\nexpected: %s\nactual: %s" (show expectedOut1) (show out1)
  return $ case (out0 == expectedOut0, out1 == expectedOut1) of
    (True, True) -> Nothing
    (False, True) -> Just msg0
    (True, False) -> Just msg1
    (False, False) -> Just (msg0 ++ "\n" ++ msg1)


testFun2 ::
  forall a
  . CM.SMatrix a
  => Proxy a
  -> (Proxy 2 -> Fun (M (JV V2) (JV V3)) (M (JV V3) (JV V4)) -> String
      -> MapStrategy -> M.Map String GType
      -> IO (Fun
             (M (JV V2) (JVec 2 (JV V3)))
             (M (JV V3) (JVec 2 (JV V4)))
            )
     )
  -> HUnit.Assertion
testFun2 _ theMapFun = toHUnit $ do
  let f :: M (JV V2) (JV V3) a -> M (JV V3) (JV V4) a
      f x = vcat (V3 o0 o1 o2)
        where
          V2 x0 x1 = vsplit x
          V3 x00 x01 x02 = hsplit x0
          V3 x10 x11 x12 = hsplit x1

          o0 = hcat $ V4 (x00) (2*x01) (3*x02) 8
          o1 = hcat $ V4 (x10) (2*x11) (3*x12) 9
          o2 = hcat $ V4 (4*x00) (5*x01) (6*x02) 10

  fun <- toFun "f" f mempty
  mapF <- theMapFun Proxy fun "map_f" Serial mempty

  let input :: M (JV V2) (JVec 2 (JV V3)) DM
      input = mkM $ blockcat'
              [ [1, 3, 5, 10, 12, 14]
              , [2, 4, 6, 11, 13, 15]
              ]

  out <- callDM mapF input
  let expectedOut = mkM $ blockcat'
                    [ [1, 6, 15, 8, 10, 24, 42, 8]
                    , [2, 8, 18, 9, 11, 26, 45, 9]
                    , [4, 15, 30, 10, 40, 60, 84, 10]
                    ]
  return $
    if out == expectedOut
    then Nothing
    else Just $ printf "expected: %s\nactual: %s" (show expectedOut) (show out)


testFunNonRepeated :: forall a . CM.SMatrix a => Proxy a -> HUnit.Assertion
testFunNonRepeated _ = toHUnit $ do
  let f :: (J (JV V2) :*: S) a -> (J (JV V3) :*: S) a
      f (x :*: y) = o0 :*: o1
        where
          o0 = vcat $ V3 (10*x0) (100*x1) (1000*x1)
          o1 = vcat $ Id (2*y0)
          V2 x0 x1 = vsplit x
          Id y0 = vsplit y

  fun <- toFun "f" f mempty
  mapF <- mapFun' (Proxy :: Proxy 5) fun "map_f" Serial mempty

  let input0 :: M (JV V2) (JV Id) DM
      input0 = mkM $ blockcat'
               [ [1]
               , [2]
               ]
      input1 :: M (JV Id) (JVec 5 (JV Id)) DM
      input1 = mkM $ blockcat'
               [ [1, 2, 3, 4, 5]
               ]

  out0 :*: out1 <- callDM mapF (input0 :*: input1)
  let expectedOut0 ::M (JV V3) (JV Id) DM
      expectedOut0 = mkM $ blockcat'
                     [ [   50]
                     , [ 1000]
                     , [10000]
                     ]
      expectedOut1 ::M (JV Id) (JVec 5 (JV Id)) DM
      expectedOut1 = mkM $ blockcat' [[2, 4, 6, 8 ,10]]

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
  [ mapTests' "SX" (Proxy :: Proxy SX)
  , mapTests' "MX" (Proxy :: Proxy MX)
  ]

mapTests' :: CM.SMatrix a => String -> Proxy a -> Test
mapTests' name p =
  testGroup ("map tests " ++ name)
  [ testGroup "V2 in, V3 out"
    [ testCase "mapFun"  $ testFun0 p (\n f _ s _ -> mapFun n f s)
    , testCase "mapFun'" $ testFun0 p mapFun'
    ]
  , testGroup "(V2 :*: Id) in, (V3 :*: Id) out"
    [ testCase "mapFun"  $ testFun1 p (\n f _ s _ -> mapFun n f s)
    , testCase "mapFun'" $ testFun1 p mapFun'
    ]
  , testGroup "(M V2 V3) in, (M V3 V4) out"
    [ testCase "mapFun"  $ testFun2 p (\n f _ s _ -> mapFun n f s)
    , testCase "mapFun'" $ testFun2 p mapFun'
    ]
  , testCase "non-repeated" $ testFunNonRepeated p
  ]
