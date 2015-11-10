{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module ViewTests
       ( Views(..)
       , CMatrices(..)
       , viewTests
       ) where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Map as M
import Data.Proxy ( Proxy(..) )
import qualified Data.Binary as B
import qualified Data.Serialize as S
import qualified Data.Traversable as T
import Linear ( V1(..), V2(..), V3(..), V4(..) )
import qualified Numeric.LinearAlgebra as Mat
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import qualified Test.HUnit.Base as HUnit
import Test.QuickCheck
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Casadi.Function ( evalDMatrix )
import Casadi.MXFunction ( mxFunction )
import Casadi.CMatrix ( CMatrix )
import qualified Casadi.CMatrix as CM
import Casadi.DMatrix ( DMatrix )
import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.Viewable ( Viewable )

import Dyno.View.Unsafe ( M(UnsafeM), mkM )
import Dyno.TypeVecs ( Vec, Dim )
import Dyno.Vectorize ( Vectorize(..), Id, fill )
import Dyno.View.View ( View(..), J, JV, JNone, JTuple, JTriple, JQuad )
import Dyno.View.JVec ( JVec )
import Dyno.View.M
import Dyno.View.Cov ( Cov, fromMat, toMat )

import Utils
import VectorizeTests ( Vectorizes(..), Dims(..) )

data Views where
  Views :: View f =>
           { vwShrinks :: [Views]
           , vwName :: String
           , vwProxy :: Proxy f
           } -> Views
instance Show Views where
  show = vwName

data CMatrices where
  CMatrices :: (Viewable f, CMatrix f, MyEq f) =>
               { cmName :: String
               , cmProxy :: Proxy f
               } -> CMatrices
instance Show CMatrices where
  show = cmName

-- MX is less frequent because evalMX takes a while
instance Arbitrary CMatrices where
  arbitrary = frequency [ (1, return (CMatrices "MX" (Proxy :: Proxy MX)))
                        , (5, return (CMatrices "SX" (Proxy :: Proxy SX)))
                        , (5, return (CMatrices "DMatrix" (Proxy :: Proxy DMatrix)))
                        ]
instance (View f, View g, CMatrix a) => Arbitrary (M f g a) where
  arbitrary = do
    let prim :: Gen (M f g a)
        prim = oneof
               [ return $ zeros
               , return $ countUp
               , return $ fromInteger 0
               , return $ fromRational 0
               , fmap fromInteger arbitrary
               , fmap fromRational arbitrary
               ]
        positive :: Gen (M f g a)
        positive = elements
                   [ ones
                   , 1 + countUp
                   , pi
                   ]
    x <- prim
    y <- prim
    z <- positive
    oneof [ return $ x
          , return $ x * y
          , return $ x + y
          , return $ x - y
          , return $ x / z
          , fmap trans (arbitrary :: Gen (M g f a))
          ]

instance (Arbitrary a, Dim n) => Arbitrary (Vec n a) where
  arbitrary = T.sequence (fill arbitrary)

evalMX :: MX -> DMatrix
evalMX x = unsafePerformIO $ do
  f <- mxFunction "evalMX" V.empty (V.singleton x) M.empty
  ret <- evalDMatrix f V.empty
  return (V.head ret)

data JX0 f a = JX0 (J (JV f) a) (J (JV f) a) deriving (Show, Generic, Generic1)
instance Vectorize f => View (JX0 f)
--instance Scheme JX

data JX1 f g a = JX1 (J (JV f) a) (J g a) deriving (Show, Generic, Generic1)
instance (Vectorize f, View g) => View (JX1 f g)
--instance Scheme JX

data JX2 f g h a = JX2 (J f a) (J (JTuple g (JV h)) a) (J g a) (J (JV h) a) (J f a)
                 deriving (Show, Generic, Generic1)
instance (View f, View g, Vectorize h) => View (JX2 f g h)
----instance Scheme JX2

maxViewSize :: Int
maxViewSize = 200

class MyEq a where
  myEq :: a -> a -> Bool

instance MyEq a => MyEq (M f g a) where
  myEq (UnsafeM x) (UnsafeM y) = myEq x y
instance MyEq SX where
  myEq = (==)
instance MyEq DMatrix where
  myEq = (==)
instance MyEq MX where
  myEq x y = myEq (evalMX x) (evalMX y)
instance (Dim n, MyEq a) => MyEq (Vec n a) where
  myEq f g = V.and $ V.zipWith myEq (vectorize f) (vectorize g)
instance MyEq (Mat.Matrix Double) where
  myEq x y
    | and [rowx == 0, rowy == 0, colx == coly] = True
    | and [colx == 0, coly == 0, rowx == rowy] = True
    | otherwise = x == y
    where
      rowx = Mat.rows x
      colx = Mat.cols x
      rowy = Mat.rows y
      coly = Mat.cols y

instance Arbitrary Views where
  arbitrary = do
    x <- oneof [primitives, compound primitives, compound (compound primitives)]
    if viewSize x <= maxViewSize then return x else arbitrary
  shrink = filter ((<= maxViewSize) . viewSize) . vwShrinks

compound :: Gen Views -> Gen Views
compound genIt = do
  vc'@(Vectorizes _ mz pz) <- arbitrary
  let vc = mkJV vc'
  vw0@(Views {vwName = mv0, vwProxy = pv0}) <- genIt
  vw1@(Views {vwName = mv1, vwProxy = pv1}) <- genIt
  vw2@(Views {vwName = mv2, vwProxy = pv2}) <- genIt
  elements
    [ Views { vwShrinks = [vc]
            , vwName = "JX0 (" ++ mz ++ ")"
            , vwProxy = reproxy (Proxy :: Proxy JX0) pz
            }
    , Views { vwShrinks = [vc,vw0]
            , vwName = "JX1 (" ++ mz ++ ") (" ++ mv0 ++ ")"
            , vwProxy = reproxy2 (Proxy :: Proxy JX1) pz pv0
            }
    , Views { vwShrinks = [vc, vw0, vw1]
            , vwName = "JX2 (" ++ mv0 ++ ") (" ++ mv1 ++ ") (" ++ mz ++ ")"
            , vwProxy = reproxy3 (Proxy :: Proxy JX2) pv0 pv1 pz
            }
    , Views { vwShrinks = [vw0]
            , vwName = "Cov (" ++ mv0 ++ ")"
            , vwProxy = reproxy (Proxy :: Proxy Cov) pv0
            }
    , Views { vwShrinks = [vw0,vw1]
            , vwName = "JTuple (" ++ mv0 ++ ") (" ++ mv1 ++ ")"
            , vwProxy = reproxy2 (Proxy :: Proxy JTuple) pv0 pv1
            }
    , Views { vwShrinks = [vw0,vw1,vw2]
            , vwName = "JTriple (" ++ mv0 ++ ") (" ++ mv1 ++ ") (" ++ mv2 ++ ")"
            , vwProxy = reproxy3 (Proxy :: Proxy JTriple) pv0 pv1 pv2
            }
    ]

viewSize :: Views -> Int
viewSize (Views _ _ p) = size p

mkJV :: Vectorizes -> Views
mkJV = mkJV' True
  where
    mkJV' :: Bool -> Vectorizes -> Views
    mkJV' sh v@(Vectorizes _ m p) = Views { vwShrinks = shrinks
                                          , vwName = "JV (" ++ m ++ ")"
                                          , vwProxy = reproxyJV p
                                          }
      where
        shrinks :: [Views]
        shrinks = if sh then map (mkJV' False) (shrink v) else []

        reproxyJV :: Proxy f -> Proxy (JV f)
        reproxyJV = const Proxy

primitives :: Gen Views
primitives = do
  v <- arbitrary
  elements
    [ Views {vwShrinks = [], vwName = "JNone", vwProxy = Proxy :: Proxy JNone}
    , mkJV v
    ]

--data M1 a = M1 (M JX JX2 a) deriving (Show, Generic, Generic1)
--data M2 a = M2 (M JNone JNone a) deriving (Show, Generic, Generic1)
--data M3 a = M3 (M JX2 JNone a) deriving (Show, Generic, Generic1)
--data M4 a = M4 (M JNone JX2 a) deriving (Show, Generic, Generic1)

--instance Scheme M1
--instance Scheme M2
--instance Scheme M3
--instance Scheme M4

beEqual :: (MyEq a, Show a) => a -> a -> Property
beEqual x y = counterexample (sx ++ " =/= " ++ sy) (myEq x y)
  where
    sx = show x
    sy = show y

prop_VSplitVCat :: Test
prop_VSplitVCat =
  testProperty "vcat . vsplit" $
  \(Vectorizes _ _ p1) (Views {vwProxy = p2}) (CMatrices {cmProxy = pm}) -> test p1 p2 pm
  where
    test :: forall f g a
            . (Vectorize f, View g, CMatrix a, MyEq a)
            => Proxy f -> Proxy g -> Proxy a -> Gen Property
    test _ _ _ = do
      x0 <- arbitrary :: Gen (M (JV f) g a)
      let x1 = vcat (vsplit x0) :: M (JV f) g a
      return $ beEqual x0 x1

prop_HSplitHCat :: Test
prop_HSplitHCat  =
  testProperty "hcat . hsplit" $
  \(Views {vwProxy = p1}) (Vectorizes _ _ p2) (CMatrices {cmProxy = pm}) -> test p1 p2 pm
  where
    test :: forall f g a
            . (View f, Vectorize g, CMatrix a, MyEq a)
            => Proxy f -> Proxy g -> Proxy a -> Gen Property
    test _ _ _ = do
      x0 <- arbitrary :: Gen (M f (JV g) a)
      let x1 = hcat (hsplit x0) :: M f (JV g) a
      return $ beEqual x0 x1

prop_VSplitVCat' :: Test
prop_VSplitVCat'  =
  testProperty "vsplit' . vcat'" $
  \(Dims _ pd) (Views {vwProxy = p1}) (Views {vwProxy = p2}) (CMatrices {cmProxy = pm}) ->
   test pd p1 p2 pm
  where
    test :: forall f g n a
            . (View f, View g, Dim n, CMatrix a, MyEq a)
            => Proxy n -> Proxy f -> Proxy g -> Proxy a -> Gen Property
    test _ _ _ _ = do
      x0 <- arbitrary :: Gen (Vec n (M f g a))
      let x1 = vsplit' (vcat' x0) :: Vec n (M f g a)
      return $ beEqual x0 x1

prop_HSplitHCat' :: Test
prop_HSplitHCat' =
  testProperty "hsplit' . hcat'" $
  \(Dims _ pd) (Views {vwProxy = p1}) (Views {vwProxy = p2}) (CMatrices {cmProxy = pm}) ->
   test pd p1 p2 pm
  where
    test :: forall f g n a
            . (View f, View g, Dim n, CMatrix a, MyEq a)
            => Proxy n -> Proxy f -> Proxy g -> Proxy a -> Gen Property
    test _ _ _ _ = do
      x0 <- arbitrary :: Gen (Vec n (M f g a))
      let x1 = hsplit' (hcat' x0) :: Vec n (M f g a)
      return $ beEqual x0 x1

prop_testSplitJ :: Test
prop_testSplitJ  =
  testProperty "split . cat J" $
  \(Vectorizes _ _ p) (CMatrices {cmProxy = pm}) -> test p pm
  where
    test :: forall f a
            . (Vectorize f, CMatrix a, Viewable a, MyEq a)
            => Proxy f -> Proxy a -> Gen Property
    test _ _ = do
      UnsafeM xm0 <- arbitrary :: Gen (M (JV f) (JV Id) a)
      let xj0 = mkM xm0 :: J (JV f) a
          xj1 = split xj0  :: JV f a
          xj2 = cat xj1 :: J (JV f) a
      return $ beEqual xj0 xj2

prop_toFromHMat :: Test
prop_toFromHMat =
  testProperty "fromHMat . toHMat" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) -> test p1 p2
  where
    test :: forall f g
            . (View f, View g)
            => Proxy f -> Proxy g -> Gen Property
    test _ _ = do
      m0 <- arbitrary :: Gen (M f g DMatrix)
      let m1 = toHMat m0 :: Mat.Matrix Double
          m2 = fromHMat m1 :: M f g DMatrix
      return $ beEqual m0 m2

prop_fromToHMat :: Test
prop_fromToHMat =
  testProperty "toHMat . fromHMat" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) -> test p1 p2
  where
    test :: forall f g
            . (View f, View g)
            => Proxy f -> Proxy g -> Gen Property
    test _ _ = do
      m0 <- arbitrary :: Gen (M f g DMatrix)
      let m1 = toHMat m0 :: Mat.Matrix Double
          m2 = fromHMat m1 :: M f g DMatrix
          m3 = toHMat m2 :: Mat.Matrix Double
      return $ beEqual m1 m3

prop_covToFromMat :: Test
prop_covToFromMat =
  testProperty "fromMat . toMat" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) -> test p1 p2
  where
    test :: forall f g
            . (View f, View g)
            => Proxy f -> Proxy g -> Gen Property
    test _ _ = do
      m0 <- arbitrary :: Gen (J (Cov f) DMatrix)
      let m1 = toMat m0 :: M f f DMatrix
          m2 = fromMat m1 :: J (Cov f) DMatrix
      return $ beEqual m0 m2

prop_covFromToMat :: Test
prop_covFromToMat =
  testProperty "toMat . fromMat" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) -> test p1 p2
  where
    test :: forall f g
            . (View f, View g)
            => Proxy f -> Proxy g -> Gen Property
    test _ _ = do
      m0' <- arbitrary :: Gen (M f f DMatrix)
      let m0 = 0.5 * (m0' + trans m0') -- make it symmetric
          m1 = fromMat m0 :: J (Cov f) DMatrix
          m2 = toMat m1 :: M f f DMatrix
      return $ beEqual m0 m2

prop_serializeDeserializeBinary :: Test
prop_serializeDeserializeBinary =
  testProperty "(M f g DMatrix): Binary deserialize . serialize" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) -> test p1 p2
  where
    test :: forall f g . (View f, View g) => Proxy f -> Proxy g -> Gen Property
    test _ _ = do
      m0 <- arbitrary :: Gen (M f g DMatrix)
      let m1 = B.encode m0
      return $
        case B.decodeOrFail m1 of
         Left (_,_,msg) -> counterexample ("deserialization failure " ++ show msg) False
         Right (_,_,m2) -> beEqual m0 m2

prop_serializeDeserializeCereal :: Test
prop_serializeDeserializeCereal =
  testProperty "(M f g DMatrix): Cereal deserialize . serialize" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) -> test p1 p2
  where
    test :: forall f g . (View f, View g) => Proxy f -> Proxy g -> Gen Property
    test _ _ = do
      m0 <- arbitrary :: Gen (M f g DMatrix)
      let m1 = S.encode m0
      return $
        case S.decode m1 of
         Left msg -> counterexample ("deserialization failure " ++ show msg) False
         Right m2 -> beEqual m0 m2

prop_vsplitTup :: Test
prop_vsplitTup =
  testProperty "vcatTup . vsplitTup" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) (Views {vwProxy = p3}) (CMatrices {cmProxy = p4})
  -> test p1 p2 p3 p4
  where
    test :: forall f g h a
            . (View f, View g, View h, CMatrix a, MyEq a)
            => Proxy f -> Proxy g -> Proxy h -> Proxy a
            -> Gen Property
    test _ _ _ _ = do
      m0 <- arbitrary :: Gen (M (JTuple f g) h a)
      let (mx,my) = vsplitTup m0
          m1 = vcatTup mx my
      return (beEqual m0 m1)

prop_hsplitTup :: Test
prop_hsplitTup =
  testProperty "hcatTup . hsplitTup" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) (Views {vwProxy = p3}) (CMatrices {cmProxy = p4})
  -> test p1 p2 p3 p4
  where
    test :: forall f g h a
            . (View f, View g, View h, CMatrix a, MyEq a)
            => Proxy f -> Proxy g -> Proxy h -> Proxy a
            -> Gen Property
    test _ _ _ _ = do
      m0 <- arbitrary :: Gen (M f (JTuple g h) a)
      let (mx,my) = hsplitTup m0
          m1 = hcatTup mx my
      return (beEqual m0 m1)

prop_vsplitTrip :: Test
prop_vsplitTrip =
  testProperty "vcatTrip . vsplitTrip" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) (Views {vwProxy = p3}) (Views {vwProxy = p4}) (CMatrices {cmProxy = p5})
  -> test p1 p2 p3 p4 p5
  where
    test :: forall f1 f2 f3 g a
            . (View f1, View f2, View f3, View g, CMatrix a, MyEq a)
            => Proxy f1 -> Proxy f2 -> Proxy f3 -> Proxy g -> Proxy a
            -> Gen Property
    test _ _ _ _ _ = do
      m0 <- arbitrary :: Gen (M (JTriple f1 f2 f3) g a)
      let (mx,my,mz) = vsplitTrip m0
          m1 = vcatTrip mx my mz
      return (beEqual m0 m1)

prop_hsplitTrip :: Test
prop_hsplitTrip =
  testProperty "hcatTrip . hsplitTrip" $
  \(Views {vwProxy = p1}) (Views {vwProxy = p2}) (Views {vwProxy = p3}) (Views {vwProxy = p4}) (CMatrices {cmProxy = p5})
  -> test p1 p2 p3 p4 p5
  where
    test :: forall f g1 g2 g3 a
            . (View f, View g1, View g2, View g3, CMatrix a, MyEq a)
            => Proxy f -> Proxy g1 -> Proxy g2 -> Proxy g3 -> Proxy a
            -> Gen Property
    test _ _ _ _ _ = do
      m0 <- arbitrary :: Gen (M f (JTriple g1 g2 g3) a)
      let (mx,my,mz) = hsplitTrip m0
          m1 = hcatTrip mx my mz
      return (beEqual m0 m1)

prop_vsplitQuad :: Test
prop_vsplitQuad =
  testProperty "vcatQuad . vsplitQuad" $
  \(Views {vwProxy = p0}) (Views {vwProxy = p1}) (Views {vwProxy = p2}) (Views {vwProxy = p3}) (Views {vwProxy = p4}) (CMatrices {cmProxy = p5})
  -> test p0 p1 p2 p3 p4 p5
  where
    test :: forall f0 f1 f2 f3 g a
            . (View f0, View f1, View f2, View f3, View g, CMatrix a, MyEq a)
            => Proxy f0 -> Proxy f1 -> Proxy f2 -> Proxy f3 -> Proxy g -> Proxy a
            -> Gen Property
    test _ _ _ _ _ _ = do
      m0 <- arbitrary :: Gen (M (JQuad f0 f1 f2 f3) g a)
      let (mf0,mf1,mf2,mf3) = vsplitQuad m0
          m1 = vcatQuad mf0 mf1 mf2 mf3
      return (beEqual m0 m1)

prop_hsplitQuad :: Test
prop_hsplitQuad =
  testProperty "hcatQuad . hsplitQuad" $
  \(Views {vwProxy = p0}) (Views {vwProxy = p1}) (Views {vwProxy = p2}) (Views {vwProxy = p3}) (Views {vwProxy = p4}) (CMatrices {cmProxy = p5})
  -> test p0 p1 p2 p3 p4 p5
  where
    test :: forall f g0 g1 g2 g3 a
            . (View f, View g0, View g1, View g2, View g3, CMatrix a, MyEq a)
            => Proxy f -> Proxy g0 -> Proxy g1 -> Proxy g2 -> Proxy g3 -> Proxy a
            -> Gen Property
    test _ _ _ _ _ _ = do
      m0 <- arbitrary :: Gen (M f (JQuad g0 g1 g2 g3) a)
      let (mg0,mg1,mg2,mg3) = hsplitQuad m0
          m1 = hcatQuad mg0 mg1 mg2 mg3
      return (beEqual m0 m1)


---------- this next part is to test blockcat/blocksplit -----------
data BV a = BV (J (JV V3) a) (J (JV V1) a) (J (JV V2) a) (J (JV V1) a)
          deriving Generic
data BH a = BH (J (JV V2) a) (J (JV V4) a) deriving Generic
instance View BV
instance View BH

blockcat' :: [[DMatrix]] -> DMatrix
blockcat' = CM.blockcat . V.fromList . map V.fromList

blockcatScalars :: Num a => [[a]]
blockcatScalars =
  [ [ 0,  1,     2,  3,  4,  5]
  , [ 6,  7,     8,  9, 10, 11]
  , [12, 13,    14, 15, 16, 17]

  , [18, 19,    20, 21, 22, 23]

  , [24, 25,    26, 27, 28, 29]
  , [30, 31,    32, 33, 34, 35]

  , [36, 37,    38, 39, 40, 41]
  ]

blockcatBlocks :: Vector (Vector DMatrix)
blockcatBlocks = V.fromList $ map V.fromList
  [ [x00, x01]
  , [x10, x11]
  , [x20, x21]
  , [x30, x31]
  ]
  where
    x00 = blockcat'
      [ [ 0,  1]
      , [ 6,  7]
      , [12, 13]
      ]

    x01 = blockcat'
      [ [ 2,  3,  4,  5]
      , [ 8,  9, 10, 11]
      , [14, 15, 16, 17]
      ]

    x10 = blockcat' [[18, 19]]
    x11 = blockcat' [[20, 21, 22, 23]]

    x20 = blockcat'
      [ [24, 25]
      , [30, 31]
      ]

    x21 = blockcat'
      [ [26, 27, 28, 29]
      , [32, 33, 34, 35]
      ]

    x30 = blockcat' [[36, 37]]
    x31 = blockcat' [[38, 39, 40, 41]]

blockCountUp :: M BV BH DMatrix
blockCountUp = countUp

test_blockcatScalars :: HUnit.Assertion
test_blockcatScalars = HUnit.assertEqual "" x y
  where
    x :: M BV BH DMatrix
    x = blockCountUp

    y :: M BV BH DMatrix
    y = mkM $ blockcat' blockcatScalars

test_blockcatBlocks :: HUnit.Assertion
test_blockcatBlocks = HUnit.assertEqual "" x y
  where
    x :: M BV BH DMatrix
    x = blockCountUp

    y :: M BV BH DMatrix
    y = mkM $ CM.blockcat blockcatBlocks

test_blockSplit :: HUnit.Assertion
test_blockSplit = HUnit.assertEqual "" x y
  where
    x, y :: V.Vector (V.Vector DMatrix)
    x = blockcatBlocks
    y = blockSplit blockCountUp

----------------- sumRows/sumCols ---------------
sumInput :: M (JV V2) (JV V3) DMatrix
sumInput = countUp

-- make sure the countUp is doing what I expect
test_sumInput :: HUnit.Assertion
test_sumInput = HUnit.assertEqual "" x sumInput
  where
    x :: M (JV V2) (JV V3) DMatrix
    x = vcat (V2 r0 r1)

    r0, r1 :: M (JV Id) (JV V3) DMatrix
    r0 = hcat $ V3 0 1 2
    r1 = hcat $ V3 3 4 5

test_sumRows :: HUnit.Assertion
test_sumRows = HUnit.assertEqual "" x y
  where
    x :: M (JV Id) (JV V3) DMatrix
    x = hcat (V3 3 5 7)

    y :: M (JV Id) (JV V3) DMatrix
    y = sumRows sumInput

test_sumCols :: HUnit.Assertion
test_sumCols = HUnit.assertEqual "" x y
  where
    x :: M (JV V2) (JV Id) DMatrix
    x = vcat (V2 3 12)

    y :: M (JV V2) (JV Id) DMatrix
    y = sumCols sumInput

test_reshape :: HUnit.Assertion
test_reshape = HUnit.assertEqual "" x y
  where
    j :: J (JVec 3 (JV V2)) DMatrix
    j = countUp

    x :: M (JV V2) (JVec 3 (JV Id)) DMatrix
    x = mkM $ blockcat'
        [ [0, 2, 4]
        , [1, 3, 5]
        ]

    y :: M (JV V2) (JVec 3 (JV Id)) DMatrix
    y = reshape j

viewTests :: Test
viewTests =
  testGroup "view tests" $
  [ testCase "blockcat scalars" test_blockcatScalars
  , testCase "blockcat blocks" test_blockcatBlocks
  , testCase "blocksplit" test_blockSplit
  , testCase "reshape" test_reshape
  , testCase "sumInput" test_sumInput
  , testCase "sumRows" test_sumRows
  , testCase "sumCols" test_sumCols
  , prop_VSplitVCat
  , prop_HSplitHCat
  , prop_VSplitVCat'
  , prop_HSplitHCat'
  , prop_testSplitJ
  , prop_toFromHMat
  , prop_fromToHMat
  , prop_covFromToMat
  , prop_covToFromMat
  , prop_serializeDeserializeBinary
  , prop_serializeDeserializeCereal
  , prop_vsplitTup
  , prop_hsplitTup
  , prop_vsplitTrip
  , prop_hsplitTrip
  , prop_vsplitQuad
  , prop_hsplitQuad
  ]
