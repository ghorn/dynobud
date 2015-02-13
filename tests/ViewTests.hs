{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
{-# Language GADTs #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}

module ViewTests
       ( Views(..)
       , CMatrices(..)
       , viewTests
       ) where

import GHC.Generics ( Generic1 )

import qualified Data.Traversable as T
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra ( ) -- for Eq Matrix
import qualified Data.Vector as V
import GHC.Generics ( Generic )
import System.IO.Unsafe ( unsafePerformIO )
import Test.QuickCheck
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Casadi.Function ( evalDMatrix )
import Casadi.MXFunction ( mxFunction )
import Casadi.SharedObject ( soInit )
import Casadi.CMatrix ( CMatrix )

import Dyno.View.Unsafe.View ( J(UnsafeJ), mkJ )
import Dyno.View.Unsafe.M ( M(UnsafeM) )

import Dyno.TypeVecs ( Vec, Dim )
import Dyno.Vectorize
import Dyno.View
import Dyno.View.M
import Dyno.Cov

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
  f <- mxFunction V.empty (V.singleton x)
  soInit f
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

instance MyEq a => MyEq (J f a) where
  myEq (UnsafeJ x) (UnsafeJ y) = myEq x y
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
      let xj0 = mkJ xm0 :: J (JV f) a
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

viewTests :: Test
viewTests =
  testGroup "view tests"
  [ prop_VSplitVCat
  , prop_HSplitHCat
  , prop_VSplitVCat'
  , prop_HSplitHCat'
  , prop_testSplitJ
  , prop_toFromHMat
  , prop_fromToHMat
  ]
