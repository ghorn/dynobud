{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language GADTs #-}
{-# Language DeriveGeneric #-}

module ViewTests
       ( Views(..)
       , viewTests
       ) where

import GHC.Generics ( Generic )

import Test.QuickCheck
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Dyno.TypeVecs ( Vec, Dim )
import Dyno.Vectorize
import Dyno.View
import Dyno.View.M
import Dyno.Cov

import Utils
import VectorizeTests ( Vectorizes(..), Dims(..) )

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

instance Arbitrary Views where
  arbitrary = do
    x <- oneof [primitives, compound primitives, compound (compound primitives)]
    if viewSize x <= maxViewSize then return x else arbitrary
  shrink = filter ((<= maxViewSize) . viewSize) . vwShrinks

type WX = DMatrix

compound :: Gen Views -> Gen Views
compound genIt = do
  vc'@(Vectorizes _ mz pz) <- arbitrary
  let vc = mkJV vc'
  vw0@(Views _ mv0 pv0) <- genIt
  vw1@(Views _ mv1 pv1) <- genIt
  elements
    [ Views [vc] ("JX0 (" ++ mz ++ ")") (reproxy (Proxy :: Proxy JX0) pz)
    , Views [vc,vw0] ("JX1 (" ++ mz ++ ") (" ++ mv0 ++ ")") (reproxy2 (Proxy :: Proxy JX1) pz pv0)
    , Views [vc, vw0, vw1] ("JX2 (" ++ mv0 ++ ") (" ++ mv1 ++ ") (" ++ mz ++ ")")
      (reproxy3 (Proxy :: Proxy JX2) pv0 pv1 pz)
    , Views [vw0] ("Cov (" ++ mv0 ++ ")") (reproxy (Proxy :: Proxy Cov) pv0)
    ]

viewSize :: Views -> Int
viewSize (Views _ _ p) = size p

mkJV :: Vectorizes -> Views
mkJV = mkJV' True
  where
    mkJV' :: Bool -> Vectorizes -> Views
    mkJV' sh v@(Vectorizes _ m p) = Views shrinks ("JV (" ++ m ++ ")") (reproxyJV p)
      where
        shrinks :: [Views]
        shrinks = if sh then map (mkJV' False) (shrink v) else []

        reproxyJV :: Proxy f -> Proxy (JV f)
        reproxyJV = const Proxy

primitives :: Gen Views
primitives = do
  v <- arbitrary
  elements
    [ Views [] "JNone" (Proxy :: Proxy JNone)
    , Views [] "S" (Proxy :: Proxy S)
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

data Views where
  Views :: View f =>
           { vwShrinks :: [Views]
           , vwName :: String
           , vwProxy :: Proxy f
           } -> Views
instance Show Views where
  show = vwName

beEqual :: (Eq a, Show a) => a -> a -> Property
beEqual x y = counterexample (show x ++ " =/= " ++ show y) (x == y)

prop_VSplitVCat :: Test
prop_VSplitVCat =
  testProperty "vcat . vsplit" $
  \(Vectorizes _ _ p1) (Views _ _ p2) -> test p1 p2
  where
    test :: forall f g . (Vectorize f, View g) => Proxy f -> Proxy g -> Property
    test _ _ = beEqual x0 x1
      where
        x0 :: M (JV f) g WX
        x0 = ones

        x1 :: M (JV f) g WX
        x1 = vcat (vsplit x0)

prop_HSplitHCat :: Test
prop_HSplitHCat  =
  testProperty "hcat . hsplit" $
  \(Views _ _ p1) (Vectorizes _ _ p2) -> test p1 p2
  where
    test :: forall f g . (View f, Vectorize g) => Proxy f -> Proxy g -> Property
    test _ _ = beEqual x0 x1
      where
        x0 :: M f (JV g) WX
        x0 = ones

        x1 :: M f (JV g) WX
        x1 = hcat (hsplit x0)

prop_VSplitVCat' :: Test
prop_VSplitVCat'  =
  testProperty "vsplit' . vcat'" $
  \(Dims _ pd) (Views _ _ p1) (Views _ _ p2) -> test pd p1 p2
  where
    test :: forall f g n . (View f, View g, Dim n) => Proxy n -> Proxy f -> Proxy g -> Property
    test _ _ _ = beEqual x0 x1
      where
        x0 :: Vec n (M f g WX)
        x0 = fill ones

        x1 :: Vec n (M f g WX)
        x1 = vsplit' (vcat' x0)


prop_HSplitHCat' :: Test
prop_HSplitHCat' =
  testProperty "hsplit' . hcat'" $
  \(Dims _ pd) (Views _ _ p1) (Views _ _ p2) -> test pd p1 p2
  where
    test :: forall f g n . (View f, View g, Dim n) => Proxy n -> Proxy f -> Proxy g -> Property
    test _ _ _ = beEqual x0 x1
      where
        x0 :: Vec n (M f g WX)
        x0 = fill ones

        x1 :: Vec n (M f g WX)
        x1 = hsplit' (hcat' x0)

prop_testSplitJ :: Test
prop_testSplitJ  =
  testProperty "split . cat J" $
  \(Vectorizes _ _ p) -> test p
  where
    test :: forall f . Vectorize f => Proxy f -> Property
    test _ = beEqual xj0 xj2
      where
        UnsafeM xm0 = ones :: M (JV f) (JV Id) WX

        xj0 :: J (JV f) WX
        xj0 = mkJ xm0

        xj1 :: JV f WX
        xj1 = split xj0

        xj2 :: J (JV f) WX
        xj2 = cat xj1

viewTests :: Test
viewTests =
  testGroup "view tests"
  [ prop_VSplitVCat
  , prop_HSplitHCat
  , prop_VSplitVCat'
  , prop_HSplitHCat'
  , prop_testSplitJ
  ]
