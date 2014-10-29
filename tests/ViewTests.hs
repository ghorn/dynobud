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

import Dyno.Vectorize
import Dyno.View
import Dyno.View.M

import Utils
import VectorizeTests ( Vectorizes(..) )

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

instance Arbitrary Views where
  arbitrary = frequency
              [ (1, compound)
              , (2, primitives)
              ]

compound :: Gen Views
compound = do
  vc'@(Vectorizes _ mz pz) <- arbitrary
  let vc = mkJV vc'
  vw0@(Views _ mv0 pv0) <- arbitrary
  vw1@(Views _ mv1 pv1) <- arbitrary
  elements
    [ Views [vc] ("JX0 (" ++ mz ++ ")") (reproxy (Proxy :: Proxy JX0) pz)
    , Views [vc,vw0] ("JX1 (" ++ mz ++ ") (" ++ mv0 ++ ")") (reproxy2 (Proxy :: Proxy JX1) pz pv0)
    , Views [vc, vw0, vw1] ("JX2 (" ++ mv0 ++ ") (" ++ mv1 ++ ") (" ++ mz ++ ")")
      (reproxy3 (Proxy :: Proxy JX2) pv0 pv1 pz)
    ]

mkJV :: Vectorizes -> Views
mkJV (Vectorizes vs m p) = Views shrinks ("JV (" ++ m ++ ")") (reproxyJV p)
  where
    shrinks :: [Views]
    shrinks = map mkJV (concatMap vShrinks vs)

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

prop_VSplitVCat :: Vectorizes -> Views -> Bool
prop_VSplitVCat (Vectorizes _ _ p1) (Views _ _ p2) = test p1 p2
  where
    test :: forall f g . (Vectorize f, View g) => Proxy f -> Proxy g -> Bool
    test _ _ = show x0 == show x1
      where
        x0 :: M (JV f) g MX
        x0 = ones

        x1 :: M (JV f) g MX
        x1 = vcat (vsplit x0)

prop_HSplitHCat :: Views -> Vectorizes -> Bool
prop_HSplitHCat (Views _ _ p1) (Vectorizes _ _ p2) = test p1 p2
  where
    test :: forall f g . (View f, Vectorize g) => Proxy f -> Proxy g -> Bool
    test _ _ = show x0 == show x1
      where
        x0 :: M f (JV g) MX
        x0 = ones

        x1 :: M f (JV g) MX
        x1 = hcat (hsplit x0)

prop_testSplitJ :: Vectorizes -> Bool
prop_testSplitJ (Vectorizes _ _ p) = test p
  where
    test :: forall f . Vectorize f => Proxy f -> Bool
    test _ = show xj0 == show xj2
      where
        UnsafeM xm0 = ones :: M (JV f) (JV Id) MX

        xj0 :: J (JV f) MX
        xj0 = mkJ xm0

        xj1 :: JV f MX
        xj1 = split xj0

        xj2 :: J (JV f) MX
        xj2 = cat xj1


viewTests :: Test
viewTests =
  testGroup "view tests"
  [ testProperty "vcat . vsplit" prop_VSplitVCat
  , testProperty "hcat . hsplit" prop_HSplitHCat
  , testProperty "split . cat J" prop_testSplitJ
  ]
