{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}

module ViewTests ( viewTests ) where

import GHC.Generics ( Generic )

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( assertEqual )

import Dyno.Vectorize
import Dyno.View
import Dyno.View.M

import VectorizeTests --( Vectorizes(..), vects )

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

views :: [Views]
views =
  [ Views ("JX2." ++ m1 ++ "." ++ m2 ++ "." ++ m3) (reproxy3 (Proxy :: Proxy JX2) p1 p2 p3)
  | Views m1 p1 <- jx1s
  , Views m2 p2 <- jx1s
  , Vectorizes m3 p3 <- vects
  ] ++ jx1s
  where
    jx0s :: [Views]
    jx0s =
      [ Views ("JX0." ++ m) (reproxy (Proxy :: Proxy JX0) p)
      | Vectorizes m p <- vects
      ] ++
      [ Views "JNone" (Proxy :: Proxy JNone)
      , Views "S" (Proxy :: Proxy S)
      ]

    jx1s :: [Views]
    jx1s =
      [ Views ("JX1." ++ m1 ++ "." ++ m2) (reproxy2 (Proxy :: Proxy JX1) p1 p2)
      | Vectorizes m1 p1 <- vects
      , Views m2 p2 <- jx0s
      ] ++ jx0s

    reproxy :: Proxy f -> Proxy g -> Proxy (f g)
    reproxy _ _ = Proxy

    reproxy2 :: Proxy f -> Proxy g -> Proxy h -> Proxy (f g h)
    reproxy2 _ _ _ = Proxy

    reproxy3 :: Proxy f -> Proxy g -> Proxy h -> Proxy j -> Proxy (f g h j)
    reproxy3 _ _ _ _ = Proxy


--data M1 a = M1 (M JX JX2 a) deriving (Show, Generic, Generic1)
--data M2 a = M2 (M JNone JNone a) deriving (Show, Generic, Generic1)
--data M3 a = M3 (M JX2 JNone a) deriving (Show, Generic, Generic1)
--data M4 a = M4 (M JNone JX2 a) deriving (Show, Generic, Generic1)

--instance Scheme M1
--instance Scheme M2
--instance Scheme M3
--instance Scheme M4

data Views where
  Views :: View f => String -> Proxy f -> Views
instance Show Views where
  show (Views s _) = s

testVSplit :: forall f g . (Vectorize f, View g) => String -> Proxy f -> Proxy g -> Test
testVSplit msg _ _ =
  testGroup msg $
  map (testCase ("testVSplit " ++ msg))
  [ assertEqual "test vertsplit" (show x0) (show x1)
  ]
  where
    x0 :: M (JV f) g MX
    x0 = ones

    x1 :: M (JV f) g MX
    x1 = vcat (vsplit x0)


testHSplit :: forall f g . (View f, Vectorize g) => String -> Proxy f -> Proxy g -> Test
testHSplit msg _ _ =
  testGroup msg $
  map (testCase ("testSplitsM " ++ msg))
  [ assertEqual "test horzsplit" (show x0) (show x1)
  ]
  where
    x0 :: M f (JV g) MX
    x0 = ones

    x1 :: M f (JV g) MX
    x1 = hcat (hsplit x0)

testSplitJ :: forall f . Vectorize f => String -> Proxy f -> Test
testSplitJ msg _ =
  testCase ("testSplitsJ " ++ msg) $
  assertEqual ("test vertsplit" ++ msg) (show xj0) (show xj2)
  where
    UnsafeM xm0 = ones :: M (JV f) (JV Id) MX

    xj0 :: J (JV f) MX
    xj0 = mkJ xm0

    xj1 :: JV f MX
    xj1 = split xj0

    xj2 :: J (JV f) MX
    xj2 = cat xj1

jTests :: Test
jTests =
  testGroup "J tests"
  [ testGroup "testSplitJ"
    [ testSplitJ m p
    | Vectorizes m p <- vects
    ]
  ]

mTests :: Test
mTests =
  testGroup "M tests"
  [ testGroup "testVSplit"
    [ testVSplit (m1 ++ "/" ++ m2) p1 p2
    | Vectorizes m1 p1 <- vects
    , Views m2 p2 <- views
    ]
  , testGroup "testHSplit"
    [ testHSplit (m1 ++ "/" ++ m2) p1 p2
    | Views m1 p1 <- views
    , Vectorizes m2 p2 <- vects
    ]
  ]

viewTests :: Test
viewTests =
  testGroup "view tests"
  [ jTests
  , mTests
  ]
