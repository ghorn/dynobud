{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module ViewTests ( viewTests ) where

import GHC.Generics ( Generic )

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( assertEqual )

import Dyno.Vectorize
import Dyno.View
import Dyno.View.M

import VectorizeTests ( X(..), XCompound(..) )

data JX a = JX (J (JV X) a) (J (JV XCompound) a) deriving (Show, Generic, Generic1)
instance View JX
instance Scheme JX

data JX2 a = JX2 (J S a) (J (JTuple JX S) a) deriving (Show, Generic, Generic1)
instance View JX2
instance Scheme JX2

data M1 a = M1 (M JX JX2 a) deriving (Show, Generic, Generic1)
data M2 a = M2 (M JNone JNone a) deriving (Show, Generic, Generic1)
data M3 a = M3 (M JX2 JNone a) deriving (Show, Generic, Generic1)
data M4 a = M4 (M JNone JX2 a) deriving (Show, Generic, Generic1)

instance Scheme M1
instance Scheme M2
instance Scheme M3
instance Scheme M4

testSplit :: forall f g . (Vectorize f, Vectorize g) => String -> Proxy f -> Proxy g -> Test
testSplit msg _ _ =
  testGroup msg $ 
  map (testCase ("testSplits " ++ msg))
  [ assertEqual "test vertsplit" (show x0) (show x1)
  , assertEqual "test horzsplit" (show x0) (show x2)
  ]
  where
    x0 :: M (JV f) (JV g) MX
    x0 = ones

    x1 :: M (JV f) (JV g) MX
    x1 = vcat (vsplit x0)

    x2 :: M (JV f) (JV g) MX
    x2 = hcat (hsplit x0)

viewTests :: Test
viewTests =
  testGroup "view tests"
    [ testSplit "X/X" (Proxy :: Proxy X) (Proxy :: Proxy X)
    , testSplit "X/XCompound" (Proxy :: Proxy X) (Proxy :: Proxy XCompound)
    , testSplit "XCompound/X" (Proxy :: Proxy XCompound) (Proxy :: Proxy X)
    , testSplit "XCompound/XCompound" (Proxy :: Proxy XCompound) (Proxy :: Proxy XCompound)
    , testSplit "XCompound/None" (Proxy :: Proxy XCompound) (Proxy :: Proxy None)
    , testSplit "None/XCompound"  (Proxy :: Proxy None) (Proxy :: Proxy XCompound)
    , testSplit "None/None"  (Proxy :: Proxy None) (Proxy :: Proxy None)
    ]
