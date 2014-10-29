{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module VectorizeTests
       ( Vectorizes(..)
       , vectorizeTests
       ) where

import GHC.Generics ( Generic )
import qualified Data.Vector as V
import Linear

import Test.QuickCheck
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Dyno.Vectorize

import Utils


data X0 a = X0 a (V3 a) a (V2 a) deriving (Show, Eq, Functor, Generic, Generic1)
data X1 f g h a = X1 (f a) (V3 (g a)) a (V2 a) a (h a) deriving (Show, Eq, Functor, Generic, Generic1)


instance Vectorize X0
instance (Vectorize f, Vectorize g, Vectorize h) => Vectorize (X1 f g h)

data Vectorizes where
  Vectorizes ::
    (Show (f Int), Eq (f Int), Vectorize f)
    => { vShrinks :: [Vectorizes]
       , vName :: String
       , vProxy :: Proxy f } -> Vectorizes

instance Show Vectorizes where
  show = vName

instance Arbitrary Vectorizes where
  arbitrary = oneof [primitives, compounds primitives, compounds (compounds primitives)]
  shrink = vShrinks

primitives :: Gen Vectorizes
primitives =
  elements
  [ Vectorizes [] "None" (Proxy :: Proxy None)
  , Vectorizes [] "Id" (Proxy :: Proxy Id)
  , Vectorizes [] "V0" (Proxy :: Proxy V0)
  , Vectorizes [] "V1" (Proxy :: Proxy V1)
  , Vectorizes [] "V2" (Proxy :: Proxy V2)
  , Vectorizes [] "V3" (Proxy :: Proxy V3)
  , Vectorizes [] "V4" (Proxy :: Proxy V4)
  , Vectorizes [] "X0" (Proxy :: Proxy X0)
  ]

compounds :: Gen Vectorizes -> Gen Vectorizes
compounds genIt = do
  v1@(Vectorizes _ m1 p1) <- genIt
  v2@(Vectorizes _ m2 p2) <- genIt
  v3@(Vectorizes _ m3 p3) <- genIt
  elements
    [ Vectorizes
      { vShrinks = [v1, v2]
      , vName = "Tuple (" ++ m1 ++ ") (" ++ m2 ++ ")"
      , vProxy = reproxy2 (Proxy :: Proxy Tuple) p1 p2
      }
    , Vectorizes
      { vShrinks = [v1, v2, v3]
      , vName = "Triple (" ++ m1 ++ ") (" ++ m2 ++ ") (" ++ m3 ++ ")"
      , vProxy = reproxy3 (Proxy :: Proxy Triple) p1 p2 p3
      }
    , Vectorizes
      { vShrinks = [v1, v2, v3]
      , vName = "X1 (" ++ m1 ++ ") (" ++ m2 ++ ") " ++ m3 ++ ")"
      , vProxy = reproxy3 (Proxy :: Proxy X1) p1 p2 p3
      }
    ]

fillInc :: forall x . Vectorize x => x Int
fillInc = devectorize $ V.fromList $ take (vlength (Proxy :: Proxy x)) [0..]

vectorizeThenDevectorize ::
  forall x
  . (Show (x Int), Eq (x Int), Vectorize x)
  => Proxy x -> Bool
vectorizeThenDevectorize _ = x0 == x1
  where
    x0 :: x Int
    x0 = fillInc

    x1 :: x Int
    x1 = devectorize (vectorize x0)

prop_vecThenDevec :: Vectorizes -> Bool
prop_vecThenDevec (Vectorizes _ _ p) = vectorizeThenDevectorize p


vectorizeTests :: Test
vectorizeTests =
  testGroup "vectorize tests"
  [ testProperty "vec . devec" prop_vecThenDevec
  ]
