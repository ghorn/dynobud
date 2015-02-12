{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module VectorizeTests
       ( Vectorizes(..)
       , Dims(..)
       , vectorizeTests
       ) where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Vector as V
import Linear
import Linear.V

import Test.QuickCheck
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Dyno.Vectorize
import Dyno.Nats
import qualified Dyno.TypeVecs as TV

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


data Dims where
  Dims :: Dim n =>
           { dShrinks :: [Dims]
           , dProxy :: Proxy n
           } -> Dims
instance Show Dims where
  show (Dims _ p) = "D" ++ show (reflectDim p)

instance Arbitrary Dims where
  arbitrary = elements [ d0, d1, d2, d3, d4, d10, d100 ]
    where
      d0   = Dims []                   (Proxy :: Proxy D0)
      d1   = Dims [d0]                 (Proxy :: Proxy D1)
      d2   = Dims [d0,d1]              (Proxy :: Proxy D2)
      d3   = Dims [d0,d1,d2]           (Proxy :: Proxy D3)
      d4   = Dims [d0,d1,d2,d3]        (Proxy :: Proxy D4)
      d10  = Dims [d0,d1,d2,d3,d4]     (Proxy :: Proxy D10)
      d100 = Dims [d0,d1,d2,d3,d4,d10] (Proxy :: Proxy D100)
  shrink = dShrinks

instance Show Vectorizes where
  show = vName

maxVSize :: Int
maxVSize = 1000

instance Arbitrary Vectorizes where
  arbitrary = do
    x <- oneof [primitives, compounds primitives, compounds (compounds primitives)]
    if vecSize x <= maxVSize then return x else arbitrary
  shrink = filter ((<= maxVSize) . vecSize) . shrink' True
    where
      shrink' True v = vShrinks v ++ concatMap (shrink' False) (vShrinks v)
      shrink' False v = vShrinks v

vecSize :: Vectorizes -> Int
vecSize (Vectorizes _ _ p) = vlength p

primitives :: Gen Vectorizes
primitives = do
  d <- arbitrary
  elements
    [ Vectorizes [] "None" (Proxy :: Proxy None)
    , Vectorizes [] "Id" (Proxy :: Proxy Id)
    , Vectorizes [] "V0" (Proxy :: Proxy V0)
    , Vectorizes [] "V1" (Proxy :: Proxy V1)
    , Vectorizes [] "V2" (Proxy :: Proxy V2)
    , Vectorizes [] "V3" (Proxy :: Proxy V3)
    , Vectorizes [] "V4" (Proxy :: Proxy V4)
    , Vectorizes [] "X0" (Proxy :: Proxy X0)
    , mkTypeVec True d
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

mkTypeVec :: Bool -> Dims -> Vectorizes
mkTypeVec shrinkThis d@(Dims _ pd) =
  Vectorizes
  { vShrinks = if shrinkThis then map (mkTypeVec False) (shrink d) else []
  , vName = "Vec " ++ show d
  , vProxy = reproxy (Proxy :: Proxy TV.Vec) pd
  }

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
