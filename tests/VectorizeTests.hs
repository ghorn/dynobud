{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module VectorizeTests ( X(..), XCompound(..), vectorizeTests ) where

import GHC.Generics ( Generic )
import qualified Data.Vector as V
import Linear

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( Assertion, assertEqual )

import Dyno.Vectorize

data X a = X a (V3 a) a (V2 a) deriving (Show, Eq, Functor, Generic, Generic1)
data XCompound a = XCompound (X a) (V3 (X a)) a (V2 a) deriving (Show, Eq, Functor, Generic, Generic1)
instance Vectorize X
instance Vectorize XCompound

fillInc :: forall x . Vectorize x => x Int
fillInc = devectorize $ V.fromList $ take (vlength (Proxy :: Proxy x)) [0..]

vectorizeThenDevectorize :: forall x . (Show (x Int), Vectorize x) => Proxy x -> Assertion
vectorizeThenDevectorize _ = assertEqual "vectorizeThenDevectorize" (show x0) (show x1)
  where
    x0 :: x Int
    x0 = fillInc

    x1 :: x Int
    x1 = devectorize (vectorize x0)


vectorizeTests :: Test
vectorizeTests =
  testGroup "view tests"
    [ testCase "X a" (vectorizeThenDevectorize (Proxy :: Proxy X))
    , testCase "XCompound a" (vectorizeThenDevectorize (Proxy :: Proxy XCompound))
    ]
