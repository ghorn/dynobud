{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module VectorizeTests ( Vectorizes(..), vectorizeTests, reifyVects, vects) where

import GHC.Generics ( Generic )
import qualified Data.Vector as V
import Linear

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
--import Test.HUnit ( Assertion, assertEqual )
import Test.HUnit ( assertEqual )

import Dyno.Vectorize

data X a = X a (V3 a) a (V2 a) deriving (Show, Eq, Functor, Generic, Generic1)
data XCompound a = XCompound (X a) (V3 (X a)) a (V2 a) deriving (Show, Eq, Functor, Generic, Generic1)
instance Vectorize X
instance Vectorize XCompound

data Vectorizes where
  Vectorizes :: (Show (f Int), Vectorize f) => String -> Proxy f -> Vectorizes

vects :: [Vectorizes]
vects =
  [ Vectorizes "X" (Proxy :: Proxy X)
  , Vectorizes "XCompound" (Proxy :: Proxy XCompound)
  , Vectorizes "Id" (Proxy :: Proxy Id)
  , Vectorizes "None" (Proxy :: Proxy None)
  ]

reifyVects :: (forall f . (Show (f Int), Vectorize f) => String -> Proxy f -> r) -> Vectorizes -> r
reifyVects f (Vectorizes x y) = f x y

fillInc :: forall x . Vectorize x => x Int
fillInc = devectorize $ V.fromList $ take (vlength (Proxy :: Proxy x)) [0..]

vectorizeThenDevectorize :: forall x . (Show (x Int), Vectorize x) => String -> Proxy x -> Test
vectorizeThenDevectorize msg _ =
  testCase ("vectorizeThenDevectorize " ++ msg) $
  assertEqual ("vectorizeThenDevectorize " ++ msg)  (show x0) (show x1)
  where
    x0 :: x Int
    x0 = fillInc

    x1 :: x Int
    x1 = devectorize (vectorize x0)


vectorizeTests :: Test
vectorizeTests =
  testGroup "view tests" $
  map (reifyVects vectorizeThenDevectorize) vects
