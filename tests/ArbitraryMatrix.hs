{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}

module ArbitraryMatrix where

import Linear.V ( Dim(..) )
import Data.Proxy ( Proxy(..) )
import Test.QuickCheck.Arbitrary ( Arbitrary(..), vector )
import Test.QuickCheck.Gen ( Gen(..), generate )
import Data.Packed.Matrix

import Dyno.Nats

data MyOde n m = MyOde { odeA :: Mat n n
                       , odeB :: Mat n m
                       } deriving Show

newtype Mat n m = Mat (Matrix Double) deriving Show

instance (Dim n, Dim m) => Arbitrary (Mat n m) where
  arbitrary :: Gen (Mat n m)
  arbitrary = do
    let n = reflectDim (Proxy :: Proxy n)
        m = reflectDim (Proxy :: Proxy m)
        
    elems <- vector (n*m)
    return $ Mat $ (n >< m) elems

instance (Dim n, Dim m) => Arbitrary (MyOde n m) where
  arbitrary :: Gen (MyOde n m)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (MyOde a b)

go :: IO ()
go = do
  randMat <- generate (arbitrary :: Gen (Mat D3 D4))
  print randMat
  putStrLn "\n\n"
  MyOde a b <- generate (arbitrary :: Gen (MyOde D2 D1))
  print a
  print b
