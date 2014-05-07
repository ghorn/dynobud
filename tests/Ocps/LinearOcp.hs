{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}

module Ocps.LinearOcp
       ( LinearOcp(..), IsLinearOcp(..)
       , FeasibleLinearOcp(..), InfeasibleLinearOcp(..)
       , runGenWithSeed
       ) where

import qualified Data.Foldable as F
import Linear.V ( Dim(..), reifyDim )
import Data.Proxy ( Proxy(..) )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen ( Gen(..), choose, vectorOf )
import Test.QuickCheck.Random
import Data.Packed.Matrix
import Data.MemoTrie ( memo2 )

import Numeric.LinearAlgebra

import Dyno.TypeVecs hiding ( reifyDim )
--import Dyno.Nats

reasonableLimit :: (Double, Double)
reasonableLimit = (-1e3, 1e3)

-- real OCP dx/dt = A x + B u
data LinearOcp n m =
  LinearOcp
  { loA :: Vec n (Vec n Double) -- A
  , loB :: Vec n (Vec m Double) -- B
  , loX0 :: Vec n Double        -- Starting point
  , loXF :: Vec n Double        -- Ending point
  } deriving (Show, Eq)
data FeasibleLinearOcp n m  = FeasibleLinearOcp (LinearOcp n m) deriving Show
data InfeasibleLinearOcp n m  = InfeasibleLinearOcp (LinearOcp n m) deriving Show

class IsLinearOcp a n m | a -> n, a -> m where
  getLinearOcp :: a -> LinearOcp n m
instance IsLinearOcp (LinearOcp n m) n m where
  getLinearOcp = id
instance IsLinearOcp (FeasibleLinearOcp n m) n m where
  getLinearOcp (FeasibleLinearOcp ocp) = ocp
instance IsLinearOcp (InfeasibleLinearOcp n m) n m where
  getLinearOcp (InfeasibleLinearOcp ocp) = ocp

genVec :: forall n a . Dim n => Gen a -> Gen (Vec n a)
genVec gen = do
  let n = reflectDim (Proxy :: Proxy n)
  xs <- vectorOf n gen
  return (mkVec' xs)

genVecs :: forall n m a . (Dim n, Dim m) => Gen a -> Gen (Vec n (Vec m a))
genVecs gen = genVec (genVec gen)

createLinearOcpWithProb :: forall n m . (Dim n, Dim m) => Double -> Gen (LinearOcp n m)
createLinearOcpWithProb prob = do
  let n = reflectDim (Proxy :: Proxy n)
  as <- genVecs (sparseDouble prob)
  bs <- genVecs (sparseDouble prob)
  x0 <- vectorOf n (choose reasonableLimit)
  xF <- vectorOf n (choose reasonableLimit)
  return $ LinearOcp as bs (mkVec' x0) (mkVec' xF)

instance (Dim n, Dim m) => Arbitrary (FeasibleLinearOcp n m) where
  arbitrary = do
    let prob = bestProbability (Proxy :: Proxy n) (Proxy :: Proxy m)
    ocp <- createLinearOcpWithProb prob
    if definitelyControllable ocp
      then return (FeasibleLinearOcp ocp)
      else arbitrary

instance (Dim n, Dim m) => Arbitrary (InfeasibleLinearOcp n m) where
  arbitrary = do
    let prob = bestProbability (Proxy :: Proxy n) (Proxy :: Proxy m)
    ocp <- createLinearOcpWithProb prob
    if definitelyUncontrollable ocp
      then return (InfeasibleLinearOcp ocp)
      else arbitrary

-- full rank and well conditioned
definitelyControllable :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Bool
definitelyControllable ocp = rank bab == n && rcond bab >= 1e-11
  where
    n = reflectDim (Proxy :: Proxy n)
    bab = createBAB ocp

-- not full rank
definitelyUncontrollable :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Bool
definitelyUncontrollable ocp = rank bab < n
  where
    n = reflectDim (Proxy :: Proxy n)
    bab = createBAB ocp

createBAB :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Matrix Double
createBAB ocp = bab
  where
    ma = concat $ fmap F.toList $ F.toList $ loA ocp
    mb = concat $ fmap F.toList $ F.toList $ loB ocp
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    bab = createBAB' n ((n><n) ma) ((n><m) mb)

createBAB' :: Int -> Matrix Double -> Matrix Double -> Matrix Double
createBAB' 1  _ matb = matb
createBAB' nn mata matb = matAB
  where
    matAAB = createBAB' (nn-1) mata (mata <> matb)
    matAB = fromLists $ zipWith (++) (toLists matb) (toLists matAAB)


-- create a double which is non-zero with some given probability
sparseDouble :: Double -> Gen Double
sparseDouble prob = do
  testProb <- choose (0,1) :: Gen Double
  if testProb <= prob
    then do
      nz <- choose reasonableLimit :: Gen Double
      if abs nz >= 1e-12
        then return nz
        else sparseDouble prob
    else return 0

getProb'' :: forall n m . (Dim n, Dim m) => Double -> Int -> Proxy n -> Proxy m -> Gen Double
getProb'' prob numRuns pn pm = do
  let nextProb
        | prob >= 1.0 = 1.0
        | otherwise = prob + 0.01
  ocps <- vectorOf numRuns (createLinearOcpWithProb prob) :: Gen [LinearOcp n m]
  let fractionControllable = (fromIntegral (length (filter definitelyControllable ocps))) / (fromIntegral numRuns :: Double)
  if fractionControllable >= 0.5
    then return prob
    else getProb'' nextProb numRuns pn pm

getProb' :: Int -> Int -> Gen Double
getProb' n m = reifyDim m $ reifyDim n $ getProb'' 0.0 numRuns
  where
    numRuns = 100

getProb :: Int -> Int -> Double
getProb n m = runGenWithSeed 42 (getProb' n m)

bestProbability' :: Int -> Int -> Double
bestProbability' = memo2 getProb

bestProbability :: (Dim n, Dim m) => Proxy n -> Proxy m -> Double
bestProbability pn pm = bestProbability' (reflectDim pn) (reflectDim pm)

runGenWithSeed :: Int -> Gen a -> a
runGenWithSeed k gen = (unGen gen) (mkQCGen k) k
