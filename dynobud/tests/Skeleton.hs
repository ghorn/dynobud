{-# OPTIONS_GHC -Wall #-}

module Skeleton ( main, runManually ) where

import Test.QuickCheck ( quickCheck )
--import Test.QuickCheck ( Args(..), quickCheckWith, stdArgs )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework ( Test, defaultMain, testGroup )
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

-- dummy "QP" that is just an Int
newtype Qp = Qp Int deriving Show
newtype PositiveQp = PositiveQp Qp deriving Show
newtype NegativeQp = NegativeQp Qp deriving Show

-- make it always >= 0
instance Arbitrary PositiveQp where
  arbitrary = do
    k <- arbitrary
    return $ PositiveQp $ Qp $ abs k

-- make it always <= 0
instance Arbitrary NegativeQp where
  arbitrary = do
    k' <- arbitrary
    let k = if k' <= 0 then k' else -k'
    return $ NegativeQp $ Qp $ -(abs k)

-- this is like "isSolveable"
isPositive :: Qp -> Bool
isPositive (Qp k) = k >= 0

-- this tests that a feasible qp is solvable
positiveQpIsPositive :: PositiveQp -> Bool
positiveQpIsPositive (PositiveQp qp) = isPositive qp

-- this tests that an infeasible qp is not solvable
negativeQpIsNotPositive :: NegativeQp -> Bool
negativeQpIsNotPositive (NegativeQp qp) = not (isPositive qp)

-- IO version
positiveQpIsPositiveIO :: PositiveQp -> Property
positiveQpIsPositiveIO (PositiveQp qp) = monadicIO $ do
  if isPositive qp
    then stop (succeeded {reason = "success"})
    else stop (failed {reason = "is not positive"})

-- IO version
negativeQpIsNotPositiveIO :: NegativeQp -> Property
negativeQpIsNotPositiveIO (NegativeQp qp) = monadicIO $ do
  if isPositive qp
    then stop (failed {reason = "is positive"})
    else stop (succeeded {reason = "success"})

-- a group of tests
allTests :: [Test]
allTests = [ testGroup "pure tests" [testProperty "pos is pos" positiveQpIsPositive
                                    , testProperty "neg is not pos" negativeQpIsNotPositive
                                    ]
           , testGroup "io tests" [ testProperty "pos is pos (IO)" positiveQpIsPositiveIO
                                  , testProperty "neg is not pos (IO)" negativeQpIsNotPositiveIO
                                  ]
           ]


-- this runs quickcheck manually
runManually :: IO ()
runManually = do
  quickCheck positiveQpIsPositive
  quickCheck negativeQpIsNotPositive
  quickCheck positiveQpIsPositiveIO
  quickCheck negativeQpIsNotPositiveIO

-- this uses test-framework to run all the tests
main :: IO ()
main = defaultMain allTests
