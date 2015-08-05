{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DummyTest ( dummyTests ) where

--import Test.QuickCheck ( quickCheck )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test )
import Test.HUnit ( Assertion, assertEqual )

-- dummy wrapper around an Int
newtype MyInt = MyInt Int deriving (Eq, Show, Num)

-- use the Int arbitrary methods to define MyInt methods
instance Arbitrary MyInt where
  arbitrary = do
    k <- arbitrary
    return (MyInt k)
  shrink (MyInt k) = map MyInt (shrink k)

addingMakesBigger :: MyInt -> Bool
addingMakesBigger (MyInt k) = k + 1 > k

twoPlusTwo :: Assertion
twoPlusTwo = assertEqual "2+2=4" (MyInt 2 + MyInt 2) (MyInt 4)

showInstance :: Assertion
showInstance = assertEqual "show instance" "MyInt 42" (show (MyInt 42))

dummyTests :: [Test]
dummyTests = [ testCase "2 + 2 == 4" twoPlusTwo
             , testCase "show instance" showInstance
             , testProperty "adding makes it bigger" addingMakesBigger
             ]

-- comment this in to run a test
--main :: IO ()
--main = quickCheck addingMakesBigger
