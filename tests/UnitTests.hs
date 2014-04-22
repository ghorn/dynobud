{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Test.Framework

import DummyTest ( dummyTests )

main :: IO ()
main = defaultMain [ testGroup "the dummy tests" dummyTests
                   ]
