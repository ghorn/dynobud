{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Data.Monoid ( mempty )
import Test.Framework ( Test, ColorMode(..), RunnerOptions'(..), TestOptions'(..)
                      , defaultMainWithOpts )

import VectorizeTests ( vectorizeTests )
import ViewTests ( viewTests )
import IntegrationTests ( integrationTests )

main :: IO ()
main = defaultMainWithOpts tests opts

tests :: [Test]
tests =
  [ integrationTests
  , vectorizeTests
  , viewTests
  ]

opts :: RunnerOptions' Maybe
opts = mempty { ropt_color_mode = Just ColorAlways
              , ropt_threads = Just 1
              , ropt_test_options = Just my_test_opts
              }

my_test_opts :: TestOptions' Maybe
my_test_opts = mempty { topt_timeout = Just (Just 2000000) }
