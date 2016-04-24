{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Monoid as Mo
import Test.Framework
       ( Test, ColorMode(..), RunnerOptions'(..), TestOptions'(..)
       , defaultMainWithOpts )

import QuadratureTests ( quadratureTests )
import VectorizeTests ( vectorizeTests )
import ViewTests ( viewTests )
import IntegrationTests ( integrationTests )
import MapTests ( mapTests )
import FittingTests ( fittingTests )
import LinearizeTests ( linearizeTests )
import FunctionTests ( functionTests )

main :: IO ()
main = defaultMainWithOpts tests opts

tests :: [Test]
tests =
  [ fittingTests
  , functionTests
  , integrationTests
  , linearizeTests
  , mapTests
  , quadratureTests
  , vectorizeTests
  , viewTests
  ]

opts :: RunnerOptions' Maybe
opts =
  Mo.mempty
  { ropt_color_mode = Just ColorAlways
  , ropt_threads = Just 1
  , ropt_test_options = Just my_test_opts
  }

my_test_opts :: TestOptions' Maybe
my_test_opts =
  Mo.mempty
  { topt_timeout = Just (Just 15000000)
  }
