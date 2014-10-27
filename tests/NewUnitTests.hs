{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Data.Monoid ( mempty )
import Test.Framework ( ColorMode(..), RunnerOptions'(..), defaultMainWithOpts )

import VectorizeTests ( vectorizeTests )
import ViewTests ( viewTests )

main :: IO ()
main = do
  defaultMainWithOpts
    [ vectorizeTests
    , viewTests
    ]
    mempty { ropt_color_mode = Just ColorAlways }
