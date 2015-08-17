{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Dyno/FormatTime.hs"
               ]
