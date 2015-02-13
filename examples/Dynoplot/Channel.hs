{-# OPTIONS_GHC -Wall #-}

module Dynoplot.Channel
       ( dynoplotChannelName
       , dynoplotUrl
       ) where

dynoplotChannelName :: String
dynoplotChannelName = "dynoplot"

dynoplotUrl :: String
dynoplotUrl = "tcp://127.0.0.1:5563"
