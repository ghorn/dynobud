{-# OPTIONS_GHC -Wall #-}

module GliderTypes ( GliderDesignVars
                   , gliderChannelName
                   , gliderUrl
                   ) where

import Hascm.Vectorize
import Hascm.Nats
import Hascm.DirectCollocation
import Hascm.Models.Aircraft

type NCollStages = D100
type CollDeg = D2

type GliderDesignVars a = CollTraj AcX None AcU None NCollStages CollDeg a

gliderChannelName :: String
gliderChannelName = "gliderchan"

gliderUrl :: String
gliderUrl = "tcp://127.0.0.1:5563"
