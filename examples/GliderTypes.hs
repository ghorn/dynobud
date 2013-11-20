{-# OPTIONS_GHC -Wall #-}

module GliderTypes ( GliderDesignVars
                   , gliderPlotTree
                   , gliderChannelName
                   , gliderUrl
                   ) where

import Data.Tree ( Tree(..) )

import Hascm.Vectorize
import Hascm.TypeNats
import Hascm.DirectCollocation
import Hascm.Models.Aircraft

type NCollStages = D60
type CollDeg = D3

type GliderDesignVars a = CollTraj AcX None AcU None NCollStages CollDeg a

gliderPlotTree :: Tree (String, String, Maybe (PlotPointsL AcX None AcU Double -> [[(Double, Double)]]))
gliderPlotTree = toPlotTree

gliderChannelName :: String
gliderChannelName = "gliderchan"

gliderUrl :: String
gliderUrl = "tcp://127.0.0.1:5563"
