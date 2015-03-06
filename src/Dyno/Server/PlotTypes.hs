{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module Dyno.Server.PlotTypes
       ( Channel(..)
       , GraphInfo(..)
       , ListViewInfo(..)
       , AxisScaling(..)
       ) where

import Data.Tree ( Tree )
import qualified Graphics.UI.Gtk as Gtk
import Data.IORef ( IORef )

data ListViewInfo a =
  ListViewInfo
  { lviName :: String
  , lviType :: String
  , lviGetter :: Maybe (a -> [[(Double,Double)]])
  , lviMarked :: Bool
  }

data AxisScaling = LogScaling
                 | LinearScaling

-- what the graph should draw
data GraphInfo a =
  GraphInfo { giXScaling :: AxisScaling
            , giYScaling :: AxisScaling
            , giXRange :: Maybe (Double,Double)
            , giYRange :: Maybe (Double,Double)
            , giGetters :: [(String, a -> [[(Double,Double)]])]
            }

data Channel a =
  Channel { chanName :: String
          , chanMsgStore :: Gtk.ListStore a
          , chanSameSignalTree :: a -> a -> Bool
          , chanToSignalTree :: a -> [Tree ( String
                                           , String
                                           , Maybe (a -> [[(Double, Double)]])
                                           )]
          , chanMaxHistory :: IORef Int
          }
