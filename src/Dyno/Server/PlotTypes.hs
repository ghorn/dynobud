{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module Dyno.Server.PlotTypes
       ( Channel(..)
       , Message(..)
       , GraphInfo(..)
       , ListViewInfo(..)
       , AxisScaling(..)
       , MetaTree
--       , XAxisType(..)
       ) where

import Data.Time ( NominalDiffTime )
import qualified Graphics.UI.Gtk as Gtk

import Dyno.DirectCollocation.Dynamic ( DynPlotPoints, CollTrajMeta, MetaTree )

data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviType :: String
                                   , lviGetter :: Maybe (a -> [[(Double,Double)]])
                                   , lviMarked :: Bool
                                   }

--data XAxisType a = XAxisTime
--                 | XAxisCounter
--                 | XAxisStaticCounter
--                 | XAxisFun (String, a -> Double)

data AxisScaling = LogScaling
                 | LinearScaling

-- what the graph should draw
data GraphInfo =
  GraphInfo { giXScaling :: AxisScaling
            , giYScaling :: AxisScaling
            , giXRange :: Maybe (Double,Double)
            , giYRange :: Maybe (Double,Double)
            , giGetters :: [(String, DynPlotPoints Double -> [[(Double,Double)]])]
            }

data Message = Message (DynPlotPoints Double) Int NominalDiffTime CollTrajMeta
data Channel =
  Channel { chanName :: String
          , chanMsgStore :: Gtk.ListStore Message
          }
