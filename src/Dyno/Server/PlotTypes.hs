{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module Dyno.Server.PlotTypes
       ( Channel(..)
       , GraphInfo(..)
       , ListViewInfo(..)
       , AxisScaling(..)
       , MetaTree
--       , XAxisType(..)
       ) where

import Control.Concurrent ( MVar, ThreadId )
import Data.Time ( NominalDiffTime )
import qualified Graphics.UI.Gtk as Gtk

import Dyno.DirectCollocation.Dynamic ( DynPlotPoints, MetaTree )

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
  GraphInfo { giData :: MVar (Maybe (DynPlotPoints Double, Int, NominalDiffTime))
            , giXScaling :: AxisScaling
            , giYScaling :: AxisScaling
            , giXRange :: Maybe (Double,Double)
            , giYRange :: Maybe (Double,Double)
            , giGetters :: [(String, DynPlotPoints Double -> [[(Double,Double)]])]
            }

data Channel =
  Channel { chanName :: String
          , chanTraj :: MVar (Maybe (DynPlotPoints Double, Int, NominalDiffTime))
          , chanMetaStore :: Gtk.ListStore (MetaTree Double)
          , chanServerThreadId :: ThreadId
          }
