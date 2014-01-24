{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module Hascm.Server.PlotTypes
       ( Channel(..)
       , GraphInfo(..)
       , AxisScaling(..)
--       , XAxisType(..)
       ) where

import Control.Concurrent ( MVar, ThreadId )
import Control.Concurrent.STM.TChan
import Data.Time ( NominalDiffTime )

import Hascm.DirectCollocation.Dynamic ( DynPlotPoints, CollTrajMeta(..) )

--data XAxisType a = XAxisTime
--                 | XAxisCounter
--                 | XAxisStaticCounter
--                 | XAxisFun (String, a -> Double)

data AxisScaling = LogScaling
                 | LinearScaling

-- what the graph should draw
data GraphInfo =
  GraphInfo { giData :: MVar (Maybe (DynPlotPoints Double, CollTrajMeta, Int,NominalDiffTime))
            , giXScaling :: AxisScaling
            , giYScaling :: AxisScaling
            , giXRange :: Maybe (Double,Double)
            , giYRange :: Maybe (Double,Double)
            , giGetters :: [(String, DynPlotPoints Double -> [[(Double,Double)]])]
            }

data Channel =
  Channel { chanName :: String
          , chanTraj :: MVar (Maybe (DynPlotPoints Double, CollTrajMeta, Int, NominalDiffTime))
          , chanMeta :: TChan CollTrajMeta
          , chanServerThreadId :: ThreadId
          }
