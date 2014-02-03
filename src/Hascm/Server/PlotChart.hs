{-# OPTIONS_GHC -Wall #-}

module Hascm.Server.PlotChart
       ( AxisScaling(..)
       , newChartCanvas
       ) where

import qualified Control.Concurrent as CC
import Control.Lens ( (.~) )
import Data.Default.Class ( def )
--import qualified Data.Foldable as F
--import qualified Data.Sequence as S
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Gtk as ChartGtk

import Hascm.Server.PlotTypes ( GraphInfo(..), AxisScaling(..) )

-- milliseconds for draw time
animationWaitTime :: Int
animationWaitTime = 33

newChartCanvas :: CC.MVar GraphInfo -> IO Gtk.DrawingArea
newChartCanvas graphInfoMVar = do
  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250
  _ <- Gtk.onExpose chartCanvas $ const (updateCanvas graphInfoMVar chartCanvas)
  _ <- Gtk.timeoutAddFull
       (Gtk.widgetQueueDraw chartCanvas >> return True)
       Gtk.priorityDefaultIdle animationWaitTime
  return chartCanvas

updateCanvas :: CC.MVar GraphInfo -> Gtk.DrawingArea -> IO Bool
updateCanvas graphInfoMVar canvas = do
  gi <- CC.readMVar graphInfoMVar
  maybeData <- CC.readMVar (giData gi)
  let namePcs = case maybeData of
        Nothing -> []
        -- convert to list of (name,S.Seq PbPrim)
        Just (datalog,_,_,_) -> map f (giGetters gi) :: [(String, [[(Double,Double)]])]
          where
            f (name,getter) = (name, getter datalog :: [[(Double,Double)]])

  let myGraph = displayChart (giXScaling gi, giYScaling gi) (giXRange gi, giYRange gi) namePcs
  ChartGtk.updateCanvas myGraph canvas

displayChart :: (Chart.PlotValue a, Show a, RealFloat a) =>
                (AxisScaling, AxisScaling) -> (Maybe (a,a),Maybe (a,a)) ->
                [(String, [[(a,a)]])] -> Chart.Renderable ()
displayChart (xScaling,yScaling) (xRange,yRange) namePcs = Chart.toRenderable layout
  where
    drawOne (name,pc) col
      = Chart.plot_lines_values .~ pc
        $ Chart.plot_lines_style  . Chart.line_color .~ col
--        $ Chart.plot_points_style ~. Chart.filledCircles 2 red
        $ Chart.plot_lines_title .~ name
        $ def
    allLines = zipWith drawOne namePcs Chart.defaultColorSeq

    xscaleFun = case xScaling of
      LogScaling -> Chart.layout_x_axis . Chart.laxis_generate .~ Chart.autoScaledLogAxis def
      LinearScaling -> case xRange of
        Nothing -> id
        Just range -> Chart.layout_x_axis . Chart.laxis_generate .~ Chart.scaledAxis def range

    yscaleFun = case yScaling of
      LogScaling -> Chart.layout_y_axis . Chart.laxis_generate .~ Chart.autoScaledLogAxis def
      LinearScaling -> case yRange of
        Nothing -> id
        Just range -> Chart.layout_y_axis . Chart.laxis_generate .~ Chart.scaledAxis def range

    layout = Chart.layout_plots .~ map Chart.toPlot allLines
--             $ Chart.layout_title .~ "Wooo, Party Graph!"
             $ Chart.layout_x_axis . Chart.laxis_title .~ "time [s]"
             $ xscaleFun
             $ yscaleFun
             def
