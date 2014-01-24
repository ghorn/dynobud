{-# OPTIONS_GHC -Wall #-}

module Hascm.Server.GraphWidget
       ( newGraph
       ) where

import qualified Control.Concurrent as CC
import Control.Monad ( unless )
import Data.Maybe ( isJust, fromJust )
import qualified Data.Tree as Tree
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
--import Data.Sequence ( Seq )
import Data.Time ( NominalDiffTime )
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )

import Hascm.Server.PlotChart ( GraphInfo(..), AxisScaling(..), newChartCanvas )
import Hascm.DirectCollocation.Dynamic ( DynPlotPoints, CollTrajMeta(..), forestFromMeta )


data ListViewInfo a = ListViewInfo { lviName :: String
                                   , lviType :: String
                                   , lviGetter :: Maybe (a -> [[(Double,Double)]])
                                   , lviMarked :: Bool
                                   }

-- make a new graph window
newGraph ::
  String ->
  CC.MVar (Maybe (DynPlotPoints Double, CollTrajMeta, Int, NominalDiffTime)) ->
  IO Gtk.Window
newGraph channame chanseq = do
  win <- Gtk.windowNew

  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := channame
                   ]

  -- mvar with everything the graphs need to plot
  graphInfoMVar <- CC.newMVar GraphInfo { giData = chanseq
                                        , giXScaling = LinearScaling
                                        , giYScaling = LinearScaling
                                        , giXRange = Nothing
                                        , giYRange = Nothing
                                        , giGetters = []
                                        }

  -- the options widget
  optionsWidget <- makeOptionsWidget graphInfoMVar
  options <- Gtk.expanderNew "options"
  Gtk.set options [ Gtk.containerChild := optionsWidget
                  , Gtk.expanderExpanded := True
                  ]


  -- the signal selector
  (treeview', treeViewModel) <- newSignalSelectorArea graphInfoMVar
  treeview <- Gtk.expanderNew "signals"
  Gtk.set treeview [ Gtk.containerChild := treeview'
                   , Gtk.expanderExpanded := True
                   ]

  -- the treeview rebuilder button
  buttonRebuild <- Gtk.buttonNewWithLabel "rebuild tree"
  _ <- Gtk.onClicked buttonRebuild $ do
    maybeGI <- CC.readMVar chanseq
    case maybeGI of
      Nothing -> putStrLn "no signal tree to build here"
      Just (_,meta,_,_) -> do
        Gtk.treeStoreClear treeViewModel
        let mkTreeNode (name,typeName,maybeget) = ListViewInfo name typeName maybeget False
        let newTrees :: [Tree.Tree (ListViewInfo (DynPlotPoints Double))]
            newTrees = map (fmap mkTreeNode) (forestFromMeta meta)
        Gtk.treeStoreInsertForest treeViewModel [] 0 newTrees

  -- options and signal selector packed in vbox
  vboxOptionsAndSignals <- Gtk.vBoxNew False 4
  Gtk.set vboxOptionsAndSignals
    [ Gtk.containerChild := options
    , Gtk.boxChildPacking options := Gtk.PackNatural
    , Gtk.containerChild := buttonRebuild
    , Gtk.boxChildPacking buttonRebuild := Gtk.PackNatural
    , Gtk.containerChild := treeview
    , Gtk.boxChildPacking treeview := Gtk.PackGrow
    ]

  -- chart drawing area
  chartCanvas <- newChartCanvas graphInfoMVar

  -- hbox to hold eveything
  hboxEverything <- Gtk.hBoxNew False 4
  Gtk.set hboxEverything
    [ Gtk.containerChild := vboxOptionsAndSignals
    , Gtk.boxChildPacking vboxOptionsAndSignals := Gtk.PackNatural
    , Gtk.containerChild := chartCanvas
    ]
  _ <- Gtk.set win [ Gtk.containerChild := hboxEverything ]

  Gtk.widgetShowAll win
  return win



newSignalSelectorArea ::
  CC.MVar GraphInfo ->
  IO (Gtk.ScrolledWindow, Gtk.TreeStore (ListViewInfo (DynPlotPoints Double)))
newSignalSelectorArea graphInfoMVar = do
  model <- Gtk.treeStoreNew []
  treeview <- Gtk.treeViewNewWithModel model

  Gtk.treeViewSetHeadersVisible treeview True

  -- add some columns
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col1 "signal"
  Gtk.treeViewColumnSetTitle col2 "visible?"

  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True

  Gtk.cellLayoutSetAttributes col1 renderer1 model $
    \(ListViewInfo {lviName = name, lviType = typeName, lviGetter = getter}) ->
      [ Gtk.cellText := if isJust getter then name else name ++ " (" ++ typeName ++ ")"]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ \lvi -> [ Gtk.cellToggleActive := lviMarked lvi]

  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2


  let -- update the graph information
      updateGraphInfo = do
        lvis <- Gtk.treeStoreGetTree model [0]
        let newGetters = [(lviName lvi, fromJust $ lviGetter lvi) | lvi <- Tree.flatten lvis, lviMarked lvi, isJust (lviGetter lvi)]

        _ <- CC.modifyMVar_ graphInfoMVar (\gi0 -> return $ gi0 { giGetters = newGetters })
        return ()

  -- update which y axes are visible
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    -- toggle the check mark
    let treePath = Gtk.stringToTreePath pathStr
        g lvi@(ListViewInfo _ _ Nothing _) = putStrLn "yeah, that's not gonna work" >> return lvi
        g lvi = return $ lvi {lviMarked = not (lviMarked lvi)}
    ret <- Gtk.treeStoreChangeM model treePath g
    unless ret $ putStrLn "treeStoreChange fail"
    updateGraphInfo

  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.containerAdd scroll treeview
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]
  return (scroll, model)



makeOptionsWidget :: CC.MVar GraphInfo -> IO Gtk.VBox
makeOptionsWidget graphInfoMVar = do
  -- user selectable range
  xRange <- Gtk.entryNew
  yRange <- Gtk.entryNew
  Gtk.set xRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  Gtk.set yRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  xRangeBox <- labeledWidget "x range:" xRange
  yRangeBox <- labeledWidget "y range:" yRange
  Gtk.set xRange [Gtk.entryText := "(-10,10)"]
  Gtk.set yRange [Gtk.entryText := "(-10,10)"]
  let updateXRange = do
        Gtk.set xRange [ Gtk.entryEditable := True
                       , Gtk.widgetSensitive := True
                       ]
        txt <- Gtk.get xRange Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid x range entry: " ++ txt
            Gtk.set xRange [Gtk.entryText := "(min,max)"]
          Just (z0,z1) -> if z0 >= z1
                    then do
                      putStrLn $ "invalid x range entry (min >= max): " ++ txt
                      Gtk.set xRange [Gtk.entryText := "(min,max)"]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giXRange = Just (z0,z1)})
                      return ()
  let updateYRange = do
        Gtk.set yRange [ Gtk.entryEditable := True
                       , Gtk.widgetSensitive := True
                       ]
        txt <- Gtk.get yRange Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid y range entry: " ++ txt
            Gtk.set yRange [Gtk.entryText := "(min,max)"]
          Just (z0,z1) -> if z0 >= z1
                    then do
                      putStrLn $ "invalid y range entry (min >= max): " ++ txt
                      Gtk.set yRange [Gtk.entryText := "(min,max)"]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giYRange = Just (z0,z1)})
                      return ()
  _ <- on xRange Gtk.entryActivate updateXRange
  _ <- on yRange Gtk.entryActivate updateYRange

  -- linear or log scaling on the x and y axis?
  xScalingSelector <- Gtk.comboBoxNewText
  yScalingSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xScalingSelector)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  mapM_ (Gtk.comboBoxAppendText yScalingSelector)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  Gtk.comboBoxSetActive xScalingSelector 0
  Gtk.comboBoxSetActive yScalingSelector 0
  xScalingBox <- labeledWidget "x scaling:" xScalingSelector
  yScalingBox <- labeledWidget "y scaling:" yScalingSelector
  let updateXScaling = do
        k <- Gtk.comboBoxGetActive xScalingSelector
        _ <- case k of
          0 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LinearScaling, giXRange = Nothing}
          1 -> do
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LinearScaling, giXRange = Nothing}
            updateXRange
          2 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LogScaling, giXRange = Nothing}
          _ -> error "the \"impossible\" happened: x scaling comboBox index should be < 3"
        return ()
  let updateYScaling = do
        k <- Gtk.comboBoxGetActive yScalingSelector
        _ <- case k of
          0 -> do
            Gtk.set yRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LinearScaling, giYRange = Nothing}
          1 -> do
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LinearScaling, giYRange = Nothing}
            updateYRange
          2 -> do
            Gtk.set yRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LogScaling, giYRange = Nothing}
          _ -> error "the \"impossible\" happened: y scaling comboBox index should be < 3"
        return ()
  updateXScaling
  updateYScaling
  _ <- on xScalingSelector Gtk.changed updateXScaling
  _ <- on yScalingSelector Gtk.changed updateYScaling

  -- vbox to hold the little window on the left
  vbox <- Gtk.vBoxNew False 4

  Gtk.set vbox [ Gtk.containerChild := xScalingBox
               , Gtk.boxChildPacking   xScalingBox := Gtk.PackNatural
               , Gtk.containerChild := xRangeBox
               , Gtk.boxChildPacking   xRangeBox := Gtk.PackNatural
               , Gtk.containerChild := yScalingBox
               , Gtk.boxChildPacking   yScalingBox := Gtk.PackNatural
               , Gtk.containerChild := yRangeBox
               , Gtk.boxChildPacking   yRangeBox := Gtk.PackNatural
               ]

  return vbox



-- helper to make an hbox with a label
labeledWidget :: Gtk.WidgetClass a => String -> a -> IO Gtk.HBox
labeledWidget name widget = do
  label <- Gtk.labelNew (Just name)
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := label
               , Gtk.containerChild := widget
               , Gtk.boxChildPacking label := Gtk.PackNatural
--               , Gtk.boxChildPacking widget := Gtk.PackNatural
               ]
  return hbox
