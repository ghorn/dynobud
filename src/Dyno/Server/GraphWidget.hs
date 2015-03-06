{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Dyno.Server.GraphWidget
       ( newGraph
       ) where

import qualified Control.Concurrent as CC
import Control.Monad ( when, unless )
import qualified Data.IORef as IORef
import Data.Maybe ( isJust, fromJust )
import qualified Data.Tree as Tree
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )
import qualified Data.Text as T
import qualified Graphics.Rendering.Chart as Chart

import Dyno.Server.PlotChart ( AxisScaling(..), displayChart, chartGtkUpdateCanvas )
import Dyno.Server.PlotTypes ( GraphInfo(..), ListViewInfo(..), Message(..) )

-- make a new graph window
newGraph ::
  forall a
  . String
  -> (a -> a -> Bool)
  -> (a -> [Tree.Tree (String, String, Maybe (a -> [[(Double, Double)]]))])
  -> Gtk.ListStore (Message a) -> IO Gtk.Window
newGraph channame sameSignalTree forestFromMeta msgStore = do
  win <- Gtk.windowNew

  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := channame
                   ]

  -- mvar with all the user input
  graphInfoMVar <- CC.newMVar GraphInfo { giXScaling = LinearScaling
                                        , giYScaling = LinearScaling
                                        , giXRange = Nothing
                                        , giYRange = Nothing
                                        , giGetters = []
                                        } :: IO (CC.MVar (GraphInfo a))

  let makeRenderable :: IO (Chart.Renderable ())
      makeRenderable = do
        gi <- CC.readMVar graphInfoMVar
        size <- Gtk.listStoreGetSize msgStore

        namePcs <- if size == 0
                   then return []
                   else do
                     Message datalog _ _ <- Gtk.listStoreGetValue msgStore 0
                     let ret :: [(String, [[(Double,Double)]])]
                         ret = map (fmap (\g -> g datalog)) (giGetters gi)
                     return ret
        return $ displayChart (giXScaling gi, giYScaling gi) (giXRange gi, giYRange gi) namePcs

  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250

  let redraw :: IO ()
      redraw = do
        renderable <- makeRenderable
        chartGtkUpdateCanvas renderable chartCanvas

  _ <- Gtk.onExpose chartCanvas $ const (redraw >> return True)


  -- the options widget
  optionsWidget <- makeOptionsWidget graphInfoMVar redraw
  options <- Gtk.expanderNew "options"
  Gtk.set options [ Gtk.containerChild := optionsWidget
                  , Gtk.expanderExpanded := False
                  ]


  -- the signal selector
  treeview' <- newSignalSelectorArea sameSignalTree forestFromMeta graphInfoMVar msgStore redraw
  treeview <- Gtk.expanderNew "signals"
  Gtk.set treeview [ Gtk.containerChild := treeview'
                   , Gtk.expanderExpanded := True
                   ]

  -- options and signal selector packed in vbox
  vboxOptionsAndSignals <- Gtk.vBoxNew False 4
  Gtk.set vboxOptionsAndSignals
    [ Gtk.containerChild := options
    , Gtk.boxChildPacking options := Gtk.PackNatural
    , Gtk.containerChild := treeview
    , Gtk.boxChildPacking treeview := Gtk.PackGrow
    ]

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
  forall a
  . (a -> a -> Bool)
  -> (a -> [Tree.Tree (String, String, Maybe (a -> [[(Double, Double)]]))])
  -> CC.MVar (GraphInfo a)
  -> Gtk.ListStore (Message a)
  -> IO () -> IO Gtk.ScrolledWindow
newSignalSelectorArea sameSignalTree forestFromMeta graphInfoMVar msgStore redraw = do
  treeStore <- Gtk.treeStoreNew []
  treeview <- Gtk.treeViewNewWithModel treeStore

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

  let showName (Just _) name _ = name
      showName Nothing name "" = name
      showName Nothing name typeName = name ++ " (" ++ typeName ++ ")"
  Gtk.cellLayoutSetAttributes col1 renderer1 treeStore $
    \(ListViewInfo {lviName = name, lviType = typeName, lviGetter = getter}) ->
      [ Gtk.cellText := showName getter name typeName]
  Gtk.cellLayoutSetAttributes col2 renderer2 treeStore $ \lvi -> [ Gtk.cellToggleActive := lviMarked lvi]

  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2


  let -- update the graph information
      updateGraphInfo = do
        -- first get all trees
        let getTrees k = do
              tree' <- Gtk.treeStoreLookup treeStore [k]
              case tree' of Nothing -> return []
                            Just tree -> fmap (tree:) (getTrees (k+1))
        theTrees <- getTrees 0
        let newGetters = [ (lviName lvi, fromJust $ lviGetter lvi)
                         | lvi <- concatMap Tree.flatten theTrees
                         , lviMarked lvi
                         , isJust (lviGetter lvi)
                         ]
        _ <- CC.modifyMVar_ graphInfoMVar (\gi0 -> return $ gi0 { giGetters = newGetters })
        return ()

  -- update which y axes are visible
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    let treePath = Gtk.stringToTreePath pathStr
    -- toggle the check mark
    let g lvi@(ListViewInfo _ _ Nothing _) = lvi
        g lvi = lvi {lviMarked = not (lviMarked lvi)}
    ret <- Gtk.treeStoreChange treeStore treePath g
    unless ret $ putStrLn "treeStoreChange fail"
    updateGraphInfo
    redraw


  -- rebuild the signal tree
  let rebuildSignalTree :: [Tree.Tree (String, String, Maybe (a -> [[(Double, Double)]]))]
                           -> IO ()
      rebuildSignalTree meta = do
        let mkTreeNode (name,typeName,maybeget) = ListViewInfo name typeName maybeget False
            newTrees :: [Tree.Tree (ListViewInfo a)]
            newTrees = map (fmap mkTreeNode) meta
        Gtk.treeStoreClear treeStore
        Gtk.treeStoreInsertForest treeStore [] 0 newTrees
        updateGraphInfo

  oldMetaRef <- IORef.newIORef Nothing
  let maybeRebuildSignalTree newMeta = do
        oldMeta <- IORef.readIORef oldMetaRef
        let sameSignalTree' Nothing _ = False
            sameSignalTree' (Just x) y = sameSignalTree x y
        unless (sameSignalTree' oldMeta newMeta) $ do
          IORef.writeIORef oldMetaRef (Just newMeta)
          rebuildSignalTree (forestFromMeta newMeta)

  -- on insert or change, rebuild the signal tree
  _ <- on msgStore Gtk.rowChanged $ \_ changedPath -> do
    Message newMsg _ _ <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
    maybeRebuildSignalTree newMsg >> redraw
  _ <- on msgStore Gtk.rowInserted $ \_ changedPath -> do
    Message newMsg _ _ <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
    maybeRebuildSignalTree newMsg >> redraw

  -- rebuild the signal tree right now if it exists
  size <- Gtk.listStoreGetSize msgStore
  when (size > 0) $ do
    Message newMsg _ _ <- Gtk.listStoreGetValue msgStore 0
    maybeRebuildSignalTree newMsg >> redraw


  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.containerAdd scroll treeview
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]
  return scroll



makeOptionsWidget :: CC.MVar (GraphInfo a) -> IO () -> IO Gtk.VBox
makeOptionsWidget graphInfoMVar redraw = do
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
                      redraw
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
                      redraw
  _ <- on xRange Gtk.entryActivate updateXRange
  _ <- on yRange Gtk.entryActivate updateYRange

  -- linear or log scaling on the x and y axis?
  xScalingSelector <- Gtk.comboBoxNewText
  yScalingSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xScalingSelector . T.pack)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  mapM_ (Gtk.comboBoxAppendText yScalingSelector . T.pack)
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
        redraw
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
        redraw
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
