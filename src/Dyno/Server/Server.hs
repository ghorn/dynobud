{-# OPTIONS_GHC -Wall #-}

module Dyno.Server.Server
       ( newChannel
       , runPlotter
       , Channel
       ) where

import qualified Control.Concurrent as CC
import qualified Data.IORef as IORef
import Data.Time ( getCurrentTime, diffUTCTime )
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL

import qualified GHC.Stats

import Dyno.Server.PlotTypes ( Channel(..), Message(..) )
import Dyno.Server.GraphWidget ( newGraph )
import Dyno.DirectCollocation.Dynamic ( CollTrajMeta(..), DynPlotPoints )

newChannel :: String -> IO (Channel, (DynPlotPoints Double, CollTrajMeta) -> IO ())
newChannel name = do
  time0 <- getCurrentTime

  msgStore <- Gtk.listStoreNew []
  counter <- IORef.newIORef 0

  let newMessage :: (DynPlotPoints Double, CollTrajMeta) -> IO ()
      newMessage (newTrajs, newMeta) = do
        -- grab the time and counter
        time <- getCurrentTime
        k <- IORef.readIORef counter
        IORef.writeIORef counter (k+1)
        Gtk.postGUIAsync $ do
          let pps = newTrajs
              val = Message pps k (diffUTCTime time time0) newMeta
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then Gtk.listStorePrepend msgStore val
            else Gtk.listStoreSetValue msgStore 0 val

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        }

  return (retChan, newMessage)

runPlotter :: Channel -> [CC.ThreadId] -> IO ()
runPlotter channel backgroundThreadsToKill = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled
  if statsEnabled
    then do putStrLn "stats enabled"
            stats <- GHC.Stats.getGCStats
            print stats
    else putStrLn "stats not enabled"

  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]

  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []
  let killEverything = do
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ CC.killThread backgroundThreadsToKill
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to clear history
  buttonClear <- Gtk.buttonNewWithLabel "clear history"
  _ <- Gtk.onClicked buttonClear $ do
    --let clearChan (Channel {chanSeq=cs}) = void (CC.swapMVar cs Seq.empty)
    let clearChan _ = putStrLn "yeah, history clear doesn't really exist lol"
    clearChan channel

  -- list of channels
  chanWidget <- newChannelWidget channel graphWindowsToBeKilled

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := buttonClear
               , Gtk.containerChild := chanWidget
               ]

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- the list of channels
newChannelWidget :: Channel -> CC.MVar [Gtk.Window] -> IO Gtk.TreeView
newChannelWidget channel graphWindowsToBeKilled = do
  -- create a new tree model
  model <- Gtk.listStoreNew [channel]
  treeview <- Gtk.treeViewNewWithModel model
  Gtk.treeViewSetHeadersVisible treeview True

  -- add some columns
  col0 <- Gtk.treeViewColumnNew
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew
  col3 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col0 "channel"
  Gtk.treeViewColumnSetTitle col1 "history"
  Gtk.treeViewColumnSetTitle col2 "new"
  Gtk.treeViewColumnSetTitle col3 "save"

  renderer0 <- Gtk.cellRendererTextNew
  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew
  renderer3 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col0 renderer0 True
  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True
  Gtk.cellLayoutPackStart col3 renderer3 True

  Gtk.cellLayoutSetAttributes col0 renderer0 model $ \lv -> [ Gtk.cellText := chanName lv]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ const [ Gtk.cellToggleActive := False]
  Gtk.cellLayoutSetAttributes col3 renderer3 model $ const [ Gtk.cellToggleActive := False]


  _ <- Gtk.treeViewAppendColumn treeview col0
  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2
  _ <- Gtk.treeViewAppendColumn treeview col3

  -- spawn a new graph when a checkbox is clicked
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    let (i:_) = Gtk.stringToTreePath pathStr
    lv <- Gtk.listStoreGetValue model i
    graphWin <- newGraph (chanName lv) (chanMsgStore lv)

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


--  -- save all channel data when this button is pressed
--  _ <- on renderer3 Gtk.cellToggled $ \pathStr -> do
--    let (i:_) = Gtk.stringToTreePath pathStr
--    lv <- Gtk.listStoreGetValue model i
--    let writerThread = do
--          bct <- chanGetByteStrings (lvChan lv)
--          let filename = chanName (lvChan lv) ++ "_log.dat"
--              blah _      sizes [] = return (reverse sizes)
--              blah handle sizes ((x,_,_):xs) = do
--                BSL.hPut handle x
--                blah handle (BSL.length x : sizes) xs
--          putStrLn $ "trying to write file \"" ++ filename ++ "\"..."
--          sizes <- withFile filename WriteMode $ \handle -> blah handle [] bct
--          putStrLn $ "finished writing file, wrote " ++ show (length sizes) ++ " protos"
--
--          putStrLn "writing file with sizes..."
--          writeFile (filename ++ ".sizes") (unlines $ map show sizes)
--          putStrLn "done"
--    _ <- CC.forkIO writerThread
    return ()

  return treeview
