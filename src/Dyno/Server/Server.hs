{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Dyno.Server.Server
       ( newChannel
       , runPlotter
       , Channel
       ) where

import qualified GHC.Stats

import qualified Control.Concurrent as CC
import qualified Data.IORef as IORef
import Data.Time ( getCurrentTime, diffUTCTime )
import Data.Tree ( Tree )
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL


import Dyno.Server.PlotTypes ( Channel(..), Message(..) )
import Dyno.Server.GraphWidget ( newGraph )

newChannel ::
  forall a
  . String
  -> (a -> a -> Bool)
  -> (a -> [Tree (String,String,Maybe (a -> [[(Double, Double)]]))])
  -> IO (Channel a, a -> IO ())
newChannel name sameSignalTree toSignalTree = do
  time0 <- getCurrentTime

  msgStore <- Gtk.listStoreNew []
  counter <- IORef.newIORef 0

  let newMessage :: a -> IO ()
      newMessage next = do
        -- grab the time and counter
        time <- getCurrentTime
        k <- IORef.readIORef counter
        IORef.writeIORef counter (k+1)
        Gtk.postGUIAsync $ do
          let val = Message next k (diffUTCTime time time0)
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then Gtk.listStorePrepend msgStore val
            else Gtk.listStoreSetValue msgStore 0 val

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        , chanSameSignalTree = sameSignalTree
                        , chanToSignalTree = toSignalTree
                        }

  return (retChan, newMessage)


runPlotter :: forall a . Channel a -> [CC.ThreadId] -> IO ()
runPlotter channel backgroundThreadsToKill = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled

  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]

  statsLabel <- Gtk.labelNew (Nothing :: Maybe String)
  let statsWorker = do
        CC.threadDelay 500000
        msg <- if statsEnabled
               then do
                 stats <- GHC.Stats.getGCStats
                 return $ printf "The current memory usage is %.2f MB"
                   ((realToFrac (GHC.Stats.currentBytesUsed stats) :: Double) /(1024*1024))
               else return "(enable GHC statistics with +RTS -T)"
        Gtk.postGUISync $ Gtk.labelSetText statsLabel ("Welcome to Plot-ho-matic!\n" ++ msg)
        statsWorker

  statsThread <- CC.forkIO statsWorker
  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []

  let killEverything = do
        CC.killThread statsThread
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ CC.killThread backgroundThreadsToKill
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to clear history
  buttonDoNothing <- Gtk.buttonNewWithLabel "this button does absolutely nothing"
  _ <- Gtk.onClicked buttonDoNothing $ do
    --let clearChan (Channel {chanSeq=cs}) = void (CC.swapMVar cs Seq.empty)
    let doNothing _ = putStrLn "seriously, it does nothing"
    doNothing channel

  -- list of channels
  chanWidget <- newChannelWidget channel graphWindowsToBeKilled

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := statsLabel
               , Gtk.boxChildPacking statsLabel := Gtk.PackNatural
               , Gtk.containerChild := buttonDoNothing
               , Gtk.containerChild := chanWidget
               ] -- ++ concatMap (\x -> [Gtk.containerChild := x
                 --                     , Gtk.boxChildPacking x := Gtk.PackNatural
                 --                     ]) [chanWidget]

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- the list of channels
newChannelWidget :: Channel a
                    -> CC.MVar [Gtk.Window] -> IO Gtk.VBox
newChannelWidget channel graphWindowsToBeKilled = do
  vbox <- Gtk.vBoxNew False 4

  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget (chanName channel) nameBox'

  buttonsBox <- Gtk.hBoxNew False 4

  -- button to clear history
  buttonAlsoDoNothing <- Gtk.buttonNewWithLabel "also do nothing"
  _ <- Gtk.onClicked buttonAlsoDoNothing $ do
    putStrLn "i promise, nothing happens"
    -- CC.modifyMVar_ logData (const (return S.empty))
    return ()

  -- button to make a new graph
  buttonNew <- Gtk.buttonNewWithLabel "new graph"
  _ <- Gtk.onClicked buttonNew $ do
    graphWin <- newGraph
                (chanName channel)
                (chanSameSignalTree channel)
                (chanToSignalTree channel)
                (chanMsgStore channel)

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


  -- entry to set history length
  entryAndLabel <- Gtk.hBoxNew False 4
  entryLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "max history:"
  entryEntry <- Gtk.entryNew
  Gtk.set entryEntry [ Gtk.entryEditable := True
                     , Gtk.widgetSensitive := True
                     ]
  Gtk.entrySetText entryEntry "200"
  let updateMaxHistory = do
        txt <- Gtk.get entryEntry Gtk.entryText
        case readMaybe txt :: Maybe Int of
          Just k -> putStrLn $ "yeah this doesn't really have much effect to be honest, but i got this: " ++ show k
          Nothing -> putStrLn $ "max history: couldn't make an Int out of \"" ++ show txt ++ "\""
  _ <- on entryEntry Gtk.entryActivate updateMaxHistory
  updateMaxHistory


  Gtk.set entryAndLabel [ Gtk.containerChild := entryLabel
                        , Gtk.boxChildPacking entryLabel := Gtk.PackNatural
                        , Gtk.containerChild := entryEntry
                        , Gtk.boxChildPacking entryEntry := Gtk.PackNatural
                        ]


  -- put all the buttons/entries together
  Gtk.set buttonsBox [ Gtk.containerChild := buttonNew
                     , Gtk.boxChildPacking buttonNew := Gtk.PackNatural
                     , Gtk.containerChild := buttonAlsoDoNothing
                     , Gtk.boxChildPacking buttonAlsoDoNothing := Gtk.PackNatural
                     , Gtk.containerChild := entryAndLabel
                     , Gtk.boxChildPacking entryAndLabel := Gtk.PackNatural
                     ]

  Gtk.set vbox [ Gtk.containerChild := nameBox
               , Gtk.boxChildPacking   nameBox := Gtk.PackNatural
               , Gtk.containerChild := buttonsBox
               , Gtk.boxChildPacking   buttonsBox := Gtk.PackNatural
               ]

  return vbox


----  -- save all channel data when this button is pressed
----  _ <- on renderer3 Gtk.cellToggled $ \pathStr -> do
----    let (i:_) = Gtk.stringToTreePath pathStr
----    lv <- Gtk.listStoreGetValue model i
----    let writerThread = do
----          bct <- chanGetByteStrings (lvChan lv)
----          let filename = chanName (lvChan lv) ++ "_log.dat"
----              blah _      sizes [] = return (reverse sizes)
----              blah handle sizes ((x,_,_):xs) = do
----                BSL.hPut handle x
----                blah handle (BSL.length x : sizes) xs
----          putStrLn $ "trying to write file \"" ++ filename ++ "\"..."
----          sizes <- withFile filename WriteMode $ \handle -> blah handle [] bct
----          putStrLn $ "finished writing file, wrote " ++ show (length sizes) ++ " protos"
----
----          putStrLn "writing file with sizes..."
----          writeFile (filename ++ ".sizes") (unlines $ map show sizes)
----          putStrLn "done"
----    _ <- CC.forkIO writerThread
--    return ()
--
--  return treeview


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
