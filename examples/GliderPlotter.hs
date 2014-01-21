{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import Control.Monad ( forever )
import qualified Data.Vector as V
import Data.ByteString.Char8 ( pack )
import Data.Serialize
import qualified System.ZMQ3 as ZMQ

import Plotter ( runPlotter, newChannel )

import Hascm.Vectorize
import Hascm.DirectCollocation.Dynamic
import Hascm.Models.Aircraft

import GliderTypes


sub :: String -> (PlotPointsL AcX None AcU Double -> IO ()) -> String -> IO ()
sub ip writeChan name = ZMQ.withContext $ \context -> do
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber ip
    ZMQ.subscribe subscriber (pack name)
    forever $ do
      _ <- ZMQ.receive subscriber
      mre <- ZMQ.moreToReceive subscriber
      if mre
      then do
        msg <- ZMQ.receive subscriber
        let traj :: GliderDesignVars Double
            traj = case fmap (devectorize . V.fromList) (decode msg) of
              Left err -> error err
              Right t -> t
        writeChan (plotPointLists $ plotPoints traj)
      else return ()


main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""
  
  (c0, writeMe) <- newChannel gliderChannelName gliderPlotTree

  listenerTid0 <- CC.forkIO (sub gliderUrl writeMe gliderChannelName)
  runPlotter c0 [listenerTid0]
