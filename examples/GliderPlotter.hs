{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import Control.Monad ( forever )
import qualified Data.Vector as V
import Data.ByteString.Char8 ( pack )
import Data.Serialize
import qualified System.ZMQ3 as ZMQ

import Hascm.Server.Server ( runPlotter, newChannel )

import Hascm.Vectorize
import Hascm.DirectCollocation.Dynamic

import GliderTypes


sub :: String -> ((DynCollTraj Double, CollTrajMeta) -> IO ()) -> String -> IO ()
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
        writeChan (ctToDynamic traj, toMeta traj)
      else return ()

main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""
  
  (c0, writeMe) <- newChannel gliderChannelName

  listenerTid0 <- CC.forkIO (sub gliderUrl writeMe gliderChannelName)
  runPlotter c0 [listenerTid0]
