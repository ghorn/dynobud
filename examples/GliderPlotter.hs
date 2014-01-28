{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import Control.Monad ( when, forever )
import Data.ByteString.Char8 ( pack )
import Data.Serialize
import qualified System.ZMQ3 as ZMQ

import Hascm.Server.Server ( runPlotter, newChannel )

import Hascm.DirectCollocation.Dynamic

import GliderShared ( gliderUrl, gliderChannelName )

sub :: String -> ((DynCollTraj Double, CollTrajMeta) -> IO ()) -> String -> IO ()
sub ip writeChan name = ZMQ.withContext $ \context ->
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber ip
    ZMQ.subscribe subscriber (pack name)
    forever $ do
      _ <- ZMQ.receive subscriber
      mre <- ZMQ.moreToReceive subscriber
      when mre $ do
        msg <- ZMQ.receive subscriber
        let decoded :: (DynCollTraj Double, CollTrajMeta)
            decoded = case decode msg of
              Left err -> error err
              Right t -> t
        writeChan decoded

main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""
  
  (c0, writeMe) <- newChannel gliderChannelName

  listenerTid0 <- CC.forkIO (sub gliderUrl writeMe gliderChannelName)
  runPlotter c0 [listenerTid0]
