{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module ServerSender ( withCallback ) where

import Data.ByteString.Char8 ( pack )
import Data.Serialize
import qualified System.ZMQ4 as ZMQ

import Dyno.DirectCollocation.Dynamic

callback :: (Serialize a) => ZMQ.Socket ZMQ.Pub -> String -> a -> IO Bool
callback publisher chanName stuff = do
  let bs = encode stuff
  ZMQ.send publisher [ZMQ.SendMore] (pack chanName)
  ZMQ.send publisher [] bs
  return True

withCallback :: (Serialize a) => String -> String -> ((([DynCollTraj a], CollTrajMeta) -> IO Bool) -> IO b) -> IO b
withCallback url channelName userFun =
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Pub $ \publisher ->
      ZMQ.bind publisher url >> userFun (callback publisher channelName)
