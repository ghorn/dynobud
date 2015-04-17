{-# OPTIONS_GHC -Wall #-}

module Dynoplot.Callback ( withCallback ) where

import Data.ByteString.Char8 ( pack )
--import Data.ByteString.Lazy ( toStrict )
--import Data.Binary ( Binary, encode )
import Data.Serialize ( Serialize, encode )
import qualified System.ZMQ4 as ZMQ

import Dyno.DirectCollocation.Dynamic

import Dynoplot.Channel ( dynoplotUrl, dynoplotChannelName )

callback :: Serialize a => ZMQ.Socket ZMQ.Pub -> String -> a -> IO Bool
callback publisher chanName stuff = do
  let bs = encode stuff
  ZMQ.send publisher [ZMQ.SendMore] (pack chanName)
--  ZMQ.send publisher [] (toStrict bs)
  ZMQ.send publisher [] bs
  return True

withCallback :: Serialize a => (((DynPlotPoints a, CollTrajMeta) -> IO Bool) -> IO b) -> IO b
withCallback userFun =
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Pub $ \publisher ->
      ZMQ.bind publisher dynoplotUrl >> userFun (callback publisher dynoplotChannelName)
