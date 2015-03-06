{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveDataTypeable #-}

module Main ( main ) where

import Control.Monad ( when, forever )
import Data.ByteString.Char8 ( pack )
import Data.Serialize
import qualified System.ZMQ4 as ZMQ
import System.Console.CmdArgs ( (&=), Data, Typeable )
import qualified System.Console.CmdArgs as CA

import PlotHo ( runPlotter )

import Dyno.DirectCollocation.Dynamic ( DynPlotPoints, CollTrajMeta, addCollocationChannel )

import Dynoplot.Channel ( dynoplotUrl, dynoplotChannelName )

sub :: String -> ((DynPlotPoints Double, CollTrajMeta) -> IO ()) -> String -> IO ()
sub ip' writeChan name = ZMQ.withContext $ \context ->
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber ip'
    ZMQ.subscribe subscriber (pack name)
    forever $ do
      _ <- ZMQ.receive subscriber
      mre <- ZMQ.moreToReceive subscriber
      when mre $ do
        msg <- ZMQ.receive subscriber
        let decoded :: (DynPlotPoints Double, CollTrajMeta)
            decoded = case decode msg of
              Left err -> error err
              Right t -> t
        writeChan decoded

main :: IO ()
main = do
  args <- CA.cmdArgs (myargs &= CA.program "dynoplot")
  let ip' = ip args
      channel' = channel args
  putStrLn $ "using ip \""++ip'++"\""
  putStrLn $ "using channel \""++channel'++"\""

  runPlotter $ addCollocationChannel channel' (\w -> sub ip' w channel')


data VisArgs = VisArgs { ip :: String
                       , channel :: String
                       } deriving (Show, Data, Typeable)

myargs :: VisArgs
myargs = VisArgs { ip = dynoplotUrl               &= CA.help "an IP address" &= CA.typ "ADDRESS"
                 , channel = dynoplotChannelName  &= CA.help "zmq channel name"
                 } &= CA.summary "plotter for dynobud OCPs"
