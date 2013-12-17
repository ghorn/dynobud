{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import Control.Monad ( forever )
import qualified Data.Vector as V
#if OSX
import Data.ByteString ( ByteString )
#endif
import Data.ByteString.Char8 ( pack )
import Data.Serialize
#if OSX
import qualified System.ZMQ3 as ZMQ
#else
import qualified System.ZMQ as ZMQ
#endif

import Plotter ( runPlotter, newChannel )

import Hascm.Vectorize
import Hascm.DirectCollocation
import Hascm.Models.Aircraft

import GliderTypes


withContext :: (ZMQ.Context -> IO a) -> IO a
#if OSX
withContext = ZMQ.withContext
#else
withContext = ZMQ.withContext 1
#endif

#if OSX
pack' :: String -> ByteString
pack' = pack
#else
pack' :: String -> String
pack' = id
#endif

sub :: String -> (PlotPointsL AcX None AcU Double -> IO ()) -> String -> IO ()
sub ip writeChan name = withContext $ \context -> do
#if OSX
  let receive = ZMQ.receive
#else
  let receive = flip ZMQ.receive []
#endif
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber ip
    ZMQ.subscribe subscriber (pack' name)
    forever $ do
      _ <- receive subscriber
      mre <- ZMQ.moreToReceive subscriber
      if mre
      then do
        msg <- receive subscriber
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
