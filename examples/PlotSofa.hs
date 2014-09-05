{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module Main ( main ) where

import qualified Data.Foldable as F
import Linear.V3 ( V3(..) )
import Linear.Quaternion ( Quaternion(..) )
import Control.Monad ( when, forever )
import Data.ByteString.Char8 ( pack )
import Data.Serialize
import qualified System.ZMQ4 as ZMQ
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent as CC
import Text.Printf

import Vis

import SofaShared

--type M22 = ((Double,Double),(Double,Double))


--sub :: ((DynCollTraj (Vector Double), CollTrajMeta, [M22], M22) -> IO ()) -> IO ()
--sub writeChan = ZMQ.withContext $ \context ->
--  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
--    ZMQ.connect subscriber url
--    ZMQ.subscribe subscriber (pack channelName)
--    forever $ do
--      _ <- ZMQ.receive subscriber
--      mre <- ZMQ.moreToReceive subscriber
--      when mre $ do

sub :: (SofaMessage -> IO ()) -> IO ()
sub writeChan = ZMQ.withContext $ \context ->
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber url
    ZMQ.subscribe subscriber (pack sofaChannel)
    forever $ do
      _ <- ZMQ.receive subscriber
      mre <- ZMQ.moreToReceive subscriber
      when mre $ do
        msg <- ZMQ.receive subscriber
        let decoded :: SofaMessage
            decoded = case decode msg of
              Left err -> error err
              Right t -> t
        writeChan decoded

main :: IO ()
main = do
  -- keep reading from tcp and storing results in the queue
  trajChan <- STM.atomically STM.newTQueue
  _ <- CC.forkIO (sub (STM.atomically . (STM.writeTQueue trajChan)))

  -- keep parsing results from the queue into nice form
  trajMVar <- CC.newMVar (VisObjects [], [], [])
  
  let getLastValue = do
        val <- STM.atomically (STM.readTQueue trajChan)
        empty' <- STM.atomically (STM.isEmptyTQueue trajChan)
        if empty' then return val else getLastValue

      parserThread = do
        sofaX <- getLastValue
        CC.modifyMVar_ trajMVar $ \(_,_,xs) -> do
          let (mainvis, stages) = toVisObjects sofaX
          return (mainvis, stages, xs)
        parserThread
        
  _ <- CC.forkIO parserThread
  
  animateIO Nothing "sofa lol" (animateFun trajMVar)

multiplyList :: Int -> Int -> [a] -> [a]
multiplyList _ _ [] = []
multiplyList k 0 (_:xs) = multiplyList k k xs
multiplyList k j xs@(x0:_) = x0 : multiplyList k (j-1) xs
  
animateFun :: CC.MVar (VisObject Double, [VisObject Double], [VisObject Double])
              -> Float -> IO (VisObject Double)
animateFun mv = const $ do
  (mainvis, stages, plotstages) <- CC.takeMVar mv
  case plotstages of
    (s0:ss) -> do
      CC.putMVar mv (mainvis, stages, ss)
      return $ VisObjects [mainvis, s0]
    []-> do
      let n = max 1 $ 400 `div` length stages
      CC.putMVar mv (mainvis, stages, multiplyList n n stages)
      return mainvis

linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 xf n = xs
  where
    h = (xf-x0)/(fromIntegral n - 1)
    xs = map (\k -> x0 + h*(fromIntegral k)) (take n [(0::Int)..])

qy :: Quaternion Double
qy = Quaternion 0 $ V3 1 0 0

toVisObjects :: SofaMessage -> (VisObject Double, [VisObject Double])
toVisObjects (SofaMessage iters r points stages) =
  ( RotQuat qy $ VisObjects [walls, txt, shape0
                            , VisObjects (allPoints (-1))
                            , Trans (V3 (-2) (-2) 0) axes
                            ]
  , map (RotQuat qy . Trans (V3 1 1 0)) (allPoints 0)
  )
  where
    walls = VisObjects
            [ Line [ V3 (-4) 1 0
                   , V3 1 1 0
                   , V3 1 (-4) 0
                   ] (makeColor 1 1 1 1)
            , Line [ V3 (-4) 2 0
                   , V3 2 2 0
                   , V3 2 (-4) 0
                   ] (makeColor 1 1 1 1)
            , Line [ V3 (-4) 0 0
                   , V3 0 0 0
                   , V3 0 (-4) 0
                   ] (makeColor 1 1 1 1)
            ]

    axes = Axes (0.5, 15)
    npoints = length points
    nsteps = length stages
    shape0 = Line' $
             zipWith (\(Point x y) c -> ((V3 x y 0) - (V3 2 2 0), c))
             (points ++ [head points])
             (colors (npoints + 1))
    drawOne :: [Point Double] -> Double -> Color -> VisObject Double
    drawOne ps@(p0:_) z =
      Line
      (map (\(Point x y) -> (V3 x y z)) (ps ++ [p0]))
    drawOne _ _ = const (VisObjects [])
  
    area = 0.5 * (F.sum $ zipWithNext' cross points)

    allPoints :: Double -> [VisObject Double]
    allPoints maxheight = zipWith3 (\c so z -> drawOne (stagePoints so) z c)
                (colors (nsteps + 1))
                stages
                (linspace 0 maxheight (nsteps + 1))

    colors :: Int -> [Color]
    colors k = fmap (\gamma -> makeColor 0 gamma (1 - gamma) 1) (gammas k)
    
    gammas :: Int -> [Float]
    gammas k = linspace 0 1 k
  
    stagePoints :: (Point Double, Double) -> [Point Double]
    stagePoints (mean, theta) = fmap rot points
      where
        rot :: Point Double -> Point Double
        rot (Point x y) = mean + Point (x*cos(theta) + y*sin(theta)) (-x*sin(theta) + y*cos(theta))
        

    messages = [ show npoints ++ " segments"
               , show nsteps ++ " stages"
               , printf "segment length: %.4f" r
               , printf "area: %.4f" area
               , "iteration: " ++ show iters
               ]
    txt = VisObjects $
          zipWith (\s k -> Text2d s (30,fromIntegral $ 30*k) TimesRoman24 (makeColor 1 1 1 1))
          messages (reverse [1..length messages])
--    trajLine = Line (zipWith (\x y -> V3 x y 0) (concat xs0) (concat ys0)) (makeColor 1 0 0 0.4)
--    trajDots = Points (zipWith (\x y -> V3 x y 0) xsCollPts ysCollPts) (Just 1) red
--    trajDots' = Points (zipWith (\x y -> V3 x y 0) xsBigPts ysBigPts) (Just 2) red
