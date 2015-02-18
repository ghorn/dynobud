-- | How big of a sofa can we get around a corner?

{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language DataKinds #-}

module Main where

import GHC.Generics ( Generic1 )

import Data.Proxy ( Proxy(..) )
import Data.IORef ( newIORef, readIORef, writeIORef )
import qualified Data.Foldable as F
import Data.Serialize
import qualified System.ZMQ4 as ZMQ
import Data.ByteString.Char8 ( pack )

import Dyno.Vectorize
import Dyno.Nlp
import Dyno.NlpSolver ( SXElement )
import Dyno.NlpUtils
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.Solvers

import Sofa.Common

type NPoints = 81
type NSteps = 61

data X a =
  X
  { xR :: a
  , xPoints :: Vec NPoints (Point a)
  , xStages :: Vec NSteps (Stage a)
  } deriving (Functor, Generic1, Show)

data G a =
  G
  { gMin90 :: Vec NPoints a
  , gEqualR :: Vec NPoints a
  , g360s :: Vec NPoints a
  , gMean0 :: Point a
  , gStages :: Vec NSteps (StageCon a)
  , gCloseMean :: Vec NSteps (Point a)
  , gCloseTheta :: Vec NSteps a
  } deriving (Functor, Generic1, Show)

data Stage a =
  Stage
  { sTheta :: a
  , sMean :: Point a
  , sPhis :: Vec NPoints a
  } deriving (Functor, Generic1, Show)

data StageCon a =
  StageCon
  { scOuters :: Vec NPoints (Point a)
  , scInners :: Vec NPoints a
  } deriving (Functor, Generic1, Show)

instance Vectorize X
instance Vectorize G
instance Vectorize Stage
instance Vectorize StageCon

npoints :: Int
npoints = vlength (Proxy :: Proxy (Vec NPoints))

nsteps :: Int
nsteps = vlength (Proxy :: Proxy (Vec NSteps))

linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 xf n =
  fmap
  (\x -> x0 + (xf - x0)*(fromIntegral x / fromIntegral (n-1)))
  $ take n [(0::Int)..]

radius0 :: Fractional a => a
radius0 = 0.3

segment0 :: Floating a => a
segment0 = 2 * radius0 * sin(pi/fromIntegral npoints)

points0 :: Vec NPoints (Point Double)
points0 = TV.mkVec' $ map (\q -> Point (radius0*cos(q)) (radius0*sin(q))) $ take npoints $ linspace 0 (2*pi) (npoints + 1)

atan2' :: RealFloat a => Point a -> a
atan2' (Point x y) = atan2 y x

--data G a =
--  G
--  { gMin90 :: Vec NPoints a
--  , gEqualR :: Vec NPoints a
--  , gMean0 :: Point a
--  , gStages :: Vec NSteps (StageCon a)
--  , gCloseMean :: Vec NSteps (Point a)
--  , gCloseTheta :: Vec NSteps a
--  } deriving (Functor, Generic1, Show)

--(f0,g0) = fg guess undefined

----worst :: Vectorize f => f Double -> Double
----worst = V.toList (fmap abs)
--  
--blah :: IO ()
--blah = do
----  putStrLn $ "gmin90: " ++ show (minimum $ F.toList $ gMin90 g0)
----  putStrLn $ "gmin90: " ++ show (maximum $ F.toList $ gMin90 g0)
--  print $ gMean0 g0
--  print $ g360 g0
    

guess :: X Double
guess =
  X
  { xR = segment0
  , xPoints = points0
  , xStages = TV.tvzipWith (\mean theta ->
                             Stage { sTheta = theta
                                   , sMean = mean
                                   , sPhis = fill $ min 0 (max (pi/2) (atan2' mean))
                                   }) means0 thetas0
  }
  where
    thetas0 :: Vec NSteps Double
    thetas0 = TV.mkVec' $ linspace 0 0 nsteps

    means0 :: Vec NSteps (Point Double)
    means0 = TV.mkVec' $ map f (linspace (-pi/4) (3*pi/4) nsteps)
--    means0 = TV.mkVec' $ map f (linspace 0 (pi/2) npoints)
      where
        f :: Double -> Point Double
        f q
          | q <= pi/4 = fmap (/ (2*px)) p0
          | otherwise = fmap (/ (2*py)) p0
          where
            p0 = Point px py
            px = cos q
            py = sin q
            

myNlp :: Nlp X None G SXElement
myNlp = Nlp { nlpFG = fg
            , nlpBX = bx
            , nlpBG = bg
            , nlpX0 = guess
            , nlpP = None
            , nlpLamX0 = Nothing
            , nlpLamG0 = Nothing
            , nlpScaleF = Nothing
            , nlpScaleX = Nothing
            , nlpScaleG = Nothing
            }
  where
    
    bx :: X Bounds
    bx = X
         { xR = (Just (segment0/2), Nothing)
         , xPoints = fill $ Point (Just (-5), Just 5) (Just (-5), Just 5)
         , xStages = TV.mkVec' $ stage0 : replicate (nsteps-1) otherStages
         }
      where
        stage0 =
          Stage
          { sTheta = (Just 0, Just 0)
          , sMean = Point (Just (-3), Just 3) (Just (-3), Just 3)
          , sPhis = fill (Just 0, Just (pi/2))
          }
        otherStages =
          Stage
          { sTheta = (Just (-4*pi), Just (4*pi))
          , sMean = Point (Just (-3), Just 3) (Just (-3), Just 3)
          , sPhis = fill (Just 0, Just (pi/2))
          }


    bg :: G Bounds
    bg = G
         { gMin90 = fill (Just 0.8, Nothing)
         , gEqualR = fill (Just 0, Just 0)
         , gMean0 = fill (Just 0, Just 0)
         , g360s = TV.mkVec' $ map (\q -> (Just (q - pi), Just (q + pi)))
                   $ linspace 0 (2*pi) npoints
         , gStages = TV.mkVec' $ stage0 : replicate (nsteps-2) midStages ++ [stageF]
         , gCloseMean = TV.mkVec' $ replicate (nsteps - 1) (fill (Just (-deltaMean), Just deltaMean)) ++ [fill (Nothing, Nothing)]
         , gCloseTheta = TV.mkVec' $ replicate (nsteps - 1) (Just (-deltaTheta), Just deltaTheta) ++ [(Nothing, Nothing)]
         }
      where
        deltaTheta = pi / fromIntegral nsteps
        deltaMean = 4 / fromIntegral nsteps
        stage0 = StageCon
                 { scOuters = fill $ Point (Nothing, Just 1) (Nothing, Just 0)
                 , scInners = fill (Just 0, Nothing)
                 }
        stageF = StageCon
                 { scOuters = fill $ Point (Nothing, Just 0) (Nothing, Just 1)
                 , scInners = fill (Just 0, Nothing)
                 }
        midStages = StageCon
                    { scOuters = fill $ Point (Nothing, Just 1) (Nothing, Just 1)
                    , scInners = fill (Just 0, Nothing)
                    }

dot :: Num a => Point a -> Point a -> a
dot (Point x0 y0) (Point x1 y1) = x0*x1 + y0*y1

fg :: forall a . Floating a => X a -> None a -> (a, G a)
fg (X r points stages) _ = (f, g)
  where
    ds :: Vec NPoints (Point a)
    ds = zipWithNext (\x0 x1 -> x1 - x0) points

    curvatureRegularization = (F.sum (zipWithNext (\x0 x1 -> dot x0 x1) ds)) / (fromIntegral npoints)

    f = 1*curvatureRegularization - 0.5 * (F.sum $ zipWithNext cross points)
    g = G
        { gMin90 = zipWithNext (\x0 x1 -> dot x0 x1 / ((norm2 x0) * (norm2 x1))) ds
        , gEqualR = fmap (\(Point x y) -> x*x + y*y - r*r) ds
        , gMean0 = F.sum points / (fromIntegral npoints)
        , g360s = TV.mkVec' $
                  drop 1 $ scanl (+) 0 $
                  F.toList $
                  zipWithNext
                  (\d0 d1 -> asin ((d0 `cross` d1) / ((1e-9 + norm2 d0) * (1e-9 + norm2 d1))))
                  ds
        , gStages = fmap stageCon stages
        , gCloseMean = zipWithNext (\(Stage _ mean1 _) (Stage _ mean0 _) -> mean1 - mean0) stages
        , gCloseTheta = zipWithNext (\(Stage theta1 _ _) (Stage theta0 _ _) -> theta1 - theta0) stages
        }

    stageCon :: Stage a -> StageCon a
    stageCon (Stage theta mean phis) = StageCon { scOuters = points'
                                                , scInners = TV.tvzipWith inner points' phis
                                                }
      where
        rot :: Point a -> Point a
        rot (Point x y) = mean + Point (x*cos(theta) + y*sin(theta)) (-x*sin(theta) + y*cos(theta))
        
        points' :: Vec NPoints (Point a)
        points' = fmap rot points

        inner (Point xij' yij') phiij = xij'*cos(phiij) + yij'*sin(phiij)

solver :: Solver
solver = ipoptSolver { options = [("ma86_order", Opt "metis"), ("max_iter", Opt (1000 :: Int))]}
--solver = snoptSolver { options = [ ("detect_linear", Opt False) ] }

send :: Serialize a => ZMQ.Socket ZMQ.Pub -> String -> a -> IO ()
send publisher chanName stuff = do
  let bs = encode stuff
  ZMQ.send publisher [ZMQ.SendMore] (pack chanName)
  ZMQ.send publisher [] bs

main :: IO ()
main =
  ZMQ.withContext $ \context ->
  ZMQ.withSocket context ZMQ.Pub $ \publisher -> do
    ZMQ.bind publisher url
    putStrLn $ "# design vars: " ++ show (vlength (Proxy :: Proxy X))
    putStrLn $ "# constraints: " ++ show (vlength (Proxy :: Proxy G))
    iters <- newIORef 0
    _ <- solveNlp solver myNlp $ Just $ \x -> do
      k <- readIORef iters
      writeIORef iters (k + 1)
      let msg = SofaMessage
                { smSegmentLength = xR x
                , smIters = k
                , smPoints = F.toList (xPoints x)
                , smMeanThetas = map (\stg -> (sMean stg, sTheta stg)) $ F.toList (xStages x)
                }
      --mapM_ (\stg -> print (sMean stg, sTheta stg)) $ F.toList (xStages x)
      send publisher sofaChannel msg
      return True
    return ()

