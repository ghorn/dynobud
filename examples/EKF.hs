{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Preliminary example of the EKF in action.
-- Todo: add noise.
module Main ( main ) where

import GHC.Generics ( Generic, Generic1 )

import Control.Monad ( void )
import Graphics.Rendering.Chart hiding ( x0 )
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Lens
import System.Environment ( getArgs )

import Casadi.SX ( SX )
import Dyno.KalmanFilter
import Dyno.Random
import Dyno.View ( S )
import Dyno.View.Vectorize
import Linear

data SpringX a =
  SpringX
  { xPos :: a
  , xVel :: a
  } deriving (Functor, Generic, Generic1, Show)
instance Vectorize SpringX
instance Applicative SpringX where
  pure = fill
  (<*>) = vapply
instance Additive SpringX where
  zero = fill 0

data SpringQ a =
  SpringQ
  { qDisturbanceForce :: a
  } deriving (Functor, Foldable, Generic, Generic1)
instance Vectorize SpringQ

data SpringY a =
  SpringY
  { yPos :: a
  } deriving (Functor, Foldable, Generic, Generic1)
instance Vectorize SpringY
instance Applicative SpringY where
  pure = fill
  (<*>) = vapply
instance Additive SpringY where
  zero = fill 0

springModel :: forall a . Floating a => KFModel SpringX SpringQ SpringY None a
springModel =
  KFModel
  { kfOde = ode
  , kfSensors = sensors
  }
  where
    ode (SpringX {xPos = p, xVel = v}) (SpringQ {qDisturbanceForce = f}) =
      SpringX
      { xPos = v
      , xVel = f - 5 * p - 0.4 * v
      }

    sensors :: SpringX a -> None a -> SpringY a
    sensors x _ = SpringY {yPos = xPos x}

main :: IO ()
main = do
  runner <- compileModel (springModel :: KFModel SpringX SpringQ SpringY None (S SX))
  args <- getArgs
  let sensorNoiseCovarianceDiag = SpringY {yPos = 0.5**2}
      processNoiseCovarianceDiag = SpringQ {qDisturbanceForce = 1.0**2}
  sensorNoise <- initRandomIO $ vdiag sensorNoiseCovarianceDiag
  processNoise <- initRandomIO $ vdiag processNoiseCovarianceDiag

  let ts = case args of
        [ts'] -> read ts'
        _ -> 0.01

      -- Simulate Euler step and try to track it.
      simStep :: (SpringX Double, KFState SpringX) -> IO (SpringX Double, KFState SpringX)
      simStep (x0, kfState0) = do
        w <- processNoise
        v <- sensorNoise
        let -- integrated simulation (Euler)
            x1 = x0 ^+^ ts *^ kfOde springModel x0 (w ^/ sqrt ts)
            -- A-priori filter state
            kfState1 = kfEulerPropagate runner (Left processNoiseCovarianceDiag) ts kfState0
            -- sensor measurement
            y1 = v ^+^ kfSensors springModel x1 None
            -- A-posteriori filter state
            kfState1' = kfSensorUpdate runner y1 None (Left sensorNoiseCovarianceDiag) kfState1
        return (x1, kfState1')

  let go :: (Double, SpringX Double, KFState SpringX) -> IO [(Double, SpringX Double, KFState SpringX)]
      go (t0, x0, kf0)
        | t0 > 10 = return []
        | otherwise = do
            (x1, kf1) <- simStep (x0, kf0)
            others <- go (t0 + ts, x1, kf1)
            return $ (t0, x0, kf0) : others

      initialSimState = SpringX 2 3
      initialKFState = initKFState (pure 0) (Left (pure 50))

  trajectory <- go (0.0, initialSimState, initialKFState)

  plotTrajectory trajectory


plotTrajectory :: [(Double, SpringX Double, KFState SpringX)] -> IO ()
plotTrajectory traj = do
  let mkStatePlot :: (String, SpringX Double -> Double) -> StackedLayout Double
      mkStatePlot (name, f) = StackedLayout layout
        where
          simulated :: PlotLines Double Double
          simulated =
            plot_lines_style . line_color .~ opaque blue
            $ plot_lines_values .~ [map (\(t, x,  _) -> (t, f x)) traj]
            $ plot_lines_title .~ "simulated"
            $ def

          estimated :: PlotLines Double Double
          estimated =
            plot_lines_style . line_color .~ opaque red
            $ plot_lines_values .~ [map (\(t, _, kf) -> (t, f (getState kf))) traj]
            $ plot_lines_title .~ "estimated"
            $ def

--          estimated' :: PlotPoints Double Double
--          estimated' =
--            plot_points_style .~ filledCircles 1 (opaque red)
--            $ plot_points_values .~ (map (\(t, _, kf) -> (t, f (getState kf))) traj)
--            $ plot_points_title .~ "estimated"
--            $ def

          green1 = opaque $ sRGB 0.5 1 0.5

          variance =
            plot_fillbetween_style .~ solidFillStyle green1
            $ plot_fillbetween_values .~ [ (t, (x - v, x + v))
                                         | (t, _, kf) <- traj
                                         , let x = f (getState kf)
                                               v = f (getSigmas kf)
                                         ]
            $ plot_fillbetween_title .~ "variance"
            $ def

          layout :: Layout Double Double
          layout = layout_title .~ name
                   $ layout_plots .~ [ toPlot variance
--                                     , toPlot estimated'
                                     , toPlot simulated
                                     , toPlot estimated
                                     ]
                   $ def


      slayouts :: StackedLayouts Double
      slayouts = slayouts_compress_legend .~ False
                 $ slayouts_layouts .~
                 concat [ map mkStatePlot [("p", xPos), ("v", xVel)]
                        ]
                 $ def

      renderable :: Renderable ()
      renderable = toRenderable slayouts

      fileOptions :: FileOptions
      fileOptions =
        fo_format .~ SVG
        $ fo_size .~ (600, 600)
        $ def

      path = "./kalman_filter.svg"

  putStrLn $ "writing solution plot to " ++ show path
  void $ renderableToFile fileOptions path renderable
