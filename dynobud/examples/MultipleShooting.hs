{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}

module Main
       ( main
       ) where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Foldable as F

import Control.Monad ( void )
import Graphics.Rendering.Chart hiding ( x0 )
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Linear ( Additive(..) )

import Casadi.MX ( MX )

import Dyno.Nlp
import Dyno.NlpUtils
import Dyno.Solvers
import Dyno.Vectorize
import Dyno.View.View
import Dyno.View.JVec
import Dyno.MultipleShooting

-- state/control/parameter definitions
data X a = X a a deriving (Functor, Generic, Generic1, Show)
data U a = U a deriving (Functor, Generic, Generic1, Show)
data P a = P deriving (Functor, Generic, Generic1, Show)

-- boilerplate
instance Vectorize X
instance Vectorize U
instance Vectorize P

instance Applicative X where {pure = vpure; (<*>) = vapply}
instance Applicative U where {pure = vpure; (<*>) = vapply}
instance Applicative P where {pure = vpure; (<*>) = vapply}
instance Additive X where
  zero = pure 0

-- ocp specification
ocp :: MsOcp X U P
ocp =
  MsOcp
  { msOde = ode
  , msEndTime = 10
  , msXBnds = X (Just (-2), Just 2) (Just (-2), Just 2)
  , msUBnds = U (Just (-3), Just 3)
  , msPBnds = P
  , msMayer = \_ -> 0
  , msLagrangeSum = \(X p v) (U u) -> p*p + v*v + u*u
  , msX0 = X (Just 0) (Just 0)
  , msXF = X (Just 1) (Just 1)
  , msNumRk4Steps = Just 10
  }

-- dynamics
ode :: Floating a => X a -> U a -> P a -> a -> X a
ode (X x v) (U u) _p _t = X v (-x -0.1*v + u)

-- run the thing
main :: IO ()
main = do
  myNlp <- makeMsNlp ocp :: IO (Nlp (MsDvs X U P 40) (JV None) (MsConstraints X 40) MX)
  (_, eopt) <- solveNlp "multiple_shooting" ipoptSolver myNlp Nothing
  opt <- case eopt of
          Left err -> error $ "nlp solver returned status " ++ show err
          Right r -> return r
  let xopt = split $ xOpt opt
      splitXU xu = (splitJV x, splitJV u)
        where
          JTuple x u = split xu
      (xs', us) = unzip $ map splitXU $ F.toList $ unJVec $ split (dvXus xopt)
      xf = splitJV (dvXf xopt)
      xs = xs' ++ [xf]
      renderable :: Renderable ()
      renderable = charts [ ("u", zip [0..] (map (\(U u)   -> u) us))
                          , ("p", zip [0..] (map (\(X p _) -> p) xs))
                          , ("v", zip [0..] (map (\(X _ v) -> v) xs))
                          ]
      fileOptions :: FileOptions
      fileOptions =
        fo_format .~ SVG
        $ fo_size .~ (600, 600)
        $ def
      path = "./multiple_shooting.svg"
  putStrLn $ "writing solution plot to " ++ show path
  void $ renderableToFile fileOptions path renderable


charts :: [(String,[(Double,Double)])] -> Renderable ()
charts vals = toRenderable slayouts
  where
    plots :: (String, [(Double, Double)]) -> StackedLayout Double
    plots (name, xys) = StackedLayout layout
      where
        lines' :: PlotLines Double Double
        lines' = plot_lines_values .~ [xys]
                 $ plot_lines_title .~ name
                 $ def

        points :: PlotPoints Double Double
        points = plot_points_style .~ filledCircles 2 (opaque red)
                 $ plot_points_values .~ [(x,y) |  (x,y) <- xys]
                 $ plot_points_title .~ name
                 $ def

        layout :: Layout Double Double
        layout = layout_title .~ name
                 $ layout_plots .~ [toPlot lines', toPlot points]
                 $ def

    slayouts :: StackedLayouts Double
    slayouts = slayouts_compress_legend .~ False
               $ slayouts_layouts .~ (map plots vals)
               $ def
