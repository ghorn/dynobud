{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}

module Main
       ( main
       ) where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Vector as V
import qualified Data.Foldable as F
import Control.Applicative ( Applicative(..) )
import Linear

import Graphics.Rendering.Chart hiding ( x0 )
import Graphics.Rendering.Chart.Gtk
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens

import Casadi.MX ( MX )

import Dyno.View.View
import Dyno.View.JV
import Dyno.View.JVec
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Solvers
import Dyno.Vectorize
import Dyno.MultipleShooting

-- state/control/parameter definitions
data X a = X a a deriving (Functor, Generic, Generic1, Show)
data U a = U a deriving (Functor, Generic, Generic1, Show)
data P a = P deriving (Functor, Generic, Generic1, Show)

-- boilerplate
instance Vectorize X
instance Vectorize U
instance Vectorize P
instance Applicative X where
 pure = fill
 x0 <*> x1 = devectorize (V.zipWith id (vectorize x0) (vectorize x1))
instance Applicative U where
 pure = fill
 x0 <*> x1 = devectorize (V.zipWith id (vectorize x0) (vectorize x1))
instance Additive X where
 zero = fill 0
instance Additive U where
 zero = fill 0

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
  myNlp <- makeMsNlp ocp :: IO (Nlp' (MsDvs X U P 40) JNone (MsConstraints X 40) MX)
  (msg,opt') <- solveNlp' ipoptSolver myNlp Nothing
  opt <- case msg of
          Left err -> error err
          Right _ -> return opt'
  let xopt = split $ xOpt' opt
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
  renderableToWindow renderable 600 600


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
