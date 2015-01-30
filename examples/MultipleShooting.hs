{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}

module Main
       ( main
       ) where

import GHC.Generics ( Generic )
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

import Dyno.View.View
import Dyno.View.JV
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Solvers
import Dyno.Vectorize
import Dyno.View.CasadiMat ( MX )
import Dyno.Nats
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
  myNlp <- makeMsNlp ocp :: IO (Nlp' (MsDvs X U P D40) JNone (MsConstraints X D40) MX)
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
  renderableToWindow (chart [ ("u", (map (\(U u) -> u) us) ++ [0])
                            , ("p", map (\(X p _) -> p) xs)
                            , ("v", map (\(X _ v) -> v) xs)
                            ]) 600 600

chart :: [(String, [Double])] -> Renderable ()
chart vals = toRenderable layout
  where
    points :: (String, [Double]) -> PlotPoints Double Double
    points (name, ys) = plot_points_style .~ filledCircles 2 (opaque red)
       $ plot_points_values .~ (zip [0..] ys)
           $ plot_points_title .~ name
           $ def

    layout :: Layout Double Double
    layout = layout_title .~ "a plot"
           $ layout_plots .~ (map (toPlot . points) vals)
           $ def
