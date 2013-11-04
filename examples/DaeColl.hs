{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Main where

--import qualified Data.Vector as V

import qualified Control.Concurrent as CC
import Plotter ( runPlotter, newChannel )

import Hascm.Vectorize
import Hascm.TypeVecs
import Hascm.TypeNats
import Hascm.Accessors

import Hascm.Ocp
import Hascm.DirectCollocation
--import Hascm.Nlp

data PendX a = PendX { pX :: a
                     , pY :: a
                     , pVx :: a
                     , pVy :: a
                     } deriving (Functor, Generic, Generic1, Show)
data PendZ a = PendZ { pTau :: a}  deriving (Functor, Generic, Generic1, Show)
data PendU a = PendU { pTorque :: a } deriving (Functor, Generic, Generic1, Show)
data PendP a = PendP { pMass :: a } deriving (Functor, Generic, Generic1, Show)
data PendR a = PendR a a a a a deriving (Functor, Generic, Generic1, Show)

instance Vectorize PendX
instance Vectorize PendZ
instance Vectorize PendU
instance Vectorize PendP
instance Vectorize PendR

instance (Lookup a, Generic a) => Lookup (PendX a)
instance (Lookup a, Generic a) => Lookup (PendZ a)
instance (Lookup a, Generic a) => Lookup (PendU a)

meyer :: Num a => t -> a
meyer _ = 0

lagrange :: Floating a => PendX a -> PendZ a -> PendU a -> PendP a -> a
lagrange (PendX _ _ vx vy) (PendZ _) (PendU torque) (PendP _) = vx*vx + vy*vy + 1e-4*torque**2
--
--springOde :: Floating a => ExplicitOde SpringX SpringU None a
--springOde (SpringX x v) None (SpringU u) None = SpringX v acc
--  where
--    acc = -k*x -b*v + u
--    k = 2.6
--    b = 0.2
r :: Floating a => a
r = 0.3

pendDae :: Floating a => Dae PendX PendZ PendU PendP PendR a
pendDae (PendX x' y' vx' vy') (PendX x y vx vy) (PendZ tau) (PendU torque) (PendP m) =
  PendR (x' - vx) (y' - vy)
  (m*vx' + x*tau - fx)
  (m*vy' + y*tau - fy)
  (x*vx' + y*vy' + (vx*vx + vy*vy))
  where
    fx =  torque*y
    fy = -torque*x + m*9.8

--    dae['c']    = dae['x']*dae['x'] + dae['z']*dae['z'] - r*r
--    dae['cdot'] = dae['dx']*dae['x'] + dae['dz']*dae['z']


pendOcp :: Floating a => OcpPhase PendX PendZ PendU PendP PendR (Vec D8) None a
pendOcp = OcpPhase { ocpMeyer = meyer
                   , ocpLagrange = lagrange
                   , ocpDae = pendDae
                   , ocpBc = bc
                   , ocpPathC = pathc
                   , ocpPathCBnds = None -- pathcb
                   , ocpXbnd = xbnd
                   , ocpUbnd = ubnd
                   , ocpZbnd = fill (Nothing, Nothing)
                   , ocpPbnd = fill (Just 0.3, Just 0.3)
                   , ocpTbnd = (Just 4, Just 4)
                   }

pathc :: x a -> z a -> u a -> p a -> None a
pathc _ _ _ _ = None

----pathcb :: None a
----pathcb = None

xbnd :: PendX (Maybe Double, Maybe Double)
xbnd = PendX { pX = (Just (-10), Just (10))
             , pY = (Just (-10), Just (10))
             , pVx = (Just (-1), Just (1))
             , pVy = (Just (-1), Just (1))
             }

ubnd :: PendU (Maybe Double, Maybe Double)
ubnd = PendU (Just (-100), Just (100))

bc :: Floating a => PendX a -> PendX a -> Vec D8 a
bc (PendX x0 y0 vx0 vy0) (PendX xf yf vxf vyf) =
  mkVec' [ x0-r
         , y0
         , vx0
         , vy0
         , xf
         , yf - r
         , vxf
         , vyf
         ]

xAccessors :: [(String, PendX Double -> Double)]
xAccessors = flatten $ accessors (fill 0)

xAccessors' :: AccessorTree (PendX Double)
xAccessors' = accessors (fill 0)

type NCollStages = D40
type CollDeg = D3

callback :: (PlotPointsL PendX PendZ PendU Double -> IO ())
            -> CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double -> IO Bool
callback writeMe traj = do
  writeMe $ plotPointLists $ plotPoints traj
--  CC.threadDelay 100000
  return True


main :: IO ()
main = do
  (chan, writeMe) <- newChannel "woo" (toPlotTree)
  _ <- CC.forkIO $ runPlotter chan []

  let runAFew 0 = return ()
      runAFew k = do
        _ <- solveCollNlp pendOcp (Just (callback writeMe))
             :: IO (CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double)
        CC.threadDelay 1000000
        runAFew (k-1 :: Int)
  runAFew 100
