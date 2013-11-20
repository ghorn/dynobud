{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}

module Main where

--import qualified Data.Vector as V

import qualified Control.Concurrent as CC
import Plotter ( runPlotter, newChannel )

import Hascm.Vectorize
import Hascm.TypeVecs
import Hascm.TypeNats
import Hascm.Accessors
--import Hascm.Ipopt
import Hascm.Snopt
import Hascm.Sqp.Sqp
import Hascm.Sqp.LineSearch

import Hascm.Ocp
import Hascm.DirectCollocation
import qualified Hascm.Nlp as Nlp

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

mayer :: Num a => PendX a -> a -> a
mayer _ _ = 0

lagrange :: Floating a => PendX a -> PendZ a -> PendU a -> PendP a -> a -> a
lagrange (PendX _ _ vx vy) (PendZ _) (PendU torque) (PendP _) _ = vx*vx + vy*vy + 1e-4*torque**2

r :: Floating a => a
r = 0.3

pendDae :: Floating a => Dae PendX PendZ PendU PendP PendR a
pendDae (PendX x' y' vx' vy') (PendX x y vx vy) (PendZ tau) (PendU torque) (PendP m) _ =
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
pendOcp = OcpPhase { ocpMayer = mayer
                   , ocpLagrange = lagrange
                   , ocpDae = pendDae
                   , ocpBc = bc
                   , ocpPathC = pathc
                   , ocpPathCBnds = None -- pathcb
                   , ocpXbnd = xbnd
                   , ocpUbnd = ubnd
                   , ocpZbnd = fill (Nothing, Nothing)
                   , ocpPbnd = fill (Just 0.3, Just 0.3)
                   , ocpTbnd = (Just 4, Just 10)
                   }

pathc :: x a -> z a -> u a -> p a -> a -> None a
pathc _ _ _ _ _ = None

----pathcb :: None a
----pathcb = None

xbnd :: PendX (Maybe Double, Maybe Double)
xbnd = PendX { pX = (Just (-10), Just (10))
             , pY = (Just (-10), Just (10))
             , pVx = (Just (-10), Just (10))
             , pVy = (Just (-10), Just (10))
             }

ubnd :: PendU (Maybe Double, Maybe Double)
ubnd = PendU (Just (-40), Just (40))

bc :: Floating a => PendX a -> PendX a -> Vec D8 a
bc (PendX x0 y0 vx0 vy0) (PendX xf yf vxf vyf) =
  mkVec' [ x0
         , y0 + r
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

type NCollStages = D80
type CollDeg = D3

callback :: (PlotPointsL PendX PendZ PendU Double -> IO ())
            -> CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double -> IO Bool
callback writeMe traj = do
  writeMe $ plotPointLists $ plotPoints traj
  --CC.threadDelay 100000
  return True

solveCollNlp ::
  (PositiveT n, NaturalT deg, NaturalT n, NaturalT (Succ deg), deg ~ Pred (Succ deg),
   Vectorize x, Vectorize r, Vectorize c, Vectorize h, Vectorize p, Vectorize u, Vectorize z) =>
  (forall a. Floating a => OcpPhase x z u p r c h a) ->
  Maybe (CollTraj x z u p n deg Double -> IO Bool) ->
  IO (CollTraj x z u p n deg Double)
--solveCollNlp ocp = solveNlpIpopt (makeCollNlp ocp) (Just (fill 1)) None
--solveCollNlp ocp cb = solveNlpSnopt (makeCollNlp ocp) cb (fill 1) None
solveCollNlp ocp _ = fmap fst $ solveSqp (makeCollNlp ocp) armilloSearch (fill 1) None
  --print xopt
  --print kktInf


--main :: IO ()
--main = do
--  (chan, writeMe) <- newChannel "woo" (toPlotTree)
--  _ <- CC.forkIO $ runPlotter chan []
--
--  let runAFew 0 = return ()
--      runAFew k = do
--        _ <- solveCollNlp pendOcp (Just (callback writeMe))
--             :: IO (Either String (CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double))
--        CC.threadDelay 1000000
--        runAFew (k-1 :: Int)
--  runAFew 10

guess :: CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double
guess = fill 1

main :: IO ()
main = do
  (Right nlpOut) <- solveNlpSnopt (makeCollNlp pendOcp) Nothing guess None Nothing -- :: IO (Either String (NlpOut (CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double) CollOcpConstraints)
--  (Right xopt) <- solveCollNlp pendOcp Nothing :: IO (CollTraj PendX PendZ PendU PendP NCollStages CollDeg Double)
  _ <- solveSqp (makeCollNlp pendOcp) armilloSearch (Nlp.xOpt nlpOut) None
  return ()
