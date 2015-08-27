{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )

import Accessors

import Dyno.Vectorize
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( catJV )
import Dyno.Solvers
import Dyno.Nlp
import Dyno.NlpUtils
import Dyno.Ocp
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem, makeGuess )
import Dyno.DirectCollocation.Types ( CollTraj' )
import Dyno.DirectCollocation.Dynamic ( toMeta )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )

import Dynoplot.Callback ( withCallback )

data PendOcp
type instance X PendOcp = PendX
type instance Z PendOcp = PendZ
type instance U PendOcp = PendU
type instance P PendOcp = PendP
type instance R PendOcp = PendR
type instance O PendOcp = PendO
type instance C PendOcp = PendBc
type instance H PendOcp = None
type instance Q PendOcp = None
type instance QO PendOcp = None
type instance FP PendOcp = None
type instance PO PendOcp = None

data PendX a = PendX { pX  :: a
                     , pY  :: a
                     , pVx :: a
                     , pVy :: a
                     , pTorque :: a
                     } deriving (Functor, Generic, Generic1, Show)
data PendZ a = PendZ { pTau :: a}  deriving (Functor, Generic, Generic1, Show)
data PendU a = PendU { pTorqueDot :: a } deriving (Functor, Generic, Generic1, Show)
data PendP a = PendP { pMass :: a } deriving (Functor, Generic, Generic1, Show)
data PendR a = PendR a a a a a a deriving (Functor, Generic, Generic1, Show)
data PendO a = PendO deriving (Functor, Generic, Generic1, Show)
data PendBc a = PendBc (PendX a) (PendX a) deriving (Functor, Generic, Generic1, Show)

instance Vectorize PendX
instance Vectorize PendZ
instance Vectorize PendU
instance Vectorize PendP
instance Vectorize PendR
instance Vectorize PendO
instance Vectorize PendBc

instance Lookup (PendX ())
instance Lookup (PendZ ())
instance Lookup (PendU ())
instance Lookup (PendO ())
instance Lookup (PendP ())

mayer :: a -> PendX a -> PendX a -> None a -> PendP a -> None a -> a
mayer tf _ _ _ _ _ = tf

lagrange :: Floating a => PendX a -> PendZ a -> PendU a -> PendP a -> None a -> PendO a -> a -> a -> a
lagrange _ _ u _ _ _ _ tf = 1e-3*torque'**2 / tf
  where
    PendU torque' = u

r :: Floating a => a
r = 0.3

pendDae :: Floating a => PendX a -> PendX a -> PendZ a -> PendU a -> PendP a -> None a -> a -> (PendR a, PendO a)
pendDae (PendX x' y' vx' vy' torque') (PendX x y vx vy torque)
  (PendZ tau) (PendU uTorque') (PendP m) _ _ = (residual, outputs)
  where
    residual =
      PendR (x' - vx) (y' - vy)
        (m*vx' + x*tau - fx)
        (m*vy' + y*tau - fy)
        (x*vx' + y*vy' + (vx*vx + vy*vy))
        (torque' - uTorque')
    outputs = PendO

    fx =  torque*y
    fy = -torque*x + m*9.8

pendOcp :: OcpPhase' PendOcp
pendOcp =
  OcpPhase
  { ocpMayer = mayer
  , ocpLagrange = lagrange
  , ocpQuadratures = \_ _ _ _ _ _ _ _ -> None
  , ocpQuadratureOutputs = \_ _ _ _ _ _ _ _ -> None
  , ocpDae = pendDae
  , ocpBc = bc
  , ocpPathC = pathc
  , ocpPlotOutputs = \_ _ _ _ _ _ _ _ _ _ _ -> None
  , ocpObjScale      = Nothing
  , ocpTScale        = Nothing
  , ocpXScale        = Just pendXScale
  , ocpZScale        = Just (PendZ 10)
  , ocpUScale        = Just (PendU 50)
  , ocpPScale        = Just (PendP 0.3)
  , ocpResidualScale = Nothing
  , ocpBcScale       = Just $ PendBc pendXScale pendXScale
  , ocpPathCScale    = Just None
  }

pendOcpInputs :: OcpPhaseInputs' PendOcp
pendOcpInputs =
  OcpPhaseInputs
  { ocpPathCBnds = None
  , ocpBcBnds = bcBnds
  , ocpXbnd = xbnd
  , ocpUbnd = ubnd
  , ocpZbnd = PendZ (Just (-200), Just 200)
  , ocpPbnd = PendP (Just 0.3, Just 0.3)
  , ocpTbnd = (Just 0.1, Just 5)
  , ocpFixedP = None
  }

pendXScale :: PendX Double
pendXScale = PendX 0.3 0.3 1 1 10

pathc :: Floating a => PendX a -> PendZ a -> PendU a -> PendP a -> None a -> PendO a -> a -> None a
pathc _ _ _ _ _ _ _ = None

xbnd :: PendX Bounds
xbnd = PendX { pX =  (Nothing, Nothing)
             , pY =  (Nothing, Nothing)
             , pVx = (Nothing, Nothing)
             , pVy = (Nothing, Nothing)
             , pTorque = (Just (-30), Just 30)
             }

ubnd :: PendU Bounds
ubnd = PendU (Just (-100), Just 100)

bc :: Floating a => PendX a -> PendX a -> None a -> PendP a -> None a -> a -> PendBc a
bc x0 xf _ _ _ _ = PendBc x0 xf

bcBnds :: PendBc Bounds
bcBnds =
  PendBc
  (PendX
   { pX = (Just 0, Just 0)
   , pY = (Just (-r), Just (-r))
   , pVx = (Just 0, Just 0)
   , pVy = (Just 0, Just 0)
   , pTorque = (Nothing, Nothing)
   })
  (PendX
   { pX = (Nothing, Nothing) -- LICQ
   , pY = (Just r, Just r)
   , pVx = (Just 0, Just 0)
   , pVy = (Nothing, Nothing) -- LICQ
   , pTorque = (Nothing, Nothing)
   })

type NCollStages = 120
type CollDeg = 3

guess :: J (CollTraj' PendOcp NCollStages CollDeg) (Vector Double)
guess = cat $ makeGuess Radau tf guessX guessZ guessU parm
  where
    tf = 1
    guessX t = PendX { pX =   r * sin q
                     , pY = - r * cos q
                     , pVx = r*w*cos q
                     , pVy = r*w*sin q
                     , pTorque = 0
                     }
      where
        q = pi*t/tf
        w = pi/tf
    guessZ _ = PendZ {pTau = 0}
    guessU _ = PendU {pTorqueDot = 0}
    parm = PendP 0.3

solver :: Solver
solver = ipoptSolver { options = [ ("expand", Opt True)
                                 , ("linear_solver", Opt "ma86")
                                 , ("ma86_order", Opt "metis")
                                 ]}

solver2 :: Solver
solver2 = ipoptSolver { options = [("expand", Opt True)] }


main :: IO ()
main = do
  cp  <- makeCollProblem Legendre pendOcp pendOcpInputs guess
  withCallback $ \send -> do
    let nlp = cpNlp cp
        meta = toMeta (cpMetaProxy cp)
        cb' traj _ = do
          plotPoints <- cpPlotPoints cp traj (catJV None)
          send (plotPoints, meta)
    _ <- solveNlp solver nlp (Just cb')
--  _ <- solveNlp solver2 nlp Nothing
    return ()
