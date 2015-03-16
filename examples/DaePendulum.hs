{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}

module Main where

import GHC.Generics ( Generic, Generic1 )

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )

import Accessors

import Dyno.Vectorize
import Dyno.View.View ( J, jfill )
import Dyno.TypeVecs
import Dyno.Solvers
import Dyno.Nlp
import Dyno.NlpUtils
import Dyno.Ocp
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem )
import Dyno.DirectCollocation.Types ( CollTraj )
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
type instance C PendOcp = Vec 8
type instance H PendOcp = None
type instance Q PendOcp = None

data PendX a = PendX { pX  :: a
                     , pY  :: a
                     , pVx :: a
                     , pVy :: a
                     } deriving (Functor, Generic, Generic1, Show)
data PendZ a = PendZ { pTau :: a}  deriving (Functor, Generic, Generic1, Show)
data PendU a = PendU { pTorque :: a } deriving (Functor, Generic, Generic1, Show)
data PendP a = PendP { pMass :: a } deriving (Functor, Generic, Generic1, Show)
data PendR a = PendR a a a a a deriving (Functor, Generic, Generic1, Show)
data PendO a = PendO deriving (Functor, Generic, Generic1, Show)

instance Vectorize PendX
instance Vectorize PendZ
instance Vectorize PendU
instance Vectorize PendP
instance Vectorize PendR
instance Vectorize PendO

instance Lookup (PendX ())
instance Lookup (PendZ ())
instance Lookup (PendU ())
instance Lookup (PendO ())
instance Lookup (PendP ())

mayer :: Num a => t -> PendX a -> PendX a -> None a -> PendP a -> a
mayer _ _ _ _ _ = 0

lagrange :: Floating a => PendX a -> PendZ a -> PendU a -> PendP a -> PendO a -> a -> a -> a
lagrange x _ u _ _ _ _ = vx*vx + vy*vy + 1e-4*torque**2
  where
    PendX _ _ vx vy = x
    PendU torque = u

r :: Floating a => a
r = 0.3

pendDae :: Floating a => PendX a -> PendX a -> PendZ a -> PendU a -> PendP a -> a -> (PendR a, PendO a)
pendDae (PendX x' y' vx' vy') (PendX x y vx vy) (PendZ tau) (PendU torque) (PendP m) _ =
  (PendR (x' - vx) (y' - vy)
   (m*vx' + x*tau - fx)
   (m*vy' + y*tau - fy)
   (x*vx' + y*vy' + (vx*vx + vy*vy))
  , PendO
  )
  where
    fx =  torque*y
    fy = -torque*x + m*9.8

pendOcp :: OcpPhase PendOcp
pendOcp = OcpPhase { ocpMayer = mayer
                   , ocpLagrange = lagrange
                   , ocpQuadratures = \_ _ _ _ _ _ _ -> None
                   , ocpDae = pendDae
                   , ocpBc = bc
                   , ocpPathC = pathc
                   , ocpPathCBnds = None
                   , ocpBcBnds = fill (Just 0, Just 0)
                   , ocpXbnd = xbnd
                   , ocpUbnd = ubnd
                   , ocpZbnd = fill (Nothing, Nothing)
                   , ocpPbnd = PendP (Just 0.3, Just 0.3)
                   , ocpTbnd = (Just 4, Just 10)
                   , ocpObjScale      = Nothing
                   , ocpTScale        = Nothing
                   , ocpXScale        = Nothing
                   , ocpZScale        = Nothing
                   , ocpUScale        = Nothing
                   , ocpPScale        = Nothing
                   , ocpResidualScale = Nothing
                   , ocpBcScale       = Nothing
                   , ocpPathCScale    = Nothing
                   }

pathc :: Floating a => PendX a -> PendZ a -> PendU a -> PendP a -> PendO a -> a -> None a
pathc _ _ _ _ _ _ = None

xbnd :: PendX Bounds
xbnd = PendX { pX =  (Just (-10), Just 10)
             , pY =  (Just (-10), Just 10)
             , pVx = (Just (-10), Just 10)
             , pVy = (Just (-10), Just 10)
             }

ubnd :: PendU Bounds
ubnd = PendU (Just (-40), Just 40)

bc :: Floating a => PendX a -> PendX a -> None a -> PendP a -> a -> Vec 8 a
bc (PendX x0 y0 vx0 vy0) (PendX xf yf vxf vyf) _ _ _ =
  mkVec'
  [ x0
  , y0 + r
  , vx0
  , vy0
  , xf
  , yf - r
  , vxf
  , vyf
  ]

type NCollStages = 80
type CollDeg = 3

guess :: J (CollTraj PendOcp NCollStages CollDeg) (Vector Double)
guess = jfill 1

solver :: Solver
solver = ipoptSolver { options = [("linear_solver", Opt "ma86")]}

solver2 :: Solver
solver2 = ipoptSolver { options = [("expand", Opt True)] }


main :: IO ()
main = do
  cp  <- makeCollProblem Legendre pendOcp
  withCallback $ \send -> do
    let nlp = cpNlp cp
        meta = toMeta (Proxy :: Proxy PendOcp)
        cb' traj = do
          plotPoints <- cpPlotPoints cp traj
          send (plotPoints, meta)
    _ <- solveNlp solver (nlp { nlpX0 = guess }) (Just cb')
--  _ <- solveNlp solver2 (nlp { nlpX0 = guess }) Nothing
    return ()
