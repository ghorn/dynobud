{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Main where

import Data.Vector ( Vector )

import Dyno.Cov
--import Dyno.Vectorize
import Dyno.View
import Dyno.TypeVecs
import Dyno.Nats
import Dyno.Ipopt
--import Dyno.Snopt
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Server.Accessors

import Dyno.Ocp
import Dyno.DirectCollocation

data PendX a = PendX { pX  :: J S a
                     , pY  :: J S a
                     , pVx :: J S a
                     , pVy :: J S a
                     } deriving (Generic, Show)
data PendZ a = PendZ { pTau :: J S a}  deriving (Generic, Show)
data PendU a = PendU { pTorque :: J S a } deriving (Generic, Show)
data PendP a = PendP { pMass :: J S a } deriving (Generic, Show)
data PendR a = PendR (J S a) (J S a) (J S a) (J S a) (J S a) deriving (Generic, Show)
data PendO a = PendO deriving (Generic, Show)

instance View PendX
instance View PendZ
instance View PendU
instance View PendP
instance View PendR
instance View PendO

instance Lookup (PendX (Vector ()))
instance Lookup (PendZ (Vector ()))
instance Lookup (PendU (Vector ()))

mayer :: (Viewable a, Num a) => t -> J PendX a -> J PendX a -> J (Cov JNone) a -> J (Cov JNone) a -> J S a
mayer _ _ _ _ _ = cat 0

lagrange :: (Viewable a, Floating (J S a))
            => J PendX a -> J PendZ a -> J PendU a -> J PendP a -> J PendO a -> J S a -> J S a
lagrange x _ u _ _ _ = vx*vx + vy*vy + 1e-4*torque**2
  where
    PendX _ _ vx vy = split x
    PendU torque = split u

r :: Floating a => a
r = 0.3

pendDae :: (Viewable a, Floating (J S a)) => Dae PendX PendZ PendU PendP PendR PendO a
pendDae xx' xx zz uu pp _ =
  (cat $ PendR (x' - vx) (y' - vy)
  (m*vx' + x*tau - fx)
  (m*vy' + y*tau - fy)
  (x*vx' + y*vy' + (vx*vx + vy*vy))
  , cat PendO
  )
  where
    fx =  torque*y
    fy = -torque*x + m*9.8
    (PendX x' y' vx' vy') = split xx'
    (PendX x y vx vy) = split xx
    (PendZ tau) = split zz
    (PendU torque) = split uu
    (PendP m) = split pp

--    dae['c']    = dae['x']*dae['x'] + dae['z']*dae['z'] - r*r
--    dae['cdot'] = dae['dx']*dae['x'] + dae['dz']*dae['z']


pendOcp :: OcpPhase PendX PendZ PendU PendP PendR PendO (JVec D8 S) JNone JNone JNone JNone
pendOcp = OcpPhase { ocpMayer = mayer
                   , ocpLagrange = lagrange
                   , ocpDae = pendDae
                   , ocpBc = bc
                   , ocpPathC = pathc
                   , ocpPathCBnds = cat JNone -- pathcb
                   , ocpBcBnds = jfill (Just 0, Just 0)
                   , ocpXbnd = xbnd
                   , ocpUbnd = ubnd
                   , ocpZbnd = jfill (Nothing, Nothing)
                   , ocpPbnd = jfill (Just 0.3, Just 0.3)
                   , ocpTbnd = jfill (Just 4, Just 10)

                   , ocpSq = 0
                   , ocpSbnd = jfill (Nothing,Nothing)
                   , ocpSbc = \_ _ -> cat JNone
                   , ocpSbcBnds = cat JNone
                   , ocpSh = \_ _ -> cat JNone
                   , ocpShBnds = cat JNone
                   }

pathc :: Viewable a => J x a -> J z a -> J u a -> J p a -> J o a -> J S a -> J JNone a
pathc _ _ _ _ _ _ = cat JNone

xbnd :: J PendX (Vector Bounds)
xbnd = jfill (Just (-10), Just 10)
--xbnd = cat $
--       PendX { pX =  cat $ S (Just (-10), Just 10)
--             , pY =  cat $ S (Just (-10), Just 10)
--             , pVx = cat $ S (Just (-10), Just 10)
--             , pVy = cat $ S (Just (-10), Just 10)
--             }

ubnd :: J PendU (Vector Bounds)
ubnd = jfill (Just (-40), Just 40)

bc :: (Viewable a, Floating (J S a)) => J PendX a -> J PendX a -> J (JVec D8 S) a
bc xx0 xxf =
  cat $ JVec $ mkVec'
  [ x0
  , y0 + r
  , vx0
  , vy0
  , xf
  , yf - r
  , vxf
  , vyf
  ]
  where
    PendX x0 y0 vx0 vy0 = split xx0
    PendX xf yf vxf vyf = split xxf

type NCollStages = D80
type CollDeg = D3

guess :: J (CollTraj PendX PendZ PendU PendP JNone NCollStages CollDeg) (Vector Double)
guess = jfill 1

solver :: NlpSolverStuff IpoptSolver
solver = ipoptSolver

solver2 :: NlpSolverStuff IpoptSolver
solver2 = ipoptSolver { options = [("expand", Opt True)] }


main :: IO ()
main = do
  nlp <- makeCollNlp pendOcp
  _ <- solveNlp solver (nlp { nlpX0 = guess }) Nothing
--  _ <- solveNlp solver2 (nlp { nlpX0 = guess }) Nothing
  return ()
