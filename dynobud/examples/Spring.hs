{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Main ( SpringX(..), SpringU(..), main ) where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )

import Accessors ( Lookup )

import Dyno.View.View ( J, jfill, catJV )
import Dyno.Nlp ( Bounds )
import Dyno.Ocp
import Dyno.Vectorize ( Vectorize, None(..), vpure, vapply )
import Dyno.Solvers ( Solver(..), GType(..), ipoptSolver )
import Dyno.NlpUtils ( solveNlp )
import Dyno.DirectCollocation.Formulate
       ( CollProblem(..), DirCollOptions(..), MapStrategy(..), makeCollProblem, def )
import Dyno.DirectCollocation.Types ( CollTraj' )
import Dyno.DirectCollocation.Dynamic ( toMeta )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )
import Dynoplot.Callback ( withCallback )

springOcp :: OcpPhase' SpringOcp
springOcp =
  OcpPhase
  { ocpMayer = mayer
  , ocpLagrange = lagrange
  , ocpQuadratures = \_ _ _ _ _ _ _ _ -> None
  , ocpQuadratureOutputs = \_ _ _ _ _ _ _ _ -> None
  , ocpDae = dae
  , ocpBc = bc
  , ocpPathC = pathC
  , ocpPlotOutputs = \_ _ _ _ _ _ _ _ _ _ _ -> None
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

springOcpInputs :: OcpPhaseInputs' SpringOcp
springOcpInputs =
  OcpPhaseInputs
  { ocpBcBnds = bcBnds
  , ocpPathCBnds = pathCBnds
  , ocpXbnd = pure (Nothing, Nothing)
  , ocpZbnd = pure (Nothing, Nothing)
  , ocpUbnd = pure (Nothing, Nothing)
  , ocpPbnd = pure (Nothing, Nothing)
  , ocpTbnd = (Just 4, Just 4)
  , ocpFixedP = None
  }

data SpringOcp
type instance X SpringOcp = SpringX
type instance O SpringOcp = SpringO
type instance R SpringOcp = SpringX
type instance U SpringOcp = SpringU
type instance C SpringOcp = SpringBc
type instance H SpringOcp = SpringPathC
type instance P SpringOcp = None
type instance Z SpringOcp = None
type instance Q SpringOcp = None
type instance QO SpringOcp = None
type instance FP SpringOcp = None
type instance PO SpringOcp = None

data SpringX a =
  SpringX
  { xPos :: a
  , xVel :: a
  } deriving (Functor, Generic, Generic1)
data SpringU a =
  SpringU
  { uMotor :: a
  } deriving (Functor, Generic, Generic1)
data SpringO a =
  SpringO
  { oForce :: a
  , oObj :: a
  } deriving (Functor, Generic, Generic1)
data SpringBc a =
  SpringBc
  { bcX0 :: SpringX a
  , bcXF :: SpringX a
  } deriving (Functor, Generic, Generic1)
data SpringPathC a = SpringPathC a deriving (Functor, Generic, Generic1)

instance Applicative SpringX where {pure = vpure; (<*>) = vapply}
instance Applicative SpringU where {pure = vpure; (<*>) = vapply}
instance Applicative SpringO where {pure = vpure; (<*>) = vapply}
instance Applicative SpringBc where {pure = vpure; (<*>) = vapply}
instance Applicative SpringPathC where {pure = vpure; (<*>) = vapply}

instance Vectorize SpringX
instance Vectorize SpringU
instance Vectorize SpringO
instance Vectorize SpringBc
instance Vectorize SpringPathC

instance Lookup a => Lookup (SpringX a)
instance Lookup a => Lookup (SpringU a)
instance Lookup a => Lookup (SpringO a)
instance Lookup a => Lookup (SpringBc a)
instance Lookup a => Lookup (SpringPathC a)

dae :: Floating a
       => SpringX a -> SpringX a -> None a -> SpringU a -> None a -> None a -> a
       -> (SpringX a, SpringO a)
dae (SpringX p' v') (SpringX p v) _ (SpringU u) _ _ t =
  (residual, outputs)
  where
    residual = SpringX (p' - v) (v' - force)
    outputs = SpringO { oForce = force
                      , oObj = obj
                      }
    k = 4
    b = 0.3
    
    force = u - k * p - b * v + 0.1 * sin t
    obj = p**2 + v**2 + u**2

bc :: SpringX a -> SpringX a -> None a -> None a -> None a -> a -> SpringBc a
bc x0 xf _ _ _ _ = SpringBc x0 xf

bcBnds :: SpringBc Bounds
bcBnds =
  SpringBc
  { bcX0 = SpringX (Just 0, Just 0) (Just 0, Just 0)
  , bcXF = SpringX (Just 1, Just 1) (Just 0, Just 0)
  }

mayer :: Floating a => a -> SpringX a -> SpringX a -> None a -> None a -> None a -> a
mayer endTime _ (SpringX pf vf) _ _ _ = (pf**2 + vf**2 + endTime/1000)

pathC :: Floating a => SpringX a -> None a -> SpringU a -> None a -> None a -> SpringO a -> a -> SpringPathC a
pathC (SpringX _ v) _ (SpringU u) _ _ _ time =
  SpringPathC (v**2 + u**2 - time/100)

pathCBnds :: SpringPathC Bounds
pathCBnds = SpringPathC (Nothing, Just 4)

lagrange :: Fractional a => SpringX a -> None a -> SpringU a -> None a -> None a -> SpringO a -> a -> a -> a
lagrange _ _ _ _ _ (SpringO force obj) _ _ = obj + force*force*1e-4

solver :: Solver
solver = ipoptSolver { options = [("expand", GBool True)] }

guess :: J (CollTraj' SpringOcp NCollStages CollDeg) (Vector Double)
guess = jfill 1

type NCollStages = 100
type CollDeg = 3

dirCollOpts :: DirCollOptions
dirCollOpts = def
  { mapStrategy = Unroll
  , collocationRoots = Legendre
  }

main :: IO ()
main = 
  withCallback $ \send -> do

    cp  <- makeCollProblem dirCollOpts springOcp springOcpInputs guess
    let nlp = cpNlp cp
        meta = toMeta (cpMetaProxy cp)

        cb' traj _ _ = do
          plotPoints <- cpPlotPoints cp traj (catJV None)
          send (plotPoints, meta)

    _ <- solveNlp "spring" solver nlp (Just cb')
    return ()
