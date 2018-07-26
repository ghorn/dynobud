{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Main ( main ) where

import Linear
import Data.Vector ( Vector )

import Dyno.Vectorize
import Dyno.View.View
import Dyno.Solvers
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch
import Dyno.NlpUtils

import Dyno.Ocp
import Dyno.DirectCollocation.Formulate
import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( toMeta )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )

import Glider.Aircraft
import Glider.AeroCoeffs
import Glider.Betty

import Dynoplot.Callback ( withCallback )

type NCollStages = 100
type CollDeg = 2

data GliderOcp
type instance X GliderOcp = AcX
type instance Z GliderOcp = None
type instance U GliderOcp = AcU
type instance P GliderOcp = None
type instance R GliderOcp = AcX
type instance O GliderOcp = None
type instance C GliderOcp = AcX
type instance H GliderOcp = None
type instance Q GliderOcp = None
type instance QO GliderOcp = None
type instance FP GliderOcp = None
type instance PO GliderOcp = None

mayer :: Floating a => a -> AcX a -> AcX a -> None a -> None a -> None a -> a
mayer _ _ _ _ _ _ = 0

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> None a -> None a -> a -> a -> a
lagrange (AcX _ _ _ _ (AcU surfs)) _ (AcU surfs') _ _ _ _ _ =
  elev**2 + rudd**2 + ail**2 + flaps**2 +
  100*(elev'**2 + rudd'**2 + ail'**2 + flaps'**2)
  where
    elev = csElev surfs
    rudd = csElev surfs
    ail = csElev surfs
    flaps = csFlaps surfs

    elev' = csElev surfs'
    rudd' = csElev surfs'
    ail' = csElev surfs'
    flaps' = csFlaps surfs'

dae :: Floating a => AcX a -> AcX a -> None a -> AcU a -> None a -> None a -> a -> (AcX a, None a)
dae x' x _ u _ _ _ = (aircraftDae (mass, inertia) fcs mcs refs x' x u, None)
  where
    mass = bettyMass
    inertia = bettyInertia
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: OcpPhase' GliderOcp
ocp = OcpPhase { ocpMayer = mayer
               , ocpLagrange = lagrange
               , ocpQuadratures = \_ _ _ _ _ _ _ _ -> None
               , ocpQuadratureOutputs = \_ _ _ _ _ _ _ _ -> None
               , ocpDae = dae
               , ocpBc = bc
               , ocpPathC = pathc
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

ocpInputs :: OcpPhaseInputs' GliderOcp
ocpInputs =
  OcpPhaseInputs
  { ocpPathCBnds = None
  , ocpBcBnds = pure (Just 0, Just 0)
  , ocpXbnd = xbnd
  , ocpUbnd = ubnd
  , ocpZbnd = None
  , ocpPbnd = None
  , ocpTbnd = (Just 0.5, Just 0.5)
--  , ocpTbnd = (Just 4, Just 4)
  , ocpFixedP = None
  }

pathc :: x a -> z a -> u a -> p a -> None a -> None a -> a -> None a
pathc _ _ _ _ _ _ _ = None

xbnd :: AcX (Maybe Double, Maybe Double)
xbnd = AcX { ac_r_n2b_n = pure (Nothing, Nothing)
           , ac_v_bn_b = pure (Nothing,Nothing)
           , ac_R_n2b = pure $ pure (Just (-1.2), Just 1.2)
           , ac_w_bn_b = pure (Just (-8*2*pi), Just (8*2*pi))
           , ac_u = ubnd
           }

d2r :: Floating a => a -> a
d2r d = d*pi/180

ubnd :: AcU (Maybe Double, Maybe Double)
ubnd =
  AcU
  ControlSurfaces { csElev = (Just (d2r (-10)), Just (d2r 10))
                  , csRudder = (Just (d2r (-10)), Just (d2r 10))
                  , csAil = (Just (d2r (-10)), Just (d2r 10))
                  , csFlaps = (Just (d2r (-0.01)), Just (d2r 0.01))
                  }

bc :: Floating a => AcX a -> AcX a -> None a -> None a -> None a -> a -> AcX a
bc (AcX x0 v0 dcm0 w0 cs) _ _ _ _ _ = AcX x0 (v0 - V3 30 0 0) (dcm0 - eye3') w0 cs

eye3' :: Num a => M33 a
eye3' =
  V3
  (V3 1 0 0)
  (V3 0 1 0)
  (V3 0 0 1)

dirCollOpts :: DirCollOptions
dirCollOpts =
  def
  { mapStrategy = Unroll
  , collocationRoots = Legendre
  }

main :: IO ()
main = do
  let guess = jfill 1 :: J (CollTraj' GliderOcp NCollStages CollDeg) (Vector Double)
  cp <- makeCollProblem dirCollOpts ocp ocpInputs guess
  let nlp = cpNlp cp
  withCallback $ \send -> do
    let meta = toMeta (cpMetaProxy cp)

        cb' traj _ _ = do
          plotPoints <- cpPlotPoints cp traj (catJV None)
          send (plotPoints, meta)

    (_, eopt) <- solveNlp ipoptSolver nlp (Just cb')
    case eopt of
      Left msg -> putStrLn $ "optimization failed, message: " ++ msg
      Right _ -> putStrLn "optimization succeeded"
--    let xopt = xOpt opt
--        lambda = lambdaOpt opt
--
--    snoptOpt' <- solveNlp snoptSolver (nlp {nlpX0 = xopt}) (Just cb) (Just lambda)
--    snoptOpt <- case snoptOpt' of Left msg -> error msg
--                                  Right opt'' -> return opt''
--    let xopt' = xOpt snoptOpt
--        lambda' = lambdaOpt opt
--        lambdax' = vectorize $ lambdaX lambda'
--        lambdag' = vectorize $ lambdaG lambda'
--    _ <- solveSqp (nlp {nlpX0 = xopt}) fullStep
--    _ <- solveSqp (nlp {nlpX0 = xopt}) armilloSearch

    return ()
