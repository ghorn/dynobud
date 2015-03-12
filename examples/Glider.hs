{-# OPTIONS_GHC -Wall #-}
{-# Language DataKinds #-}

module Main ( main ) where

import Data.Proxy ( Proxy(..) )
import Linear
import Data.Vector ( Vector )

import Dyno.Vectorize
import Dyno.View.View
import Dyno.Solvers
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch
import Dyno.Nlp
import Dyno.NlpUtils

import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.DirectCollocation.Dynamic ( toMeta )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )

import Glider.Aircraft
import Glider.AeroCoeffs
import Glider.Betty

import Dynoplot.Callback ( withCallback )

type NCollStages = 100
type CollDeg = 2

mayer :: Floating a => a -> AcX a -> AcX a -> None a -> None a -> a
mayer _ _ _ _ _ = 0

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> None a -> a -> a -> a
lagrange (AcX _ _ _ _ (AcU surfs)) _ (AcU surfs') _ _ _ _ =
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

dae :: Floating a => AcX a -> AcX a -> None a -> AcU a -> None a -> a -> (AcX a, None a)
dae x' x _ u _ _ = (aircraftDae (mass, inertia) fcs mcs refs x' x u, None)
  where
    mass = bettyMass
    inertia = bettyInertia
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: OcpPhase AcX None AcU None AcX None AcX None None
ocp = OcpPhase { ocpMayer = mayer
               , ocpLagrange = lagrange
               , ocpQuadratures = \_ _ _ _ _ _ _ -> None
               , ocpDae = dae
               , ocpBc = bc
               , ocpPathC = pathc
               , ocpPathCBnds = None
               , ocpBcBnds = fill (Just 0, Just 0)
               , ocpXbnd = xbnd
               , ocpUbnd = ubnd
               , ocpZbnd = None
               , ocpPbnd = None
               , ocpTbnd = (Just 0.5, Just 0.5)
--               , ocpTbnd = (Just 4, Just 4)
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

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

xbnd :: AcX (Maybe Double, Maybe Double)
xbnd = AcX { ac_r_n2b_n = fill (Nothing, Nothing)
           , ac_v_bn_b = fill (Nothing,Nothing)
           , ac_R_n2b = fill $ fill (Just (-1.2), Just 1.2)
           , ac_w_bn_b = fill (Just (-8*2*pi), Just (8*2*pi))
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

bc :: Floating a => AcX a -> AcX a -> None a -> None a -> a -> AcX a
bc (AcX x0 v0 dcm0 w0 cs) _ _ _ _ = AcX x0 (v0 - V3 30 0 0) (dcm0 - eye3) w0 cs

main :: IO ()
main = do
  cp <- makeCollProblem Legendre ocp
  let nlp = cpNlp cp
  withCallback $ \cb -> do
    let guess = jfill 1

        cb' :: J (CollTraj AcX None AcU None NCollStages CollDeg) (Vector Double) -> IO Bool
        cb' traj = do
          plotPoints <- cpPlotPoints cp traj
          let proxy :: Proxy (CollTraj AcX None AcU None NCollStages CollDeg)
              proxy = Proxy
          cb (plotPoints, toMeta (Proxy :: Proxy None) (Proxy :: Proxy None) proxy)

    (msg,_) <- solveNlp ipoptSolver (nlp { nlpX0 = guess }) (Just cb')
    case msg of Left msg' -> putStrLn $ "optimization failed, message: " ++ msg'
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
