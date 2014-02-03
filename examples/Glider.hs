{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module Main ( main ) where

import Linear

import Hascm.Vectorize
import Hascm.Ipopt.Ipopt
--import Hascm.Snopt
--import Hascm.Sqp.Sqp
--import Hascm.Sqp.LineSearch
import Hascm.Nlp

import Hascm.Ocp
import Hascm.DirectCollocation
import Hascm.DirectCollocation.Dynamic ( toMeta, ctToDynamic )

import Hascm.Models.Aircraft
import Hascm.Models.AeroCoeffs
import Hascm.Models.Betty
import Hascm.Nats

import GliderShared
import ServerSender ( withCallback )

type NCollStages = D100
type CollDeg = D2

type GliderDesignVars a = CollTraj AcX None AcU None NCollStages CollDeg a

mayer :: Num a => AcX a -> a -> a
mayer _ _ = 0

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> None a -> a -> a
lagrange (AcX _ _ _ _ (AcU surfs)) _ (AcU surfs') _ _ _ =
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

dae :: Floating a => Dae AcX None AcU None AcX None a
dae x' x _ u _ _ = (aircraftDae (mass, inertia) fcs mcs refs x' x u, None)
  where
    mass = bettyMass
    inertia = bettyInertia
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: OcpPhase AcX None AcU None AcX None AcX None
ocp = OcpPhase { ocpMayer = mayer
               , ocpLagrange = lagrange
               , ocpDae = dae
               , ocpBc = bc
               , ocpPathC = pathc
               , ocpPathCBnds = None -- pathcb
               , ocpXbnd = xbnd
               , ocpUbnd = ubnd
               , ocpZbnd = None
               , ocpPbnd = None
               , ocpTbnd = (Just 0.5, Just 0.5)
--               , ocpTbnd = (Just 4, Just 4)
               }

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

------pathcb :: None a
------pathcb = None
--

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

bc :: Floating a => AcX a -> AcX a -> AcX a
bc (AcX x0 v0 dcm0 w0 cs) _ = AcX x0 (v0 - V3 30 0 0) (dcm0 - eye3) w0 cs

main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""

  withCallback gliderUrl gliderChannelName $ \cb -> do
    let guess :: GliderDesignVars Double
        guess = fill 1

        cb' traj = cb (ctToDynamic traj, toMeta traj)
        nlp = (makeCollNlp ocp) { nlpX0 = guess }

    opt' <- solveNlpIpopt nlp (Just cb')
    opt <- case opt' of Left msg -> error msg
                        Right opt'' -> return opt''
--    let xopt = xOpt opt
--        lambda = lambdaOpt opt
--
--    snoptOpt' <- solveNlpSnopt (nlp {nlpX0 = xopt}) (Just cb) (Just lambda)
--    snoptOpt <- case snoptOpt' of Left msg -> error msg
--                                  Right opt'' -> return opt''
--    let xopt' = xOpt snoptOpt
--        lambda' = lambdaOpt opt
--        lambdax' = vectorize $ lambdaX lambda'
--        lambdag' = vectorize $ lambdaG lambda'
--    _ <- solveSqp (nlp {nlpX0 = xopt}) fullStep
--    _ <- solveSqp (nlp {nlpX0 = xopt}) armilloSearch

    return ()
