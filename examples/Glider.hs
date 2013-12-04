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

import qualified Data.Vector as V
import Data.Serialize

-- #if OSX
-- import qualified System.ZMQ3 as ZMQ
-- #else
import qualified System.ZMQ as ZMQ
-- #endif

import Linear
import Data.ByteString.Char8 ( pack )

import Hascm.Vectorize
--import Hascm.TypeVecs
import Hascm.Nats
import Hascm.Ipopt
--import Hascm.Snopt
--import Hascm.Sqp.Sqp
--import Hascm.Sqp.LineSearch
--import qualified Hascm.Nlp as Nlp

import Hascm.Ocp
import Hascm.DirectCollocation

import Hascm.Models.Aircraft
import Hascm.Models.AeroCoeffs
import Hascm.Models.Betty

import GliderTypes

mayer :: Num a => AcX a -> a -> a
mayer _ _ = 0

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> a -> a
lagrange (AcX _ _ _ _ (AcU surfs)) _ (AcU surfs') _ _ =
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

dae :: Floating a => Dae AcX None AcU None AcX a
dae x' x _ u _ _ = aircraftDae (mass, inertia) fcs mcs refs x' x u
  where
    mass = bettyMass
    inertia = bettyInertia
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: Floating a => OcpPhase AcX None AcU None AcX AcX None a
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

pathc :: x a -> z a -> u a -> p a -> a -> None a
pathc _ _ _ _ _ = None

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
  AcU $
  ControlSurfaces { csElev = (Just (d2r (-10)), Just (d2r 10))
                  , csRudder = (Just (d2r (-10)), Just (d2r 10))
                  , csAil = (Just (d2r (-10)), Just (d2r 10))
                  , csFlaps = (Just (d2r (-0.01)), Just (d2r 0.01))
                  }

bc :: Floating a => AcX a -> AcX a -> AcX a
bc (AcX x0 v0 dcm0 w0 cs) _ = AcX x0 (v0 - V3 30 0 0) (dcm0 - eye3) w0 cs


type NCollStages = D10
type CollDeg = D3

callback :: ZMQ.Socket ZMQ.Pub -> String -> GliderDesignVars Double -> IO Bool
callback publisher chanName traj = do
  let bs = encode $ V.toList $ vectorize $ traj
  ZMQ.send publisher (pack chanName) [ZMQ.SndMore]
  ZMQ.send publisher bs []
  return True

main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""
  
  ZMQ.withContext 1 $ \context -> do
    ZMQ.withSocket context ZMQ.Pub $ \publisher -> do
      ZMQ.bind publisher gliderUrl

      let guess :: GliderDesignVars Double
          guess = fill 1

          cb = callback publisher gliderChannelName

      opt' <- solveNlpIpopt (makeCollNlp ocp) guess None (Just cb)
      opt <- case opt' of Left msg -> error msg
                          Right opt'' -> return opt''
      --let xopt = Nlp.xOpt opt
      --    lambda = Nlp.lambdaOpt opt
      --_ <- solveNlpSnopt (makeCollNlp ocp) (Just cb) xopt None (Just lambda)
      --_ <- solveSqp (makeCollNlp ocp) fullStep xopt None
      --_ <- solveSqp (makeCollNlp ocp) armilloSearch xopt None
      
      return ()

--  (Right xopt) <- solveCollNlp pendOcp Nothing :: IO (CollTraj AcX AcZ AcU AcP NCollStages CollDeg Double)
