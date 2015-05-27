-- This example is based on Fabian Wierer's final project for
-- Optimal Control and Estimation, 2014, taught by Prof. Moritz Diehl
-- Used with permission.

{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language DataKinds #-}

module Main ( main
            , SbX(..) -- to suppress warnings about unused record names
            , SbU(..) -- to suppress warnings about unused record names
            ) where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )
import qualified System.ZMQ4 as ZMQ
import Linear -- ( V2(..) )
import qualified Data.List.NonEmpty as NE
--import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Binary as B
import qualified Data.Serialize as Ser
import Text.Printf ( printf )

import Accessors ( Lookup )

import Dyno.Vectorize
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( catJV, splitJV )
import Dyno.Solvers
import Dyno.NlpUtils
import Dyno.Nlp ( NlpOut(..) )
import Dyno.Ocp
import Dyno.DirectCollocation.Formulate
import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )
import Dyno.DirectCollocation.Dynamic

data SailboatOcp
type instance X SailboatOcp = SbX
type instance Z SailboatOcp = SbZ
type instance U SailboatOcp = SbU
type instance P SailboatOcp = SbP
type instance R SailboatOcp = SbR
type instance O SailboatOcp = SbO
type instance C SailboatOcp = SbBc
type instance H SailboatOcp = None
type instance Q SailboatOcp = None
type instance QO SailboatOcp = None
type instance FP SailboatOcp = None
type instance PO SailboatOcp = None

data SbX a = SbX { xGamma :: a
                 , xP :: V2 a
                 , xV :: V2 a
                 } deriving (Functor, Generic, Generic1, Show)
data SbZ a = SbZ deriving (Functor, Generic, Generic1, Show)
data SbU a = SbU { uOmega :: a
                 , uAlpha :: a
                 } deriving (Functor, Generic, Generic1, Show)
data SbP a = SbP deriving (Functor, Generic, Generic1, Show)
data SbR a = SbR (SbX a) deriving (Functor, Generic, Generic1, Show)
data SbO a = SbO { oFla :: V2 a
                 , oFda :: V2 a
                 , oFlw :: V2 a
                 , oFdw :: V2 a
                 , oAirspeed :: a
                 , oWaterspeed :: a
                 , oAlphaDeg :: a
                 , oOmegaDeg :: a
                 , oGammaDeg :: a
                 } deriving (Functor, Generic, Generic1, Show)

instance Vectorize SbX
instance Vectorize SbZ
instance Vectorize SbU
instance Vectorize SbP
instance Vectorize SbR
instance Vectorize SbO
instance Vectorize SbBc

instance Lookup (SbX ())
instance Lookup (SbZ ())
instance Lookup (SbU ())
instance Lookup (SbP ())
instance Lookup (SbO ())

------------------------------ zmq helpers -------------------------------------
newtype Packed = Packed { unPacked :: BS.ByteString }

encodeSerial :: Ser.Serialize a => a -> Packed
encodeSerial = Packed . Ser.encode

--------------------------------------------------------------------------
norm2sqr :: Num a => V2 a -> a
norm2sqr (V2 x y) = x*x + y*y

clift :: Floating a => a -> a
clift alpha = 2*pi*alpha*10/12 - exp (alpha/pi*180 - 12) + exp (-alpha/pi*180 - 12)

sbDae :: forall a . Floating a => SbX a -> SbX a -> SbZ a -> SbU a -> SbP a -> None a -> a -> (SbR a, SbO a)
sbDae
  (SbX gamma' p' v')
  (SbX gamma  _ v@(V2 vx vz))
  _
  (SbU omega alpha)
  _
  _
  _
  = (residual, outputs)
  where
    residual :: SbR a
    residual = SbR $
               SbX
               (gamma' - omega)
               (p' - v)
               (v' - (fmap (/mB) f))
    outputs :: SbO a
    outputs = SbO { oFla = fLa
                  , oFda = fDa
                  , oFlw = fLw
                  , oFdw = fDw
                  , oAirspeed = airspeed
                  , oWaterspeed = waterspeed
                  , oAlphaDeg = alpha * 180/pi
                  , oOmegaDeg = omega * 180/pi
                  , oGammaDeg = gamma * 180/pi
                  }

    mB = 160 + 70 -- boat's mass + sailor's mass
    w = V2 (-5) 0
    we@(V2 wex wez) = w ^-^ v
    rhoAir = 1.2
    airspeed2 = norm2sqr we
    airspeed = sqrt airspeed2
    liftDirectionAir = (V2 wez (-wex)) ^/ airspeed
    
    srefSail = 10 + 6 -- main + fock sail
    clSail = clift alpha
    cdSail = 0.01 + clSail*clSail/(10*pi)
    dragDirectionAir = we ^/ airspeed
    fDa = 0.5 * rhoAir * airspeed2 * cdSail * srefSail *^ dragDirectionAir
    fLa = 0.5 * rhoAir * airspeed2 * clSail * srefSail *^ liftDirectionAir

    waterspeed2 = norm2sqr v
    waterspeed = sqrt waterspeed2

    finArea = afin + ahull
      where
        afin = 0.3 * 0.7 -- area of fin
        ahull = 0.2 * 5 -- submerged part of hull: Length * submerged height

    rhoWater = 1000
    clFin = clift gamma
    cdFin = 0.01 + clFin*clFin/(10*pi)
    liftDirectionWater :: V2 a
    liftDirectionWater = (V2 (-vz) vx) ^/ waterspeed
    dragDirectionWater = (-v) ^/ waterspeed
    fLw = 0.5 * rhoWater * waterspeed2 * clFin * finArea *^ liftDirectionWater
    fDw = 0.5 * rhoWater * waterspeed2 * cdFin * finArea *^ dragDirectionWater
    f = fDa + fLa + fDw + fLw


data SbBc a  = SbBc { bcPeriodicGamma :: a
                    , bcPeriodicPz :: a
                    , bcPeriodicVx :: a
                    , bcPeriodicVz :: a
                    , bcP0 :: V2 a
                    }
                    deriving (Functor, Generic, Generic1, Show)
bc :: Num a => SbX a -> SbX a -> None a -> SbP a -> None a -> a -> SbBc a
bc
  (SbX gamma0 p0@(V2 _ pz0) (V2 vx0 vz0))
  (SbX gammaF    (V2 _ pzF) (V2 vxF vzF))
  _
  _
  _
  _
  = SbBc
    { bcPeriodicGamma = gamma0 + gammaF
    , bcPeriodicPz = pz0 - pzF
    , bcPeriodicVx = vx0 - vxF
    , bcPeriodicVz = vz0 + vzF
    , bcP0 = p0
    }

mayer :: Floating a => a -> SbX a -> SbX a -> None a -> SbP a -> None a -> a
mayer tf _ (SbX _ (V2 pxF _) _) _ _ _ = - pxF / tf

lagrange :: Floating a => SbX a -> SbZ a -> SbU a -> SbP a -> None a -> SbO a -> a -> a -> a
lagrange _ _ (SbU omega alpha) _ _ _ _ _ = 1e-3*omega*omega + 1e-3*alpha*alpha

ubnd :: SbU (Maybe Double, Maybe Double)
ubnd = SbU
       (Just (-5*pi/180), Just (5*pi/180))
       (Just (-12*pi/180), Just (12*pi/180))

xbnd :: SbX (Maybe Double, Maybe Double)
xbnd = SbX
       (Just (-12*pi/180), Just (12*pi/180))
       (V2
       (Just (-1000), Just 1000)
       (Just (-30), Just 30))
       (V2
       (Just (-100), Just 100)
       (Just (-100), Just 100))

pathc :: t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> None a
pathc _ _ _ _ _ _ _ = None

ocp :: OcpPhase' SailboatOcp
ocp =
  OcpPhase
  { ocpMayer = mayer
  , ocpLagrange = lagrange
  , ocpQuadratures = \_ _ _ _ _ _ _ _ -> None
  , ocpQuadratureOutputs = \_ _ _ _ _ _ _ _ -> None
  , ocpDae = sbDae
  , ocpBc = bc
  , ocpPlotOutputs = \_ _ _ _ _ _ _ _ _ _ -> None
  , ocpPathC = pathc
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

ocpInputs :: OcpPhaseInputs' SailboatOcp
ocpInputs =
  OcpPhaseInputs
  { ocpPathCBnds = None
  , ocpBcBnds = fill (Just 0, Just 0)
  , ocpXbnd = xbnd
  , ocpUbnd = ubnd
  , ocpZbnd = SbZ
  , ocpPbnd = SbP
  , ocpTbnd = (Just 1, Just 50)
  , ocpFixedP = None
  }



urlDynoPlot :: String
urlDynoPlot = "tcp://127.0.0.1:5563"

--urlOptTelem :: String
--urlOptTelem = "tcp://127.0.0.1:5563"

withPublisher
  :: ZMQ.Context -> String -> ((String -> Packed -> IO ()) -> IO a) -> IO a
withPublisher context url f =
  ZMQ.withSocket context ZMQ.Pub $ \publisher -> do
    ZMQ.bind publisher url
    let send :: String -> Packed -> IO ()
        send channel msg =
          ZMQ.sendMulti publisher (NE.fromList [ BS8.pack channel
                                               , unPacked msg
--                                               , BSL.toStrict (unPacked msg)
                                               ])
    f send

initialGuess :: CollTraj' SailboatOcp NCollStages CollDeg (Vector Double)
initialGuess = makeGuess Legendre tf guessX (const SbZ) guessU SbP
  where
    tf = 20
    r = 30

    guessU _ = SbU 0 0
    guessX t = SbX 0
               (V2 (r - r*cos(w*t)) (r*sin(w*t)))
               (V2 (w*r*sin(w*t)) (w*r*cos(w*t)))
      where
        w = pi/tf

type NCollStages = 200
type CollDeg = 2

solver :: Solver
solver = ipoptSolver
--solver = snoptSolver { options = [("detect_linear", Opt False)] }

main :: IO ()
main = do
  cp <- makeCollProblem Legendre ocp ocpInputs (cat initialGuess)
  let nlp = cpNlp cp
  ZMQ.withContext $ \context ->
    withPublisher context urlDynoPlot $ \sendDynoPlotMsg -> do
--    withPublisher context urlOptTelem $ \sendOptTelemMsg -> do
      let meta = toMeta (cpMetaProxy cp)

          callback :: J (CollTraj' SailboatOcp NCollStages CollDeg) (Vector Double) -> b -> IO Bool
          callback traj _ = do
            plotPoints <- cpPlotPoints cp traj (catJV None)
            -- dynoplot
            let dynoPlotMsg = encodeSerial (plotPoints, meta)
            sendDynoPlotMsg "glider" dynoPlotMsg

--            -- 3d vis
--            let CollTraj tf' _ _ stages' xf = split traj
--                stages :: [(CollStage (JV SbX) (JV None) (JV SbU) CollDeg) (Vector Double)]
--                stages = map split $ F.toList $ unJVec (split stages')
--
--                states :: [SbX Double]
--                states = concatMap stageToXs stages ++ [jToX xf]
--
--                stageToXs :: CollStage (JV SbX) (JV None) (JV SbU) CollDeg (Vector Double)
--                             -> [SbX Double]
--                stageToXs (CollStage x0 xzus) = [jToX x0]
----                stageToXs (CollStage x0 xzus) = jToX x0 : map getX points
----                  where
----                    points :: [CollPoint (JV SbX) (JV None) (JV SbU) (Vector Double)]
----                    points = map split (F.toList (unJVec (split xzus)))
--
--                jToX = fmap V.head . unJV . split
--
--                getX :: CollPoint (JV SbX) (JV None) (JV SbU) (Vector Double) -> SbX Double
--                getX (CollPoint x _ _) = jToX x
--
--                poses :: [Msg.SbPose]
--                poses = map stateToPose states
--
--                stateToPose :: SbX Double -> Msg.SbPose
--                stateToPose acX = Msg.SbPose
--                                    (toXyz (ac_r_n2b_n acX))
--                                    (toDcmMsg (ac_R_n2b acX))
--                tf :: Double
--                tf = (V.head . unS . split) tf'
--                msgs = [printf "final time: %.2f" tf]
--                optTelemMsg = Msg.OptTelem (S.fromList poses) (S.fromList (map PB.fromString msgs))
--
--            sendOptTelemMsg "opt_telem" (encodeProto optTelemMsg)
            return True

      (msg0,opt0') <- solveNlp solver nlp (Just callback)
      opt0 <- case msg0 of Left msg' -> error msg'
                           Right _ -> return opt0'
      let CollTraj endTime' _ _ xf = split (xOpt opt0)
          endTime = unId $ splitJV endTime'
          V2 pxF _ = xP $ splitJV xf
      printf "optimal velocity: %.2f m/s\n" (pxF / endTime)
