{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module will implement a very simple extended Kalman Filter.
-- It is a work in progress.
-- Only Euler integration is available at the moment.
--
-- TODO(greg):
-- * More integrator options than Euler.
-- * Explicitely allow control inputs.
module Dyno.KalmanFilter
       ( KFModel(..)
       , KFRunner
       , compileModel
       , KFState
       , getState
       , getSigmas
       , kfEulerPropagate
       , kfSensorUpdate
       , initKFState
       ) where

import Casadi.DM ( DM )
import Casadi.Matrix ( SMatrix )
import Dyno.View
import System.IO.Unsafe ( unsafePerformIO )

getState :: Vectorize x => KFState x -> x Double
getState = splitJV . d2v . kfsState

getSigmas :: Vectorize x => KFState x -> x Double
getSigmas = fmap sqrt . splitJV . d2v . takeDiag . kfsCovariance

data KFState x
  = KFState
    { kfsState :: J (JV x) DM
    , kfsCovariance :: M (JV x) (JV x) DM
    } deriving Show

data KFRunner x q y ym
  = KFRunner
    { evalOde :: J (JV x) DM -> (M (JV x) (JV x) DM, M (JV x) (JV q) DM, J (JV x) DM)
    , evalSensors :: J (JV x) DM -> J (JV ym) DM -> (M (JV y) (JV x) DM, J (JV y) DM)
    }

data KFModel x q y ym a
  = KFModel
    { kfOde :: x a -> q a -> x a
    , kfSensors :: x a -> ym a -> y a
    }

compileModel ::
  forall x q y ym a
  . (Vectorize x, Vectorize q, Vectorize y, Vectorize ym, SMatrix a)
  => KFModel x q y ym (S a)
  -> IO (KFRunner x q y ym)
compileModel model = do
  let odeX :: J (JV x) a -> (M (JV x) (JV x) :*: J (JV x)) a
      odeX x = jacobian xdot x :*: xdot
        where
          xdot = vcat $ kfOde model (vsplit x) (fill 0)
  jacOdeX <- toFun "ode_x" odeX mempty

  let odeQ :: (J (JV x) :*: J (JV q)) a -> M (JV x) (JV q) a
      odeQ (x :*: q) = jacobian xdot q
        where
          xdot = vcat $ kfOde model (vsplit x) (vsplit q)
  jacOdeQ <- toFun "ode_q" odeQ mempty

  let evalOde' :: J (JV x) DM -> (M (JV x) (JV x) DM, M (JV x) (JV q) DM, J (JV x) DM)
      evalOde' x = unsafePerformIO $ do
        dxdx :*: xdot <- callDM jacOdeX x
        let q :: J (JV q) DM
            q = 0
        dxdq <- callDM jacOdeQ (x :*: q)

        return (dxdx, dxdq, xdot)

  let sensors :: (J (JV x) :*: J (JV ym)) a -> (M (JV y) (JV x) :*: J (JV y)) a
      sensors (x :*: ym) = jacobian y x :*: y
        where
          y = vcat $ kfSensors model (vsplit x) (vsplit ym)

  jacSensors <- toFun "sensors" sensors mempty

  let evalSensors' :: J (JV x) DM -> J (JV ym) DM -> (M (JV y) (JV x) DM, J (JV y) DM)
      evalSensors' x ym = unsafePerformIO $ do
        dydx :*: y <- callDM jacSensors (x :*: ym)
        return (dydx, y)

  return
    KFRunner
    { evalOde = evalOde'
    , evalSensors = evalSensors'
    }

initKFState :: Vectorize x => x Double -> Either (x Double) (M (JV x) (JV x) DM) -> KFState x
initKFState x0 eitherCov =
  KFState
  { kfsState = v2d (catJV x0)
  , kfsCovariance = case eitherCov of
      Right r -> r
      Left r -> diag $ v2d (catJV r)
  }

kfEulerPropagate ::
  forall x q y ym
  . (Vectorize x, Vectorize q)
  => KFRunner x q y ym -> Either (q Double) (M (JV q) (JV q) DM) -> Double -> KFState x -> KFState x
kfEulerPropagate sys eitherQc dt' kf0 =
  KFState
  { kfsState = x0 + dt `sm` dxdt
  , kfsCovariance = 0.5 `sm` (p1 + trans p1) -- make sure it's symmetric
  }
  where
    p1 = fpf + qd

    dt = v2d $ catJV (Id dt')

    dxdx' :: M (JV x) (JV x) DM
    dxdq' :: M (JV x) (JV q) DM
    dxdt :: J (JV x) DM
    (dxdx', dxdq', dxdt) = evalOde sys x0

    -- continuous to discrete
    dxdx = eye + dt `sm` dxdx'
    dxdq = dt `sm` dxdq'

    qd :: M (JV x) (JV x) DM
    qd = dxdq `mm` qc `mm` (trans dxdq) `ms` (1 / dt)

    fpf :: M (JV x) (JV x) DM
    fpf = dxdx `mm` p0 `mm` (trans dxdx)
      where
        p0 :: M (JV x) (JV x) DM
        p0 = kfsCovariance kf0


    qc = case eitherQc of
      Right r -> r
      Left r -> diag (v2d (catJV r))

    x0 :: J (JV x) DM
    x0 = kfsState kf0


kfSensorUpdate ::
  forall x q y ym
  . (Vectorize x, Vectorize y, Vectorize ym)
  => KFRunner x q y ym -> y Double -> ym Double -> Either (y Double) (M (JV y) (JV y) DM) -> KFState x -> KFState x
kfSensorUpdate sys y' yMeta' eitherRCov kf0 =
  KFState
  { kfsState = x0 + k `mm` (y - yhat)
  , kfsCovariance = 0.5 `sm` (p1 + trans p1) -- make sure it's symmetric
  }
  where
    y = v2d $ catJV y'
    yMeta = v2d $ catJV yMeta'

    yhat :: J (JV y) DM
    dydx :: M (JV y) (JV x) DM
    (dydx, yhat) = evalSensors sys x0 yMeta

    hph' :: M (JV y) (JV y) DM
    hph' = dydx `mm` p0 `mm` (trans dydx)

    s = hph' + rCovariance

    rCovariance = case eitherRCov of
      Right r -> r
      Left r -> diag (v2d (catJV r))

    k :: M (JV x) (JV y) DM
    k = p0 `mm` (trans dydx) `mm` (inv s)

    p1 = (eye - k `mm` dydx) `mm` p0

    p0 :: M (JV x) (JV x) DM
    p0 = kfsCovariance kf0

    x0 :: J (JV x) DM
    x0 = kfsState kf0
