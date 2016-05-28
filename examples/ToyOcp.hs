{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic1 )

import Dyno.View.Vectorize
import Dyno.SimpleOcp

-- state
data X a = X { xTheta :: a, xOmega :: a }
         deriving (Functor, Generic1, Show)
instance Vectorize X

-- controls
data U a = U { uTorque :: a }
         deriving (Functor, Generic1, Show)
instance Vectorize U

pendOde :: Floating a => X a -> U a -> X a
pendOde (X theta omega) (U torque) = X thetaDot omegaDot
  where
    thetaDot = omega
    omegaDot = torque + 9.8 * sin theta

ocp :: SimpleOcp X U
ocp =
  SimpleOcp
  { ode = pendOde
  , objective = \(X _ omega) (U torque) -> omega * omega + torque * torque
  , xBounds = X (-pi, pi) (-5, 5)
  , uBounds = U (-50, 50)
  , xInitial = X {xTheta = pi/2, xOmega =  0}
  , xFinal = X {xTheta = 0, xOmega = 0}
  , endTime = 1
  , initialGuess = \t -> X ((1-t) * pi/2) (pi/1)
  }

main :: IO ()
main = do
  result <- solveOcp ocp
  case result of
    Left msg -> putStrLn $ "failed with " ++ msg
    Right xus -> print xus
