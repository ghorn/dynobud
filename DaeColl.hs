{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Main where

--import qualified Data.Vector as V

import Vectorize
import TypeVecs
import TypeNats

import Ocp
import DirectCollocation
--import Nlp

data PendX a = PendX { pX :: a
                     , pY :: a
                     , pVx :: a
                     , pVy :: a
                     } deriving (Functor, Generic1, Show)
data PendZ a = PendZ { pTau :: a}  deriving (Functor, Generic1, Show)
data PendU a = PendU { pTorque :: a } deriving (Functor, Generic1, Show)
data PendP a = PendP { pMass :: a } deriving (Functor, Generic1, Show)
data PendR a = PendR a a a a a deriving (Functor, Generic1, Show)
instance Vectorize PendX
instance Vectorize PendZ
instance Vectorize PendU
instance Vectorize PendP
instance Vectorize PendR

meyer :: Num a => t -> a
meyer _ = 0

lagrange :: Fractional a => PendX a -> PendZ a -> PendU a -> PendP a -> a
lagrange (PendX x y vx vy) (PendZ tau) (PendU torque) (PendP m) = 0
--
--springOde :: Floating a => ExplicitOde SpringX SpringU None a
--springOde (SpringX x v) None (SpringU u) None = SpringX v acc
--  where
--    acc = -k*x -b*v + u 
--    k = 2.6
--    b = 0.2
r :: Fractional a => a
r = 0.3

pendDae :: Fractional a => Dae PendX PendZ PendU PendP PendR a
pendDae (PendX x' y' vx' vy') (PendX x y vx vy) (PendZ tau) (PendU torque) (PendP m) =
  PendR (x' - vx) (y' - vy)
  (m*vx' + x*tau - fx)
  (m*vy' + y*tau - fy)
  (x*vx' + y*vy' + (vx*vx + vy*vy))
  where
    fx =  torque*y
    fy = -torque*x + m*9.8
    
--    dae['c']    = dae['x']*dae['x'] + dae['z']*dae['z'] - r*r
--    dae['cdot'] = dae['dx']*dae['x'] + dae['dz']*dae['z']
    

pendOcp :: Fractional a => OcpPhase PendX PendZ PendU PendP PendR (Vec D4) None a
pendOcp = OcpPhase { ocpMeyer = meyer
                   , ocpLagrange = lagrange
                   , ocpDae = pendDae
                   , ocpBc = bc
                   , ocpPathC = pathc
                   , ocpPathCBnds = None -- pathcb
                   , ocpXbnd = xbnd
                   , ocpUbnd = ubnd
                   , ocpZbnd = fill (Nothing, Nothing)
                   , ocpPbnd = fill (Just 0.3, Just 0.3)
                   , ocpTbnd = (10, 10)
                   }

pathc :: x a -> z a -> u a -> p a -> None a
pathc _ _ _ _ = None

----pathcb :: None a
----pathcb = None
----
xbnd :: PendX (Maybe Double, Maybe Double)
xbnd = PendX (Just (-10), Just (10)) (Just (-10), Just (10)) (Just (-1), Just (1)) (Just (-1), Just (1))

ubnd :: PendU (Maybe Double, Maybe Double)
ubnd = PendU (Just (-10), Just (10))

bc :: Num a => PendX a -> PendX a -> Vec D4 a
bc (PendX x0 y0 vx0 vy0) (PendX xf yf vxf vyf) = mkVec' [x0, vx0, y0, vy0]

----nlp :: Nlp (MsTraj SpringX SpringU None D9) (MsConstraints SpringX D9 (Vec D4) None)
----nlp = makeNlp springOcp forwardEuler

main :: IO ()
main = do
  xopt <- solveCollNlp pendOcp :: IO (CollTraj PendX PendZ PendU PendP D10 D3 Double)
  print (vectorize xopt)
