{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Main where

import qualified Data.Vector as V

import Vectorize
import TypeVecs ( Vec(..), mkVec )
import TypeNats

import Ocp
import Dae
import MultipleShooting
import Nlp

data SpringX a = SpringX a a deriving (Functor, Generic1, Show)
data SpringU a = SpringU a deriving (Functor, Generic1, Show)
instance Vectorize SpringX
instance Vectorize SpringU

mayer :: Num a => t -> a
mayer _ = 0

lagrange :: Floating a => SpringX a -> None a -> SpringU a -> None a -> a
lagrange (SpringX x v) None (SpringU u) None = x**2 + 2*v**2 + 10*u**2

springOde :: Floating a => ExplicitOde SpringX SpringU None a
springOde (SpringX x v) None (SpringU u) None = SpringX v acc
  where
    acc = -k*x -b*v + u 
    k = 2.6
    b = 0.2

springOcp :: Floating a => OcpPhase SpringX None SpringU None SpringX (Vec D4) None a
springOcp = OcpPhase { ocpMayer = mayer
                     , ocpLagrange = lagrange
                     , ocpDae = springOde
                     , ocpBc = bc
                     , ocpPathC = pathc
                     , ocpPathCBnds = pathcb
                     , ocpXbnd = xbnd
                     , ocpUbnd = ubnd
                     , ocpZbnd = None
                     , ocpPbnd = None
                     , ocpTbnd = (10, 10)
                     }

pathc :: f -> g -> h -> k -> None a
pathc _ _ _ _ = None

pathcb :: None a
pathcb = None

xbnd :: SpringX (Maybe Double, Maybe Double)
xbnd = SpringX (Just (-10), Just (10)) (Just (-10), Just (10))

ubnd :: SpringU (Maybe Double, Maybe Double)
ubnd = SpringU (Just (-10), Just (10))

bc :: Num a => SpringX a -> SpringX a -> Vec D4 a
bc (SpringX x0 v0) (SpringX xf vf) = mkVec (V.fromList [x0-5,v0,xf-1,vf])

nlp :: Nlp (MsTraj SpringX SpringU None D9) (MsConstraints SpringX D9 (Vec D4) None)
nlp = makeNlp springOcp forwardEuler

main :: IO ()
main = do
  xopt <- solveNlp nlp
  print xopt
