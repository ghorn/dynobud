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
import Nlp

data SpringX a = SpringX a a deriving (Functor, Generic1, Show)
data SpringU a = SpringU a deriving (Functor, Generic1, Show)
instance Vectorize SpringX D2
instance Vectorize SpringU D1

meyer :: Num a => t -> a
meyer _ = 0

lagrange :: Floating a => SpringX a -> SpringU a -> a
lagrange (SpringX x v) (SpringU u) = x**2 + 2*v**2 + 10*u**2

springOde :: Floating a => ExplicitOde SpringX SpringU a
springOde (SpringX x v) (SpringU u) = SpringX (x + ts*v) (v + ts*acc)
  where
    acc = -k*x -b*v + u 
    k = 2.6
    b = 0.2
    ts = 1

springOcp :: Floating a => OcpPhase SpringX SpringU (Vec D4) None a
springOcp = OcpPhase meyer lagrange springOde bc pathc pathcb xbnd ubnd

pathc :: f -> g -> None a
pathc _ _ = None

pathcb :: None a
pathcb = None

xbnd :: SpringX (Maybe Double, Maybe Double)
xbnd = SpringX (Just (-10), Just (10)) (Just (-10), Just (10))

ubnd :: SpringU (Maybe Double, Maybe Double)
ubnd = SpringU (Just (-10), Just (10))

bc :: Num a => SpringX a -> SpringX a -> Vec D4 a
bc (SpringX x0 v0) (SpringX xf vf) = mkVec (V.fromList [x0-5,v0,xf-1,vf])


nlp :: Nlp (ExplEulerMsDvs SpringX SpringU D9) (ExplEulerMsConstraints SpringX D9 (Vec D4) None)
nlp = makeNlp springOcp

main :: IO ()
main = do
  xopt <- solveNlp nlp
  print xopt
