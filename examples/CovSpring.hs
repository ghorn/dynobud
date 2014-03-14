{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Data.Vector as V


import Hascm.Vectorize
import Hascm.TypeVecs
import Hascm.Nats
import Hascm.Ipopt
--import Hascm.Snopt
--import Hascm.Sqp.Sqp
--import Hascm.Sqp.LineSearch
import Hascm.Nlp
import Hascm.NlpSolver
import Hascm.Server.Accessors

import Hascm.Ocp
import Hascm.Casadi.SXElement
import Hascm.Cov
import Hascm.DirectCollocation

data SprX a = SprX { pX :: a
                   , pVx :: a
                   } deriving (Functor, Generic, Generic1, Show)
data SprZ a = SprZ deriving (Functor, Generic, Generic1, Show)
data SprU a = SprU { pTorque :: a } deriving (Functor, Generic, Generic1, Show)
data SprP a = SprP { pMass :: a } deriving (Functor, Generic, Generic1, Show)
data SprR a = SprR a a deriving (Functor, Generic, Generic1, Show)
data SprO a = SprO deriving (Functor, Generic, Generic1, Show)
data SprH a = SprH deriving (Functor, Generic, Generic1, Show)
newtype SprC a = SprC (Vec D4 a) deriving (Functor, Generic, Generic1, Show)
newtype SprS a = SprS (SprX a) deriving (Functor, Generic, Generic1, Show)
data SprSh a = SprSh a
             deriving (Functor, Generic, Generic1, Show)
newtype SprSc a = SprSc (Vec D3 a) deriving (Functor, Generic, Generic1, Show)

instance Vectorize SprX
instance Vectorize SprZ
instance Vectorize SprU
instance Vectorize SprP
instance Vectorize SprR
instance Vectorize SprO
instance Vectorize SprH
instance Vectorize SprC
instance Vectorize SprS
instance Vectorize SprSh
instance Vectorize SprSc

instance (Lookup a, Generic a) => Lookup (SprX a)
instance (Lookup a, Generic a) => Lookup (SprZ a)
instance (Lookup a, Generic a) => Lookup (SprU a)

mayer :: Num a => SprX a -> a -> a
mayer _ _ = 0

lagrange :: Floating a => SprX a -> SprZ a -> SprU a -> SprP a -> SprO a -> a -> a
lagrange (SprX x vx) SprZ (SprU u) (SprP _) _ _ = x*x + vx*vx + 1e-4*u**2

r :: Floating a => a
r = 0.3

sprDae :: Floating a => Dae SprX SprZ SprU SprP SprR SprO a
sprDae (SprX x' vx') (SprX x vx) SprZ (SprU u) (SprP m) _ =
  (SprR (x' - vx) (vx' - (u - k*x - b*vx)/m), SprO)
  where
    k = 4
    b = 0.3

sprOcp :: OcpPhase SprX SprZ SprU SprP SprR SprO SprC SprH SprS SprSh SprSc
sprOcp = OcpPhase { ocpMayer = mayer
                  , ocpLagrange = lagrange
                  , ocpDae = sprDae
                  , ocpBc = bc
                  , ocpPathC = pathc
                  , ocpPathCBnds = SprH
                  , ocpXbnd = xbnd
                  , ocpUbnd = ubnd
                  , ocpZbnd = fill (Nothing, Nothing)
                  , ocpPbnd = fill (Just 0.3, Just 0.3)
                  , ocpTbnd = (Just 4, Just 10)

                  , ocpSq = devectorize (V.fromList [0.001, 0, 0.1])
                  , ocpSbnd = fill (Nothing,Nothing)
                  , ocpSc = \p0 _ -> let [p00,p01,p11] = V.toList (vectorize p0)
                                     in devectorize (V.fromList [p00,p01,p11])
                  , ocpScBnds = devectorize (V.fromList [(Just 1, Just 1), (Just 0, Just 0), (Just 1, Just 1)])
                  , ocpSh = \_ p -> let [p00,_,_] = V.toList (vectorize p)
                                    in SprSh p00
                  , ocpShBnds = SprSh (Just (-0.1), Nothing)
                  }

pathc :: x a -> z a -> u a -> p a -> o a -> a -> SprH a
pathc _ _ _ _ _ _ = SprH

xbnd :: SprX (Maybe Double, Maybe Double)
xbnd = SprX { pX =  (Just (-10), Just 10)
            , pVx = (Just (-10), Just 10)
            }

ubnd :: SprU (Maybe Double, Maybe Double)
ubnd = SprU (Just (-40), Just 40)

bc :: Floating a => SprX a -> SprX a -> SprC a
bc (SprX x0 vx0) (SprX xf vxf) =
  devectorize $
  V.fromList [ x0
             , vx0
             , xf - 1
             , vxf
             ]

type NCollStages = D10
type CollDeg = D2
--type NCollStages = D1
--type CollDeg = D1

guess :: CollTraj SprX SprZ SprU SprP SprS NCollStages CollDeg Double
guess = fill 1

main :: IO ()
main = do
  let nlp = (makeCollNlp sprOcp) { nlpX0 = guess }
  _ <- solveNlp ipoptSolver nlp Nothing
  --(Right nlpOut) <- solveNlp snoptSolver (makeCollNlp sprOcp) Nothing guess None Nothing
  --_ <- solveSqp (makeCollNlp sprOcp) armilloSearch (Nlp.xOpt nlpOut) None
  return ()


cov :: Cov (Vec D2) Double
cov = devectorize (V.fromList [1,2,3])

covs :: Cov (Vec D2) SXElement
covs = devectorize (V.fromList [1,2,3])
