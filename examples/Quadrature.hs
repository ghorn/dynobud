{-# OPTIONS_GHC -Wall #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}

module Main
       ( main
       , lolMahQ
       ) where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )
import Text.Printf ( printf )

import Accessors ( Lookup )

import Dyno.Vectorize ( Vectorize(..), None(..), Id(..) )
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( splitJV, catJV )
import Dyno.Solvers
import Dyno.Nlp ( NlpOut(..), Bounds )
import Dyno.NlpUtils
import Dyno.Ocp
import Dyno.DirectCollocation.Formulate
import Dyno.DirectCollocation.Types
--import Dyno.DirectCollocation.Types ( CollTraj(..) )
import Dyno.DirectCollocation.Dynamic ( toMeta )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )

import Dynoplot.Callback ( withCallback )


data QuadOcp
type instance X QuadOcp = QuadX
type instance Z QuadOcp = QuadZ
type instance U QuadOcp = QuadU
type instance P QuadOcp = QuadP
type instance R QuadOcp = QuadR
type instance O QuadOcp = QuadO
type instance C QuadOcp = QuadBc
type instance H QuadOcp = None
type instance Q QuadOcp = QuadQ
type instance FP QuadOcp = None

data QuadX a = QuadX { xP  :: a
                     , xV  :: a
                     } deriving (Functor, Generic, Generic1, Show)
data QuadZ a = QuadZ  deriving (Functor, Generic, Generic1, Show)
data QuadU a = QuadU deriving (Functor, Generic, Generic1, Show)
data QuadP a = QuadP deriving (Functor, Generic, Generic1, Show)
data QuadR a = QuadR (QuadX a) deriving (Functor, Generic, Generic1, Show)
data QuadO a = QuadO a deriving (Functor, Generic, Generic1, Show)
data QuadBc a = QuadBc (QuadX a) deriving (Functor, Generic, Generic1, Show)
data QuadQ a = QuadQ { lolMahQ :: a } deriving (Functor, Generic, Generic1, Show)

instance Vectorize QuadX
instance Vectorize QuadZ
instance Vectorize QuadU
instance Vectorize QuadP
instance Vectorize QuadR
instance Vectorize QuadO
instance Vectorize QuadBc
instance Vectorize QuadQ

instance Lookup (QuadX ())
instance Lookup (QuadZ ())
instance Lookup (QuadU ())
instance Lookup (QuadO ())
instance Lookup (QuadP ())
instance Lookup (QuadQ ())

mayer :: Num a => QuadOrLagrange -> a -> QuadX a -> QuadX a -> QuadQ a -> QuadP a -> None a -> a
mayer TestQuadratures _ _ _ (QuadQ qf) _ _ = qf
mayer TestLagrangeTerm _ _ _ _ _ _ = 0

data QuadOrLagrange = TestQuadratures | TestLagrangeTerm deriving Show
data StateOrOutput = TestState | TestOutput deriving Show

lagrange :: Num a => StateOrOutput -> QuadOrLagrange -> QuadX a -> QuadZ a -> QuadU a -> QuadP a -> None a -> QuadO a -> a -> a -> a
lagrange _ TestQuadratures _ _ _ _ _ _ _ _ = 0
lagrange TestState TestLagrangeTerm (QuadX _ v) _ _ _ _ _ _ _ = v
lagrange TestOutput TestLagrangeTerm _ _ _ _ _ (QuadO v) _ _ = v

quadratures :: Floating a =>
               StateOrOutput -> QuadX a -> QuadZ a -> QuadU a -> QuadP a -> None a -> QuadO a -> a -> a -> QuadQ a
quadratures TestState (QuadX _ v) _ _ _ _ _ _ _ = QuadQ v
quadratures TestOutput _ _ _ _ _ (QuadO v) _ _ = QuadQ v

dae :: Floating a => QuadX a -> QuadX a -> QuadZ a -> QuadU a -> QuadP a -> None a -> a -> (QuadR a, QuadO a)
dae (QuadX p' v') (QuadX _ v) _ _ _ _ _ = (residual, outputs)
  where
    residual =
      QuadR
      QuadX { xP = p' - v
            , xV = v' - alpha
            }
    outputs = QuadO v

alpha :: Fractional a => a
alpha = 1

tf :: Fractional a => a
tf = 10.0

quadOcp :: StateOrOutput -> QuadOrLagrange -> OcpPhase' QuadOcp
quadOcp stateOrOutput quadOrLag =
  OcpPhase
  { ocpMayer = mayer quadOrLag
  , ocpLagrange = lagrange stateOrOutput quadOrLag
  , ocpQuadratures = quadratures stateOrOutput
  , ocpDae = dae
  , ocpBc = bc
  , ocpPathC = pathc
  , ocpObjScale      = Nothing
  , ocpTScale        = Nothing
  , ocpXScale        = Nothing
  , ocpZScale        = Nothing
  , ocpUScale        = Nothing
  , ocpPScale        = Nothing
  , ocpResidualScale = Nothing
  , ocpBcScale       = Nothing
  , ocpPathCScale    = Just None
  }

quadOcpInputs :: OcpPhaseInputs' QuadOcp
quadOcpInputs =
  OcpPhaseInputs
  { ocpPathCBnds = None
  , ocpBcBnds = bcBnds
  , ocpXbnd = xbnd
  , ocpUbnd = ubnd
  , ocpZbnd = QuadZ
  , ocpPbnd = QuadP
  , ocpTbnd = (Just tf, Just tf)
  , ocpFixedP = None
  }

pathc :: Floating a => QuadX a -> QuadZ a -> QuadU a -> QuadP a -> None a -> QuadO a -> a -> None a
pathc _ _ _ _ _ _ _ = None

xbnd :: QuadX Bounds
xbnd = QuadX { xP =  (Nothing, Nothing)
             , xV =  (Nothing, Nothing)
             }

ubnd :: QuadU Bounds
ubnd = QuadU

bc :: Floating a => QuadX a -> QuadX a -> QuadQ a -> QuadP a -> None a -> a -> QuadBc a
bc x0 _ _ _ _ _ = QuadBc x0

bcBnds :: QuadBc Bounds
bcBnds =
  QuadBc
  (QuadX
   { xP = (Just 0, Just 0)
   , xV = (Just 0, Just 0)
   })

type NCollStages = 10
type CollDeg = 3

guess :: QuadratureRoots -> J (CollTraj' QuadOcp NCollStages CollDeg) (Vector Double)
guess roots = cat $ makeGuess roots tf guessX guessZ guessU parm
  where
    guessX _ = QuadX { xP = 0
                     , xV = 0
                     }
    guessZ _ = QuadZ
    guessU _ = QuadU
    parm = QuadP



solver :: Solver
solver = ipoptSolver { options = [ ("expand", Opt True)
                                 , ("linear_solver", Opt "ma86")
                                 , ("ma86_order", Opt "metis")
--                                 , ("print_level", Opt (0 :: Int))
--                                 , ("print_time", Opt False)
                                 ]}

goodSolution :: NlpOut
                (CollTraj QuadX QuadZ QuadU QuadP NCollStages CollDeg)
                (CollOcpConstraints QuadX QuadR QuadBc None NCollStages CollDeg)
                (Vector Double)
                -> String
goodSolution out = msg
  where
    msg = printf "    objective: %.4f, expected: %.4f" f fExpected
    fExpected = 0.5 * alpha * tf**2 :: Double
    Id f = splitJV (fOpt out)

compareIntegration :: (QuadratureRoots, StateOrOutput, QuadOrLagrange) -> IO ()
compareIntegration (roots, stateOrOutput, quadOrLag) = do
  withCallback $ \send -> do
    cp  <- makeCollProblem roots (quadOcp stateOrOutput quadOrLag) quadOcpInputs (guess roots)
    let nlp = cpNlp cp
        meta = toMeta (cpMetaProxy cp)
        cb traj _ = do
          plotPoints <- cpPlotPoints cp traj (catJV None)
          send (plotPoints, meta)
    (ret, out) <- solveNlp solver nlp (Just cb)
    case ret of
     Left msg -> return (error msg)
     Right _ -> putStrLn (goodSolution out)

main :: IO ()
main = do
  compareIntegration (Legendre, TestState, TestLagrangeTerm)
