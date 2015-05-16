{-# OPTIONS_GHC -Wall #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module QuadratureTests
       ( quadratureTests
       ) where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Text.Printf ( printf )

import Dyno.Vectorize ( Vectorize(..), None(..), Id(..) )
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( splitJV )
import Dyno.Solvers
import Dyno.Nlp ( NlpOut(..), Bounds )
import Dyno.NlpUtils
import Dyno.Ocp
import Dyno.DirectCollocation.Formulate
import Dyno.DirectCollocation.Types
--import Dyno.DirectCollocation.Types ( CollTraj(..) )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )




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
data QuadQ a = QuadQ a deriving (Functor, Generic, Generic1, Show)

instance Vectorize QuadX
instance Vectorize QuadZ
instance Vectorize QuadU
instance Vectorize QuadP
instance Vectorize QuadR
instance Vectorize QuadO
instance Vectorize QuadBc
instance Vectorize QuadQ

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
alpha = 7

tf :: Fractional a => a
tf = 4.4

quadOcp :: StateOrOutput -> QuadOrLagrange -> OcpPhase' QuadOcp
quadOcp stateOrOutput quadOrLag =
  OcpPhase
  { ocpMayer = mayer quadOrLag
  , ocpLagrange = lagrange stateOrOutput quadOrLag
  , ocpQuadratures = quadratures stateOrOutput
  , ocpDae = dae
  , ocpBc = bc
  , ocpPathC = pathc
  , ocpPathCBnds = None
  , ocpBcBnds = bcBnds
  , ocpXbnd = xbnd
  , ocpUbnd = ubnd
  , ocpZbnd = QuadZ
  , ocpPbnd = QuadP
  , ocpTbnd = (Just tf, Just tf)
  , ocpObjScale      = Nothing
  , ocpTScale        = Nothing
  , ocpXScale        = Nothing
  , ocpZScale        = Nothing
  , ocpUScale        = Nothing
  , ocpPScale        = Nothing
  , ocpResidualScale = Nothing
  , ocpBcScale       = Nothing
  , ocpPathCScale    = Just None
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

type NCollStages = 120
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
                                 , ("print_level", Opt (0 :: Int))
                                 , ("print_time", Opt False)
                                 ]}

goodSolution :: NlpOut
                (CollTraj QuadX QuadZ QuadU QuadP NCollStages CollDeg)
                (CollOcpConstraints QuadX QuadR QuadBc None NCollStages CollDeg)
                (Vector Double)
                -> HUnit.Assertion
goodSolution out = HUnit.assertBool msg (abs (f - fExpected) < 1e-8 && abs (pF - fExpected) < 1e-8)
  where
    msg = printf "    objective: %.4f, final pos: %.4f, expected: %.4f" f pF fExpected
    fExpected = 0.5 * alpha * tf**2 :: Double
    QuadX pF _ = splitJV xf'
    CollTraj _ _ _ xf' = split (xOpt out)
    Id f = splitJV (fOpt out)

compareIntegration :: (QuadratureRoots, StateOrOutput, QuadOrLagrange) -> HUnit.Assertion
compareIntegration (roots, stateOrOutput, quadOrLag) = HUnit.assert $ do
  cp  <- makeCollProblem roots (quadOcp stateOrOutput quadOrLag) (guess roots)
  let nlp = cpNlp cp
  (ret, out) <- solveNlp solver nlp Nothing
  case ret of
   Left msg -> return (HUnit.assertString msg)
   Right _ -> return (goodSolution out) :: IO HUnit.Assertion


quadratureTests :: Test
quadratureTests =
  testGroup "quadrature tests"
  [ testCase (show input) (compareIntegration input)
  | root <- [Radau, Legendre]
  , stateOrOutput <- [TestState, TestOutput]
  , quadOrLagr <- [TestQuadratures, TestLagrangeTerm]
  , let input = (root, stateOrOutput, quadOrLagr)
  ]
