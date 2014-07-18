{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}

module Ocps.SolveLinearOcp
       ( feasibleOcpIsFeasible, infeasibleOcpIsInfeasible
       ) where

import Test.QuickCheck.Monadic
import Test.QuickCheck.Property
import Data.Vector ( Vector )
import qualified Data.Foldable as F
import Linear

import Dyno.Vectorize
import Dyno.View
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.Cov
import Dyno.Nats
import Dyno.TypeVecs

import Ocps.LinearOcp ( IsLinearOcp(..), LinearOcp(..)
                      , FeasibleLinearOcp(..)
                      , InfeasibleLinearOcp(..)
                      --, runGenWithSeed
                      )

data Bcs n a = Bcs (Vec n a) (Vec n a) deriving (Functor, Generic1, Show)
data X n a = X (Vec n a) deriving (Functor, Generic1, Show)
data U m a = U (Vec m a) deriving (Functor, Generic1, Show)

instance Dim n => Vectorize (X n)
instance Dim m => Vectorize (U m)
instance Dim n => Vectorize (Bcs n)

mayer :: Floating a => a -> x -> x -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer _ _ _ _ _ = 0

lagrange :: Floating a => X n a -> None a -> U m a -> None a -> None a -> a -> a
lagrange _ _ (U vec) _ _ _ = F.sum $ fmap (\x -> x*x) vec

myDae :: forall n m a . (Dim n, Dim m, Floating a)
         => LinearOcp n m -> Dae (X n) None (U m) None (X n) None a
myDae myOcp (X x') (X x) _ (U u) _ _ = (X (x' ^-^ force), None)
  where
    a' :: Vec n (Vec n a)
    a' = fmap (fmap realToFrac) (loA myOcp)

    b' :: Vec n (Vec m a)
    b' = fmap (fmap realToFrac) (loB myOcp)

    force :: Vec n a
    force = (a' !* x)  ^+^  (b' !* u)

bc :: forall n m a . (Dim n, Floating a) => LinearOcp n m -> X n a -> X n a -> Bcs n a
bc myOcp (X x0) (X xF) = Bcs
                         (x0 ^-^ x0')
                         (xF ^-^ xF')
  where
    x0',xF' :: Vec n a
    x0' = fmap realToFrac $ loX0 myOcp
    xF' = fmap realToFrac $ loXF myOcp

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

toOcpPhase :: (Dim n, Dim m, IsLinearOcp a n m)
              => a -> OcpPhase (X n) None (U m) None (X n) None (Bcs n) None JNone JNone JNone
toOcpPhase myOcp' =
  OcpPhase { ocpMayer = mayer
           , ocpLagrange = lagrange
           , ocpDae = myDae myOcp
           , ocpBc = bc myOcp
           , ocpPathC = pathc
           , ocpPathCBnds = None
           , ocpBcBnds = fill (Just 0, Just 0)
           , ocpXbnd = fill (Nothing, Nothing)
           , ocpUbnd = fill (Nothing, Nothing)
           , ocpZbnd = None
           , ocpPbnd = None
           , ocpTbnd = (Just 4, Just 4)

           , ocpSq = 0
           , ocpSbnd = jfill (Nothing,Nothing)
           , ocpSbc = \_ _ -> cat JNone
           , ocpSbcBnds = cat JNone
           , ocpSh = \_ _ -> cat JNone
           , ocpShBnds = cat JNone
           }
  where
    myOcp = getLinearOcp myOcp'


solveLinearOcp :: forall n m a . (IsLinearOcp a n m, Dim n, Dim m)
                  => NlpSolverStuff -> a -> IO (Either String String)
solveLinearOcp solver ocp = do
  let guess = jfill 0 :: J (CollTraj (X n) None (U m) None JNone D10 D2) (Vector Double)
  nlp <- makeCollNlp (toOcpPhase (getLinearOcp ocp))
  fmap fst $ solveNlp' solver (nlp { nlpX0' = guess }) Nothing

feasibleOcpIsFeasible :: (Dim n, Dim m)
                         => NlpSolverStuff -> FeasibleLinearOcp n m -> Property
feasibleOcpIsFeasible solver (FeasibleLinearOcp ocp) = monadicIO $ do
  ret <- run $ solveLinearOcp solver ocp
  case ret of
    Right msg -> stop (succeeded {reason = msg})
    Left msg -> stop (failed {reason = msg})

infeasibleOcpIsInfeasible :: (Dim n, Dim m)
                             => NlpSolverStuff -> InfeasibleLinearOcp n m -> Property
infeasibleOcpIsInfeasible solver (InfeasibleLinearOcp ocp) = monadicIO $ do
  ret <- run $ solveLinearOcp solver ocp
  case ret of
    Right msg -> stop (failed {reason = msg})
    Left msg -> stop (succeeded {reason = msg})
