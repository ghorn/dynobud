{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dyno.SimpleOcp
       ( SimpleOcp(..)
       , Se
       , solveOcp
       ) where

import GHC.Generics ( Generic, Generic1 )
import GHC.TypeLits ( KnownNat )

import Accessors ( Lookup )
import Casadi.SX ( SX )
import qualified Data.Foldable as F
import Data.Proxy
import Data.Reflection ( reifyNat )
import Data.Vector ( Vector )

import Dyno.Ocp
import Dyno.Solvers
import Dyno.NlpUtils
import Dyno.Nlp
import Dyno.DirectCollocation.Formulate
import Dyno.DirectCollocation.Types
import Dyno.View.Vectorize ( Vectorize(..), Tuple(..), None(..), fill, vzipWith )
import Dyno.View.View -- ( View(..) )
import Dyno.View.JVec

-- | scalar symbolic type
newtype Se = Se {unSe :: S SX} deriving (Num, Fractional, Floating)

data SimpleOcp x u =
  SimpleOcp
  { ode :: x Se -> u Se -> x Se
  , objective :: x Se -> u Se -> Se
  , xBounds :: x (Double, Double)
  , uBounds :: u (Double, Double)
  , xInitial :: x Double
  , xFinal :: x Double
  , endTime :: Double
  , initialGuess :: Double -> x Double
  }

vminus :: (Vectorize f, Num a) => f a -> f a -> f a
vminus = vzipWith (-)

dot :: (Vectorize f, Num a) => f a -> f a -> a
dot x y = F.sum $ vectorize $ vzipWith (*) x y

toOcp :: (Vectorize x, Vectorize u)
         => SimpleOcp x u
         -> OcpPhase (Tuple x u) None u None (Tuple x u) None (SimpleBc x) None None None None None
toOcp simple =
  OcpPhase
  { ocpMayer = \_ _ _ _ _ _ -> 0
  , ocpLagrange = \(Tuple x u) _ u' _ _ _ _ _ -> 1e-9 * (u' `dot` u')  + unSe (objective simple (fmap Se x) (fmap Se u))
  , ocpQuadratures = \_ _ _ _ _ _ _ _ -> None
  , ocpQuadratureOutputs = \_ _ _ _ _ _ _ _ -> None
  , ocpDae = \(Tuple xd ud) (Tuple x u) _ u' _ _ _ ->
     let r = Tuple (xd `vminus` x') (ud `vminus` u')
         x' = fmap unSe $ ode simple (fmap Se x) (fmap Se u)
     in (r, None)
  , ocpBc = \(Tuple x0 _) (Tuple xf _) _ _ _ _ ->SimpleBc x0 xf
  , ocpPathC = \_ _ _ _ _ _ _ -> None
  , ocpPlotOutputs = \_ _ _ _ _ _ _ _ _ _ _ -> None
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

data SimpleBc x a = SimpleBc (x a) (x a) deriving (Functor, Generic, Generic1)
instance Vectorize x => Vectorize (SimpleBc x)
instance Lookup (x a) => Lookup (SimpleBc x a)

toOcpInputs :: (Vectorize x, Vectorize u) => SimpleOcp x u -> OcpPhaseInputs (Tuple x u) None u None (SimpleBc x) None None
toOcpInputs simple =
  OcpPhaseInputs
  { ocpBcBnds = SimpleBc
                (fmap (\x -> (Just x, Just x)) (xInitial simple))
                (fmap (\x -> (Just x, Just x)) (xFinal simple))
  , ocpPathCBnds = None
  , ocpXbnd = fmap toBounds $ Tuple (xBounds simple) (uBounds simple)
  , ocpUbnd = fill (Nothing, Nothing)
  , ocpZbnd = None
  , ocpPbnd = None
  , ocpTbnd = (Just (endTime simple), Just (endTime simple))
  , ocpFixedP = None
  }
  where
    toBounds (lb,ub) = (Just lb, Just ub)

solveOcp :: (Vectorize x, Vectorize u) => SimpleOcp x u -> IO (Either String [(x Double, u Double)])
solveOcp simple = reifyNat deg $ reifyNat n $ solveOcp' simple
  where
    n = 50
    deg = 2

solver :: Solver
solver = ipoptSolver

solveOcp' ::
  forall x u n deg
  . (Vectorize x, Vectorize u, KnownNat deg, KnownNat n)
  => SimpleOcp x u -> Proxy n -> Proxy deg -> IO (Either String [(x Double, u Double)])
solveOcp' simple _ _ = do
  let ocp = toOcp simple
      ocpInputs = toOcpInputs simple
      tf = endTime simple
      dirCollOpts = def
      roots = collocationRoots dirCollOpts

      guess :: CollTraj (Tuple x u) None u None n deg (Vector Double)
      guess = makeGuess roots tf (\t -> Tuple (initialGuess simple t) (fill 0)) (const None) (const (fill 0)) None
  cp <- makeCollProblem dirCollOpts ocp ocpInputs (cat guess)
  let _ = cp :: CollProblem (Tuple x u) None u None (Tuple x u) None (SimpleBc x) None None None None None n deg
  (_, eopt) <- solveNlp solver (cpNlp cp) Nothing
  case eopt of
    Left msg -> return (Left msg)
    Right opt -> do
      let CollTraj _ _ stages' xf' = split (xOpt opt)
          xs = map ((\(CollStage x _ _ _) -> splitJV x) . split) $ F.toList $ unJVec (split stages')
      return $ Right $ map (\(Tuple x u) -> (x, u)) (xs ++ [splitJV xf'])
