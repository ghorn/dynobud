{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Dyno.DirectCollocation
       ( CollTraj(..)
       , CollProblem(..)
       , makeCollProblem
       , solveOcp
       ) where

import Data.Proxy
import Data.Vector ( Vector )

import Dyno.View.View ( J, jfill )
import Dyno.Vectorize ( Vectorize )
import Dyno.Ocp ( OcpPhase )
import Dyno.NlpUtils ( solveNlp )
import Dyno.Solvers ( Solver )
import Dyno.Nlp ( Nlp(..) )
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem )
import Dyno.DirectCollocation.Types ( CollTraj(..) )
import Dyno.DirectCollocation.Dynamic ( DynPlotPoints )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots )
import qualified Dyno.TypeVecs as TV

solveOcp ::
  forall x z u p r o c h .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize r, Vectorize o, Vectorize c, Vectorize h)
  => QuadratureRoots -> Solver -> Int -> Int -> Maybe (DynPlotPoints Double -> IO Bool)
  -> OcpPhase x z u p r o c h
  -> IO (Either String String)
solveOcp roots solverStuff n deg cb0 ocp =
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) -> do
    let guess :: J (CollTraj x z u p n deg) (Vector Double)
        guess = jfill 1
    cp <- makeCollProblem roots ocp
    let nlp = cpNlp cp
        toPlotPoints = cpPlotPoints cp
    --_ <- solveNlp solverStuff (nlp {nlpX0 = guess}) (fmap (. ctToDynamic) cb)
    let cb = case cb0 of
          Nothing -> Nothing
          Just cb' -> Just $ \x -> toPlotPoints x >>= cb'

    (res, _) <- solveNlp solverStuff (nlp {nlpX0 = guess}) cb
    return res
