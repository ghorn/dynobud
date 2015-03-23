{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}

module Dyno.DirectCollocation.Profile
       ( ProfileReport(..)
       , profile
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import Linear.V ( Dim(..) )

import Dyno.View.View ( J )
import Dyno.Vectorize ( Vectorize )
import Dyno.Ocp
import Dyno.Solvers ( Solver )
import Dyno.DirectCollocation.Types ( CollTraj, CollOcpConstraints )
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots )
import qualified Dyno.TypeVecs as TV
import Dyno.NlpUtils ( solveNlp )
import Dyno.Nlp ( Nlp(..), NlpOut(..) )

data ProfileReport =
  ProfileReport
  {
  }

toProfileReport ::
  Either String String
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO ProfileReport
toProfileReport _ _ = return ProfileReport

profile :: forall x z u p r o c h q .
  ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Vectorize r, Vectorize o, Vectorize c, Vectorize h, Vectorize q
  )
  => QuadratureRoots
  -> OcpPhase x z u p r o c h q
  -> (forall deg n . (Dim deg, Dim n) => J (CollTraj x z u p n deg) (Vector Double))
  -> Solver
  -> [(Int,Int)]
  -> IO [ProfileReport]
profile roots ocp guess solver range = do
  let go :: (Int,Int) -> IO ProfileReport
      go (n,deg) =
        TV.reifyDim n   $ \(Proxy :: Proxy n  ) ->
        TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
        profileOne roots ocp (guess :: J (CollTraj x z u p n deg) (Vector Double)) solver
  mapM go range

profileOne ::
  forall x z u p r o c h q n deg .
  ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Vectorize r, Vectorize o, Vectorize c, Vectorize h, Vectorize q
  , Dim n, Dim deg
  )
  => QuadratureRoots
  -> OcpPhase x z u p r o c h q
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> Solver
  -> IO ProfileReport
profileOne roots ocp guess solver = do
  cp <- makeCollProblem roots ocp
  let nlp = cpNlp cp
  x <- solveNlp solver (nlp { nlpX0 = guess }) Nothing
  uncurry toProfileReport x
