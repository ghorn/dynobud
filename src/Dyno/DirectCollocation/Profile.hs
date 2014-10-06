{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}

module Dyno.DirectCollocation.Profile
       ( ProfileReport(..)
       , profile
       ) where

import Data.Vector ( Vector )
import Linear.V ( Dim(..) )

import Dyno.View.View ( J )
import Dyno.Vectorize ( Vectorize, Proxy(..) )
import Dyno.Ocp ( OcpPhase )
import Dyno.Solvers ( NlpSolverStuff )
import Dyno.DirectCollocation.Types ( CollTraj, CollOcpConstraints )
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem )
import qualified Dyno.TypeVecs as TV
import Dyno.NlpSolver ( solveNlp' )
import Dyno.Nlp ( Nlp'(..), NlpOut'(..) )

data ProfileReport =
  ProfileReport
  {
  }

toProfileReport ::
  Either String String
  -> NlpOut' (CollTraj x z u p n deg) (CollOcpConstraints n deg x r c h) (Vector Double)
  -> IO ProfileReport
toProfileReport _ _ = return ProfileReport

profile :: forall x z u p r o c h .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize r, Vectorize o, Vectorize c, Vectorize h)
   => OcpPhase x z u p r o c h
  -> (forall deg n . (Dim deg, Dim n) => J (CollTraj x z u p n deg) (Vector Double))
  -> NlpSolverStuff
  -> [(Int,Int)]
  -> IO [ProfileReport]
profile ocp guess solver range = do
  let go :: (Int,Int) -> IO ProfileReport
      go (n,deg) =
        TV.reifyDim n   $ \(Proxy :: Proxy n  ) ->
        TV.reifyDim deg $ \(Proxy :: Proxy deg) ->
        profileOne ocp (guess :: J (CollTraj x z u p n deg) (Vector Double)) solver
  mapM go range

profileOne ::
  forall x z u p r o c h n deg .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p,
   Vectorize r, Vectorize o, Vectorize c, Vectorize h,
   Dim n, Dim deg)
  => OcpPhase x z u p r o c h
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> NlpSolverStuff
  -> IO ProfileReport
profileOne ocp guess solver = do
  cp <- makeCollProblem ocp
  let nlp = cpNlp cp
  x <- solveNlp' solver (nlp { nlpX0' = guess }) Nothing
  uncurry toProfileReport x
