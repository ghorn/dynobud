{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.OcpHomotopy
       ( runOcpHomotopy
       ) where

import GHC.TypeLits ( KnownNat )

import Data.Vector ( Vector )
import qualified Data.Traversable as T

import Casadi.MX ( MX )

import qualified Data.Map as M
import Dyno.Ocp
import Dyno.Vectorize ( Vectorize )
import Dyno.View.View ( J, JV, catJV )
import Dyno.Solvers ( Solver )
import Dyno.Nlp ( Nlp(..), NlpIn(..), NlpOut(..) )
import Dyno.NlpSolver ( GType )
import Dyno.NlpUtils ( HomotopyParams(..), solveNlp, solveNlpHomotopy )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollOcpConstraints )
import Dyno.DirectCollocation.Formulate
       ( CollProblem(..), DirCollOptions, makeCollProblem )


runOcpHomotopy ::
  forall x z u p r o c h q qo po fp n deg t
  . ( KnownNat n, KnownNat deg
    , Vectorize x, Vectorize z, Vectorize u, Vectorize p
    , Vectorize r, Vectorize o, Vectorize c, Vectorize h
    , Vectorize q, Vectorize po, Vectorize qo
    , Vectorize fp
    , T.Traversable t )
  => DirCollOptions
  -> Double -> HomotopyParams
  -> OcpPhase x z u p r o c h q qo po fp
  -> OcpPhaseInputs x z u p c h fp
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> Bool -> Bool -> Solver -> Solver
  -> t (fp Double)
  -> (CollProblem x z u p r o c h q qo po fp n deg
      -> IO ([String] -> J (CollTraj x z u p n deg) (Vector Double)
             -> J (JV fp) (Vector Double) -> M.Map String GType -> IO Bool)
     )
  -> IO (t (NlpOut (CollTraj x z u p n deg)
                   (CollOcpConstraints x p r c h n deg)
                   (Vector Double)))
runOcpHomotopy dirCollOpts step0 homotopyParams ocpHomotopy ocpHomotopyInputs guess
  useStartupCallback useHomotopyCallback
  startupSolver homotopySolver nominalParams makeCallback = do
  cp0 <- makeCollProblem dirCollOpts ocpHomotopy ocpHomotopyInputs guess
  callback <- makeCallback cp0
  let nlpHomotopy :: Nlp
                     (CollTraj x z u p n deg)
                     (JV fp)
                     (CollOcpConstraints x p r c h n deg)
                     MX
      nlpHomotopy = cpNlp cp0

  let scb = if useStartupCallback
            then Just (callback ["homotopy startup solve"])
            else Nothing

  putStrLn "running startup solver..."
  (_, eret) <- solveNlp startupSolver nlpHomotopy scb

  opt0 <- case eret of
    Left msg -> error msg
    Right r -> return r

  let homoGuessX :: J (CollTraj x z u p n deg) (Vector Double)
      homoGuessX  = xOpt opt0
      homoGuessLX = lambdaXOpt opt0
      homoGuessLG :: J (CollOcpConstraints x p r c h n deg) (Vector Double)
      homoGuessLG = lambdaGOpt opt0

      pFinals :: t (J (JV fp) (Vector Double))
      pFinals = fmap catJV nominalParams

      homoCallback :: J (CollTraj x z u p n deg) (Vector Double)
                      -> J (JV fp) (Vector Double)
                      -> M.Map String GType
                      -> IO Bool
      homoCallback traj0 fp stats =
        callback [ "homotopy stepping"
                 ] traj0 fp stats

  putStrLn "\ninitial solve done, starting homotopy steps"
  let hcb = if useHomotopyCallback then Just homoCallback else Nothing
  solveNlpHomotopy step0 homotopyParams
    homotopySolver
    (nlpHomotopy {nlpIn =
                    (nlpIn nlpHomotopy)
                    { nlpX0    = homoGuessX
                    , nlpLamX0 = Just homoGuessLX
                    , nlpLamG0 = Just homoGuessLG
                    }
                 })
    pFinals
    hcb Nothing
