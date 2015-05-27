{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Dyno.OcpHomotopy
       ( runOcpHomotopy
       ) where

import Data.Vector ( Vector )
import qualified Data.Traversable as T

import Casadi.MX ( MX )

import Dyno.Ocp
import Dyno.Vectorize ( Vectorize )
import Dyno.View.View ( J )
import Dyno.View.JV ( JV, catJV )
import Dyno.TypeVecs ( Dim )
import Dyno.Solvers ( Solver )
import Dyno.Nlp ( Nlp(..), NlpOut(..) )
import Dyno.NlpUtils ( HomotopyParams(..), solveNlp, solveNlpHomotopy )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollOcpConstraints )
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots )


runOcpHomotopy ::
  forall x z u p r o c h q po fp n deg t
  . ( Dim n, Dim deg
    , Vectorize x, Vectorize z, Vectorize u, Vectorize p
    , Vectorize r, Vectorize o, Vectorize c, Vectorize h
    , Vectorize q, Vectorize po
    , Vectorize fp
    , T.Traversable t )
  => Double -> HomotopyParams
  -> OcpPhase x z u p r o c h q po fp
  -> OcpPhaseInputs x z u p c h fp
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> QuadratureRoots -> Bool -> Bool -> Solver -> Solver
  -> t (fp Double)
  -> (CollProblem x z u p r o c h q po fp n deg
      -> IO ([String] -> J (CollTraj x z u p n deg) (Vector Double) -> J (JV fp) (Vector Double) -> IO Bool)
     )
  -> IO (t (NlpOut (CollTraj x z u p n deg)
                   (CollOcpConstraints x r c h n deg)
                   (Vector Double)))
runOcpHomotopy step0 homotopyParams ocpHomotopy ocpHomotopyInputs guess roots
  useStartupCallback useHomotopyCallback
  startupSolver homotopySolver nominalParams makeCallback = do
  cp0 <- makeCollProblem roots ocpHomotopy ocpHomotopyInputs guess
  callback <- makeCallback cp0
  let nlpHomotopy :: Nlp
                     (CollTraj x z u p n deg)
                     (JV fp)
                     (CollOcpConstraints x r c h n deg)
                     MX
      nlpHomotopy = cpNlp cp0

  let scb = if useStartupCallback
            then Just (callback ["homotopy startup solve"])
            else Nothing

  putStrLn "running startup solver..."
  (msg0,opt0') <- solveNlp startupSolver nlpHomotopy scb

  opt0 <- case msg0 of
    Left msg' -> error msg'
    Right _ -> return opt0'

  let homoGuessX :: J (CollTraj x z u p n deg) (Vector Double)
      homoGuessX  = xOpt opt0
      homoGuessLX = lambdaXOpt opt0
      homoGuessLG :: J (CollOcpConstraints x r c h n deg) (Vector Double)
      homoGuessLG = lambdaGOpt opt0

      pFinals :: t (J (JV fp) (Vector Double))
      pFinals = fmap catJV nominalParams

      homoCallback :: J (CollTraj x z u p n deg) (Vector Double) -> J (JV fp) (Vector Double)
                      -> IO Bool
      homoCallback traj0 fp =
        callback [ "homotopy stepping"
                 ] traj0 fp

  putStrLn "\ninitial solve done, starting homotopy steps"
  let hcb = if useHomotopyCallback then Just homoCallback else Nothing
  solveNlpHomotopy step0 homotopyParams
    homotopySolver
    (nlpHomotopy { nlpX0    = homoGuessX
                 , nlpLamX0 = Just homoGuessLX
                 , nlpLamG0 = Just homoGuessLG
                 })
    pFinals
    hcb Nothing
