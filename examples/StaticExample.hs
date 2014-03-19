{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Vector as V

import Dyno.Nlp
import Dyno.NlpMonad
import Dyno.NlpSolver
import Dyno.Ipopt
--import Dyno.Snopt
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch

buildRosen :: IO (Nlp V.Vector V.Vector V.Vector)
buildRosen = fmap fst $ buildNlp' $ do
  x1 <- designVar "x1"
  x2 <- designVar "x2"
  x3 <- designVar "x3"
  x4 <- designVar "x4"

  0 +     x1**2 +   x2**2 + x3      === 2
  0 +               x2**4      + x4 === 4
  0 +   2*x1    + 4*x2              >== 0.0
  x3 >== 0
  x4 >== 0

  minimize $ (x1 + x2 + x3)**2 + 3*x3 + 5*x4


main :: IO ()
main = do
  rosen0 <- buildRosen

  let rosen = rosen0 {nlpX0 = V.fromList [0.1,0.125,0.666666,0.142857]}

  ret <- solveStaticNlp ipoptSolver rosen Nothing
  print ret
  --ret2 <- solveStaticNlp snoptSolver rosen Nothing
--  print ret'
--  (xopt , kktInf) <- solveSqp rosen armilloSearch
--  print xopt
--  print kktInf
