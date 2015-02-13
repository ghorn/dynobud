{-# OPTIONS_GHC -Wall #-}

module Main where

import Dyno.Solvers

import ExampleDsl.NlpMonad

rosen :: NlpMonad ()
rosen = do
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
  let guess = [ ("x1", 0.1)
              , ("x2", 0.125)
              , ("x3", 0.666666)
              , ("x4", 0.142857)
              ]
              
  (status, fopt, xopt) <- solveStaticNlp ipoptSolver rosen guess Nothing
  print status
  putStrLn $ "value: " ++ show fopt
  mapM_ (\(n,v) -> putStrLn $ n ++ ": " ++ show v) xopt
