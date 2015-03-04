{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Control.Monad ( void )

import Dyno.Solvers

import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )
import Dynoplot.Callback
import ExampleDsl.OcpMonad

myDae :: SXElement -> DaeMonad ()
myDae time = do
  (p,p') <- diffState "p"
  (v,v') <- diffState "v"
  u <- control "u"

  let k = 4
      b = 0.3

      force = u - k * p - b * v
      obj = p**2 + v**2 + u**2
  output "force" force
  output "obj" obj

  p' === v
  v' === force + 0.1 * sin time

boundaryConditions :: (String -> BCMonad SXElement) -> (String -> BCMonad SXElement) -> BCMonad ()
boundaryConditions get0 getF = do
  p0 <- get0 "p"
  v0 <- get0 "v"

  pF <- getF "p"
  vF <- getF "v"

  p0 === 0
  v0 === 0

--  p0 + 4 <== pF -- inequalities missing for now
--  v0 === vF
  pF === 1
  vF === 0

mayer :: (Floating a, Monad m) => a -> (String -> m a) -> (String -> m a) -> m a
mayer endTime _get0 getF = do
  p <- getF "p"
  v <- getF "v"

  return (p**2 + v**2 + endTime/1000)

myOcp :: SXElement -> (String -> OcpMonad SXElement) -> OcpMonad ()
myOcp time get = do
  v <- get "v"
  u <- get "u"
  force <- get "force"
  obj <- get "obj"

  v**2 + u**2 <== 4 + time/100

  lagrangeTerm (obj + force*force*1e-4)

main :: IO ()
main = void $ withCallback go
  where
    n = 100
    deg = 3
    tbnds = (Just 4, Just 4)
    go cb = solveStaticOcp Legendre ipoptSolver myDae mayer boundaryConditions myOcp tbnds n deg (Just cb')
      where
        cb' meta x = cb (x, meta)
