{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Hascm.OcpMonad
import Hascm.Ipopt.Ipopt
import Dvda.Expr

myDae :: DaeMonad ()
myDae = do
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
  v' === force

boundaryConditions :: Floating a => (String -> BCMonad a a) -> (String -> BCMonad a a) -> BCMonad a ()
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

mayer :: (Floating a, Monad m) => (String -> m a) -> a -> m a
mayer get endTime = do
  p <- get "p"
  v <- get "v"

  return (p**2 + v**2)

myOcp :: (String -> OcpMonad (Expr Double)) -> OcpMonad ()
myOcp get = do
  p <- get "p"
  v <- get "v"
  u <- get "u"
  force <- get "force"
  obj <- get "obj"

  v**2 + u**2 <== 4

  lagrangeTerm obj

main :: IO ()
main = solveStaticOcpIpopt n deg (toOcpPhase myDae mayer boundaryConditions myOcp tbnds)
  where
    n = 60
    deg = 3
    tbnds = (Just 4, Just 4)
