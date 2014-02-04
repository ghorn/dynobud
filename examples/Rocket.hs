{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Hascm.OcpMonad
import Hascm.Ipopt.Ipopt
import Dvda.Expr
import ServerSender
import GliderShared

myDae :: DaeMonad ()
myDae = do
  (p,p') <- diffState "p"
  (v,v') <- diffState "v"
  (m,m') <- diffState "m"
  u <- control "u"

  let g = 9.8
      force = u - m*g

  output "force" force

  p' === v
  v' === force/m
  --m' === -1e-2*u**2
  m' === -1e-1*u

boundaryConditions :: Floating a => (String -> BCMonad a a) -> (String -> BCMonad a a) -> BCMonad a ()
boundaryConditions get0 getF = do
  p0 <- get0 "p"
  v0 <- get0 "v"
  m0 <- get0 "m"

  p0 === 1
  v0 === 0
  m0 === 10

  pF <- getF "p"
  vF <- getF "v"
  pF === 0
  vF === 0

mayer :: (Floating a, Monad m) => (String -> m a) -> a -> m a
mayer get endTime = do
  m <- get "m"

  return (-m) -- endTime -- (p**2 + v**2)

myOcp :: (String -> OcpMonad (Expr Double)) -> OcpMonad ()
myOcp get = do
  p <- get "p"
  v <- get "v"
  m <- get "m"
  u <- get "u"

  0 <== u
  u <== 200

  0.01 <== m

  -0.1 <== p

  -10 <== v
  v <== 1

  lagrangeTerm 0 -- (1e-8*u*u + 1e-9*p*p + 1e-9*v*v + 1e-9*m*m)

main :: IO ()
main = withCallback gliderUrl gliderChannelName go
  where
    n = 60
    deg = 3
    tbnds = (Just 0.1, Just 20)
    (ocpPhase, meta) = buildOcpPhase myDae mayer boundaryConditions myOcp tbnds
    go cb = solveStaticOcpIpopt n deg (Just cb') ocpPhase
      where
        cb' x = cb (x, meta n deg)
