{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

--import Control.Concurrent ( threadDelay )
import Hascm.OcpMonad
import Hascm.Ipopt
--import Hascm.Snopt
import Hascm.NlpSolver ( solveStaticOcp )

import Dvda.Expr
import ServerSender
import GliderShared

myDae :: DaeMonad ()
myDae = do
  (p,p') <- diffState "p"
  (v,v') <- diffState "v"
  (m,m') <- diffState "m"
  (u,u') <- diffState "u"
  u'' <- control "u'"

  let g = 9.8
      force = u - m*g

  output "force" force

  p' === v
  v' === force/m
  m' === -1e-2*u**2
  u'' === u'

boundaryConditions :: Floating a => (String -> BCMonad a a) -> (String -> BCMonad a a) -> BCMonad a ()
boundaryConditions get0 getF = do
  -- initial
  p0 <- get0 "p"
  v0 <- get0 "v"
  m0 <- get0 "m"

  p0 === 1
  v0 === 0
  m0 === 10

  -- final
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
  u' <- get "u'"

  -200 <== u
  u <== 200

  -100 <== u'
  u' <== 100

  0.01 <== m

  0 <== p

  -10 <== v
  v <== 0.0

  lagrangeTerm (1e-4*u'*u')
  --lagrangeTerm (1e-8*u*u + 1e-9*p*p + 1e-9*v*v + 1e-9*m*m)
  --lagrangeTerm (1e-6*u*u + 1e-6*p*p + 1e-6*v*v + 1e-6*m*m)

main :: IO ()
main = withCallback gliderUrl gliderChannelName go
  where
    n = 100
    deg = 3
    tbnds = (Just 0.2, Just 6)
    --tbnds = (Just 1.5, Just 1.5)
    (ocpPhase, meta) = buildOcpPhase myDae mayer boundaryConditions myOcp tbnds
    go cb = solveStaticOcp ipoptSolver n deg (Just cb') ocpPhase
    --go cb = solveStaticOcp snoptSolver n deg (Just cb') ocpPhase
      where
        cb' x = cb (x, meta n deg)
        --cb' x = threadDelay 200000 >> cb (x, meta n deg)
