{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}

module Hascm.Casadi.QP ( QPIn(..), QPOut(..), newQP, solveQP ) where

import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.QPStructure
import Casadi.Wrappers.Classes.IpoptSolver
import Casadi.Wrappers.Classes.GenericType
import Casadi.Wrappers.Classes.OptionsFunctionality
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IOInterfaceFX
import Casadi.Wrappers.Classes.FX
import Casadi.Wrappers.Classes.NLPQPSolver

data QPIn a = QPIn { qpH :: a
                   , qpG :: a
                   , qpA :: a
                   , qpLbA :: a
                   , qpUbA :: a
                   , qpLbX :: a
                   , qpUbX :: a
                   , qpX0 :: a
                   , qpLamX0 :: a
                   }

data QPOut a = QPOut { qpX :: a
                     , qpCost :: a
                     , qpLamA :: a
                     , qpLamX :: a
                     } deriving Show

newQP :: QPStructure -> IO NLPQPSolver
newQP qpStruct = do
  putStrLn "newQP"
  qp <- nlpqpSolver' qpStruct
  putStrLn "ipoptSolver"
  sol <- ipoptSolver
  putStrLn "genericTypeFX"
  gsol <- genericTypeFX (castFX sol)

  putStrLn "set option"
  optionsFunctionality_setOption qp "nlp_solver" gsol

  putStrLn "init"
  sharedObject_init' qp

  putStrLn "return"
  return qp


solveQP :: NLPQPSolver -> QPIn DMatrix -> IO (QPOut DMatrix)
solveQP qp (QPIn h g a lba uba lbx ubx x0 lam_x0) = do
  mapM_ (uncurry (ioInterfaceFX_setInput'''''''' qp))
    [ (h,      "h")
    , (g,      "g")
    , (a,      "a")
    , (lba,    "lba")
    , (uba,    "uba")
    , (lbx,    "lbx")
    , (ubx,    "ubx")
    , (x0,     "x0")
    , (lam_x0, "lam_x0")
    , (h,      "h")
    ]

  fx_evaluate'' qp
  x <- ioInterfaceFX_output'' qp "x"
  cost <- ioInterfaceFX_output'' qp "cost"
  lamA <- ioInterfaceFX_output'' qp "lam_a"
  lamX <- ioInterfaceFX_output'' qp "lam_x"

  return $ QPOut x cost lamA lamX

--toSXFunction :: (Vectorize f, Vectorize g) => f SXMatrix -> g SXMatrix -> IO (SXFunction f g)
--toSXFunction inputs outputs = do
--  sxf <- C.sxFunction''' (vectorize inputs) (vectorize outputs)
--  sharedObject_init' sxf
--  return (SXFunction sxf)
--
--evalSXFun :: (Vectorize f, Vectorize g) => SXFunction f g -> f DMatrix -> IO (g DMatrix)
--evalSXFun (SXFunction sxf) inputs = do
--  -- set inputs
--  zipWithM_ (ioInterfaceFX_setInput'''''' sxf) (V.toList (vectorize inputs)) [0..]
--  -- eval
--  fx_evaluate'' sxf
--  -- get outputs
--  numOut <- ioInterfaceFX_getNumOutputs sxf
--  outputs <- mapM (ioInterfaceFX_output sxf) (take numOut [0..])
--  -- return vectorized outputs
--  return (devectorize (V.fromList outputs))
