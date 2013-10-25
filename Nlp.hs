{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleContexts #-}

module Nlp ( Nlp(..), NlpFun(..), solveNlp ) where

import qualified Data.Vector as V
import Data.Maybe ( fromMaybe )

import Casadi.Wrappers.Enums ( InputOutputScheme(SCHEME_NLPInput, SCHEME_NLPOutput) )
import Casadi.Wrappers.Classes.FX
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.SXMatrix
import Casadi.Wrappers.Classes.SXFunction
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IpoptSolver
import Casadi.Wrappers.Classes.IOInterfaceFX

import Vectorize
import DvdaCasadi
import IOSchemes
import TypeNats
import TypeVecs

data NlpFun g a = NlpFun a (g a) deriving (Show, Functor, Generic1)
instance (Vectorize g ng, NaturalT n, PositiveT n, Succ ng ~ n) =>
         Vectorize (NlpFun g) n where
  empty = NlpFun () empty
  vectorize (NlpFun a g) = a <| vectorize g
  devectorize v = NlpFun (tvhead v) (devectorize (tvtail v))

data Nlp x g =
  Nlp { nlpFG :: Floating a => x a -> NlpFun g a
      , nlpBX :: x (Maybe Double, Maybe Double)
      , nlpBG :: g (Maybe Double, Maybe Double)
      }

inf :: Double
inf = read "Infinity"

toBnds :: V.Vector (Maybe Double, Maybe Double) -> (V.Vector Double, V.Vector Double)
toBnds vs = (V.map (fromMaybe (-inf)) lb, V.map (fromMaybe inf) ub)
  where
    (lb,ub) = V.unzip vs


solveNlp :: (Vectorize x nx, Vectorize g ng, NaturalT ng, NaturalT (Succ ng), PositiveT (Succ ng)) => Nlp x g -> IO (x Double)
solveNlp nlp = do
  (inputs, NlpFun obj g') <- funToSX (nlpFG nlp)
  let g = unVec (vectorize g') :: V.Vector SX
  inputsMat <- sxMatrix''''''''''' (unVec $ vectorize inputs)
  paramsMat <- sxMatrix''''''''''' V.empty
  objMat    <- sxMatrix''''''''''' (V.singleton obj)
  gMat      <- sxMatrix''''''''''' g

  inputScheme <- mkSchemeSXMatrix SCHEME_NLPInput [("x", inputsMat), ("p", paramsMat)]
  outputScheme <- mkSchemeSXMatrix SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  f <- sxFunction''' inputScheme outputScheme
  ipopt <- ipoptSolver'' (castFX f)
  sharedObject_init' ipopt

  let (lbx,ubx) = toBnds (unVec $ vectorize $ nlpBX nlp)
      (lbg,ubg) = toBnds (unVec $ vectorize $ nlpBG nlp)
  ioInterfaceFX_setInput''''' ipopt lbx "lbx"
  ioInterfaceFX_setInput''''' ipopt ubx "ubx"
  ioInterfaceFX_setInput''''' ipopt lbg "lbg"
  ioInterfaceFX_setInput''''' ipopt ubg "ubg"

  fx_solve ipopt

  xopt <- ioInterfaceFX_output'' ipopt "x" >>= dmatrix_data
  return (devectorize $ mkVec xopt)
