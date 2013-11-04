{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}

module Hascm.Nlp ( Nlp(..), NlpFun(..), solveNlp ) where

import qualified Data.Vector as V
import Data.Maybe ( fromMaybe )

import Casadi.Wrappers.Enums ( InputOutputScheme(..) )
import Casadi.Callback
import Casadi.Wrappers.Tools ( sp_dense )
import Casadi.Wrappers.Classes.FX
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.SXMatrix
import Casadi.Wrappers.Classes.SXFunction
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IpoptSolver
import Casadi.Wrappers.Classes.IOInterfaceFX

import Hascm.Vectorize
import Hascm.DvdaCasadi
import Hascm.IOSchemes

data NlpFun g a = NlpFun a (g a) deriving (Show, Functor, Generic1)
instance Vectorize g => Vectorize (NlpFun g)

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


solveNlp :: forall x g . (Vectorize x, Vectorize g) =>
            Nlp x g -> Maybe (x Double -> IO Bool) -> IO (x Double)
solveNlp nlp callback' = do
  (inputs', NlpFun obj g') <- funToSX (nlpFG nlp)
  let inputs = vectorize inputs' :: V.Vector SX
      g = vectorize g' :: V.Vector SX
  inputsMat <- sxMatrix''''''''''' inputs
  paramsMat <- sxMatrix''''''''''' V.empty
  objMat    <- sxMatrix''''''''''' (V.singleton obj)
  gMat      <- sxMatrix''''''''''' g

  inputScheme <- mkSchemeSXMatrix SCHEME_NLPInput [("x", inputsMat), ("p", paramsMat)]
  outputScheme <- mkSchemeSXMatrix SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  f <- sxFunction''' inputScheme outputScheme
  ipopt <- ipoptSolver'' (castFX f)

  -- add callback if user provides it
  case callback' of
    Nothing -> return ()
    Just callback -> do
      spX  <- sp_dense (V.length inputs) 1
      spLX <- sp_dense (V.length inputs) 1
      spF  <- sp_dense 1 1
      spG  <- sp_dense (V.length g) 1
      spLG <- sp_dense (V.length g) 1
      spLP <- sp_dense 0 1
      cfunInput <- mkSchemeCRSSparsity SCHEME_NLPSolverOutput
                   [ ("x",spX)
                   , ("f",spF)
                   , ("lam_x",spLX)
                   , ("lam_g",spLG)
                   , ("lam_p",spLP)
                   , ("g",spG)
                   ]
      spOut <- sp_dense 1 1
      let cfunOutput = V.singleton spOut
          cb fx' _ _ = do
            xval <- ioInterfaceFX_input fx' 0 >>= dmatrix_data
            callbackRet <- callback (devectorize xval)
            -- terminate execution if user requests
            if callbackRet
              then ioInterfaceFX_setOutput' fx' 0
              else ioInterfaceFX_setOutput' fx' 1
      addCallback ipopt cb cfunInput cfunOutput

  sharedObject_init' ipopt

  let (lbx,ubx) = toBnds (vectorize $ nlpBX nlp)
      (lbg,ubg) = toBnds (vectorize $ nlpBG nlp)
  ioInterfaceFX_setInput''''' ipopt lbx "lbx"
  ioInterfaceFX_setInput''''' ipopt ubx "ubx"
  ioInterfaceFX_setInput''''' ipopt lbg "lbg"
  ioInterfaceFX_setInput''''' ipopt ubg "ubg"

  fxSolveSafe ipopt
  --fx_solve ipopt

  xopt <- ioInterfaceFX_output'' ipopt "x" >>= dmatrix_data
  return (devectorize xopt)
