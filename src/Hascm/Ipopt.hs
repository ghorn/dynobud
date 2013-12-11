{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Hascm.Ipopt ( solveNlpIpopt
                   , solveStaticNlpIpopt
                   , solve
                   , setX0
                   , setP
                   , setLbx
                   , setUbx
                   , setLbg
                   , setUbg
                   , getX0
                   , getP
                   , getLbx
                   , getUbx
                   , getLbg
                   , getUbg
                   ) where

import Control.Monad ( when )
import Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V

import Casadi.Wrappers.Enums ( InputOutputScheme(..) )
import Casadi.Callback
import Casadi.Wrappers.Classes.FX
import Casadi.Wrappers.Classes.OptionsFunctionality
import Casadi.Wrappers.Classes.PrintableObject
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.SXMatrix
import Casadi.Wrappers.Classes.GenericType
import Casadi.Wrappers.Classes.SXFunction ( sxFunction''' )
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IpoptSolver
import Casadi.Wrappers.Classes.IOInterfaceFX

import Hascm.Vectorize
import Hascm.Casadi.SX
import Hascm.Casadi.IOSchemes

import Hascm.Nlp
import Hascm.StaticNlp

inf :: Double
inf = read "Infinity"

toBnds :: V.Vector (Maybe Double, Maybe Double) -> (V.Vector Double, V.Vector Double)
toBnds vs = (V.map (fromMaybe (-inf)) lb, V.map (fromMaybe inf) ub)
  where
    (lb,ub) = V.unzip vs

setInput :: (IpoptState -> Int) -> String -> V.Vector Double -> Ipopt ()
setInput getLen name x = do
  ipoptState <- ask
  let nx' = V.length x
      nx = getLen ipoptState
  when (nx /= nx') $ error $ name ++ " dimension mismatch, " ++ show nx ++ " (true) /= " ++ show nx' ++ " (given)"
  liftIO $ ioInterfaceFX_setInput''''' (isSolver ipoptState) x name
  return ()

getInput :: String -> Ipopt (V.Vector Double)
getInput name = do
  ipoptState <- ask
  liftIO $ ioInterfaceFX_input'' (isSolver ipoptState) name >>= dmatrix_data

setX0 :: V.Vector Double -> Ipopt ()
setX0 = setInput isNx "x0"

setLbx :: V.Vector Double -> Ipopt ()
setLbx = setInput isNx "lbx"

setUbx :: V.Vector Double -> Ipopt ()
setUbx = setInput isNx "ubx"

setLbg :: V.Vector Double -> Ipopt ()
setLbg = setInput isNg "lbg"

setUbg :: V.Vector Double -> Ipopt ()
setUbg = setInput isNg "ubg"

setP :: V.Vector Double -> Ipopt ()
setP = setInput isNp "p"

solve :: Ipopt (Either String (NlpOut V.Vector V.Vector Double))
solve = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  liftIO $ do
    fxSolveSafe ipopt

    solveStatus <- fx_getStat ipopt "return_status"  >>= printableObject_getDescription

    fopt <- fmap V.head $ ioInterfaceFX_output'' ipopt "f" >>= dmatrix_data
    xopt <- ioInterfaceFX_output'' ipopt "x" >>= dmatrix_data
    gopt <- ioInterfaceFX_output'' ipopt "g" >>= dmatrix_data
    lamXOpt <- ioInterfaceFX_output'' ipopt "lam_x" >>= dmatrix_data
    lamGOpt <- ioInterfaceFX_output'' ipopt "lam_g" >>= dmatrix_data
    let lambdaOut = Multipliers { lambdaX = lamXOpt
                                , lambdaG = lamGOpt
                                }
        nlpOut = NlpOut { fOpt = fopt
                        , xOpt = xopt
                        , gOpt = gopt
                        , lambdaOpt = lambdaOut
                        }
    if solveStatus `elem` ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
      then return (Right nlpOut)
      else return (Left solveStatus)


getX0 :: Ipopt (V.Vector Double)
getX0 = getInput "x0"

getLbx :: Ipopt (V.Vector Double)
getLbx = getInput "lbx"

getUbx :: Ipopt (V.Vector Double)
getUbx = getInput "ubx"

getLbg :: Ipopt (V.Vector Double)
getLbg = getInput "lbg"

getUbg :: Ipopt (V.Vector Double)
getUbg = getInput "ubg"

getP :: Ipopt (V.Vector Double)
getP = getInput "p"

data IpoptState = IpoptState { isNx :: Int
                             , isNg :: Int
                             , isNp :: Int
                             , isSolver :: IpoptSolver
                             }
newtype Ipopt a = Ipopt (ReaderT IpoptState IO a)
                deriving ( Monad
                         , MonadReader IpoptState
                         , MonadIO
                         )

runIpopt ::
  NlpInputs V.Vector V.Vector SX -> NlpFun V.Vector SX
  -> Maybe (V.Vector Double -> IO Bool)
  -> Ipopt a -> IO a
runIpopt nlpInputs@(NlpInputs x p) nlpOutputs@(NlpFun _ g) callback (Ipopt ipoptMonad) = do
  ipopt <- makeIpoptSolver nlpInputs nlpOutputs callback
  let conf = IpoptState { isNx = V.length x
                        , isNg = V.length g
                        , isNp = V.length p
                        , isSolver = ipopt
                        }
  liftIO $ runReaderT ipoptMonad conf

makeIpoptSolver ::
  NlpInputs V.Vector V.Vector SX -> NlpFun V.Vector SX
  -> Maybe (V.Vector Double -> IO Bool)
  -> IO IpoptSolver
makeIpoptSolver (NlpInputs inputsX inputsP) (NlpFun obj g) callback' = do
  inputsXMat <- sxMatrix''''''''''' inputsX
  inputsPMat <- sxMatrix''''''''''' inputsP
  objMat    <- sxMatrix''''''''''' (V.singleton obj)
  gMat      <- sxMatrix''''''''''' g

  inputScheme <- mkSchemeSXMatrix SCHEME_NLPInput [("x", inputsXMat), ("p", inputsPMat)]
  outputScheme <- mkSchemeSXMatrix SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  f <- sxFunction''' inputScheme outputScheme
  ipopt <- ipoptSolver'' (castFX f)

  -- add callback if user provides it
  case callback' of
    Nothing -> return ()
    Just callback -> do
      let cb fx' = do
            xval <- ioInterfaceFX_output fx' 0 >>= dmatrix_data
            callbackRet <- callback xval
            return $ if callbackRet then 0 else 1
      casadiCallback <- makeCallback cb >>= genericTypeCallback
      optionsFunctionality_setOption ipopt "iteration_callback" casadiCallback

  sharedObject_init ipopt

  return ipopt


solveNlpIpopt ::
  (Vectorize x, Vectorize p, Vectorize g) =>
  Nlp x p g -> Maybe (x Double -> IO Bool)
  -> IO (Either String (NlpOut x g Double))
solveNlpIpopt nlp callback' = do
  (NlpInputs inputsX' inputsP', NlpFun obj g') <- funToSX (nlpFG nlp)
  let inputsX = vectorize inputsX' :: V.Vector SX
      inputsP = vectorize inputsP' :: V.Vector SX
      g = vectorize g' :: V.Vector SX
      callback = fmap (. devectorize) callback'
      nlpInputs = NlpInputs inputsX inputsP
      nlpOutputs = NlpFun obj g

  let (lbx,ubx) = toBnds (vectorize $ nlpBX nlp)
      (lbg,ubg) = toBnds (vectorize $ nlpBG nlp)
      runMe = do
        setX0 (vectorize (nlpX0 nlp))
        setP (vectorize (nlpP nlp))
        setLbx lbx
        setUbx ubx
        setLbg lbg
        setUbg ubg
        solve

  woo <- runIpopt nlpInputs nlpOutputs callback runMe
  return $ case woo of
    Left x -> Left x
    Right (NlpOut { fOpt = fopt
                  , xOpt = xopt
                  , gOpt = gopt
                  , lambdaOpt = Multipliers { lambdaX = lambdax
                                            , lambdaG = lambdag
                                            }}) ->
      Right $ NlpOut { fOpt = fopt
                     , xOpt = devectorize xopt
                     , gOpt = devectorize gopt
                     , lambdaOpt = Multipliers { lambdaX = devectorize lambdax
                                               , lambdaG = devectorize lambdag
                                               }}


solveStaticNlpIpopt ::
  Nlp V.Vector V.Vector V.Vector -> IO (Either String (NlpOut V.Vector V.Vector Double))
solveStaticNlpIpopt nlp = reifyNlp nlp foo
  where
    foo nlp' = do
      ret <- solveNlpIpopt nlp' Nothing
      return $ case ret of
        Left x -> Left x
        Right (NlpOut { fOpt = fopt
                      , xOpt = xopt
                      , gOpt = gopt
                      , lambdaOpt = Multipliers { lambdaX = lambdax
                                                , lambdaG = lambdag
                                                }}) ->
          Right $ NlpOut { fOpt = fopt
                         , xOpt = vectorize xopt
                         , gOpt = vectorize gopt
                         , lambdaOpt = Multipliers { lambdaX = vectorize lambdax
                                                   , lambdaG = vectorize lambdag
                                                   }}
