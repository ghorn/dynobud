{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

module Hascm.Ipopt ( solveNlpIpopt
                   , solveStaticNlpIpopt
                   , solveStaticOcpIpopt
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
                   , IpoptOption
                   , setOption
                   ) where

import Control.Monad ( when )
import Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
--import Linear.V ( Dim )
import Data.Proxy

import Casadi.Wrappers.Enums ( InputOutputScheme(..) )
import Casadi.Callback
import Casadi.Wrappers.Classes.FX
import Casadi.Wrappers.Classes.OptionsFunctionality
import Casadi.Wrappers.Classes.PrintableObject
import Casadi.Wrappers.Classes.GenericType
import Casadi.Wrappers.Classes.SXFunction ( sxFunction''' )
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.IpoptSolver
import Casadi.Wrappers.Classes.IOInterfaceFX

import Hascm.Vectorize
import Hascm.Casadi.SX
import Hascm.Casadi.DMatrix
import Hascm.Casadi.SXMatrix
import Hascm.Casadi.IOSchemes

import Hascm.Nlp
import Hascm.NlpMonad ( reifyNlp )
import Hascm.Ocp ( OcpPhase )
import Hascm.OcpMonad ( reifyOcp )
import Hascm.DirectCollocation ( CollTraj, makeCollNlp )
import qualified Hascm.TypeVecs as TV

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
  liftIO $ fmap ddata $ ioInterfaceFX_input'' (isSolver ipoptState) name

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

class IpoptOption a where
  mkGeneric :: a -> IO GenericType

instance IpoptOption (V.Vector Double) where
  mkGeneric = genericTypeDoubleVec
instance IpoptOption (V.Vector Int) where
  mkGeneric = genericTypeIntVec
instance IpoptOption Double where
  mkGeneric = genericTypeDouble
instance IpoptOption String where
  mkGeneric = genericTypeString
instance IpoptOption Int where
  mkGeneric = genericTypeInt

setOption :: IpoptOption a => String -> a -> Ipopt ()
setOption name val = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  liftIO $ do
    gt <- mkGeneric val
    optionsFunctionality_setOption ipopt name gt
  reinit

reinit :: Ipopt ()
reinit = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  liftIO $ sharedObject_init ipopt

solve :: Ipopt (Either String (NlpOut V.Vector V.Vector Double))
solve = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  liftIO $ do
    fxSolveSafe ipopt

    solveStatus <- fx_getStat ipopt "return_status"  >>= printableObject_getDescription

    fopt <- fmap (V.head . ddata) $ ioInterfaceFX_output'' ipopt "f"
    xopt <- fmap ddata $ ioInterfaceFX_output'' ipopt "x"
    gopt <- fmap ddata $ ioInterfaceFX_output'' ipopt "g"
    lamXOpt <- fmap ddata $ ioInterfaceFX_output'' ipopt "lam_x"
    lamGOpt <- fmap ddata $ ioInterfaceFX_output'' ipopt "lam_g"
    let lambdaOut = Multipliers { lambdaX = lamXOpt
                                , lambdaG = lamGOpt
                                }
        nlpOut = NlpOut { fOpt = fopt
                        , xOpt = xopt
                        , gOpt = gopt
                        , lambdaOpt = lambdaOut
                        }
    return $ if solveStatus `elem` ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
      then Right nlpOut
      else Left solveStatus


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
  let inputsXMat = svector inputsX
      inputsPMat = svector inputsP
      objMat     = svector (V.singleton obj)
      gMat       = svector g

  inputScheme <- mkSchemeSXMatrix SCHEME_NLPInput [("x", inputsXMat), ("p", inputsPMat)]
  outputScheme <- mkSchemeSXMatrix SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  f <- sxFunction''' inputScheme outputScheme
  ipopt <- ipoptSolver'' (castFX f)

  -- add callback if user provides it
  case callback' of
    Nothing -> return ()
    Just callback -> do
      let cb fx' = do
            xval <- fmap ddata $ ioInterfaceFX_output fx' 0
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
        setOption "max_iter" (3000 :: Int)
        --setOption "tol" (1e-9 :: Double)
        --setOption "fixed_variable_treatment" "make_constraint" -- causes segfaults?
        --setOption "fixed_variable_treatment" "make_parameter"
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
      Right NlpOut { fOpt = fopt
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
          Right NlpOut { fOpt = fopt
                       , xOpt = vectorize xopt
                       , gOpt = vectorize gopt
                       , lambdaOpt = Multipliers { lambdaX = vectorize lambdax
                                                 , lambdaG = vectorize lambdag
                                                 }}


--solveOcpIpopt ::
--  forall x z u p r c h n deg .
--  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize c, Vectorize h)
--  => OcpPhase x z u p r c h -> Maybe (CollTraj x z u p n deg Double) -> IO ()
--solveOcpIpopt ocp guess' = do
--    let guess :: CollTraj x z u p n deg Double
--        guess = case guess' of Nothing -> fill 1
--                               Just g -> g
--    _ <- solveNlpIpopt ((makeCollNlp ocp) {nlpX0 = guess}) Nothing
--    return ()

solveOcpIpopt' ::
  forall x z u p r o c h .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize o, Vectorize c, Vectorize h)
  => Int -> Int -> OcpPhase x z u p r o c h -> IO ()
solveOcpIpopt' n deg ocp =
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) -> do
    let guess :: CollTraj x z u p n deg Double
        guess = fill 1
    _ <- solveNlpIpopt ((makeCollNlp ocp) {nlpX0 = guess}) Nothing
    return ()

solveStaticOcpIpopt ::
  Int -> Int -> OcpPhase V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector -> IO ()
solveStaticOcpIpopt n deg ocp = reifyOcp ocp (solveOcpIpopt' n deg)
