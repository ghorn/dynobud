{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PackageImports #-}
{-# Language KindSignatures #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Dyno.NlpSolver
       ( NlpSolver
       , NLPSolverClass
       , runNlpSolver
       , solveNlp
       , solveStaticNlp
       , solveOcp
       , solveStaticOcp
         -- * solve
       , solve
       , solve'
         -- * inputs
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
         -- * outputs
       , getF
       , getX
       , getG
       , getLamX
       , getLamG
       , NlpSolverStuff(..)
         -- * options
       , Opt(..)
       , NlpOption
       , setOption
       , reinit
       ) where

import Control.Exception ( AsyncException( UserInterrupt ), try )
import Control.Concurrent ( forkIO, newEmptyMVar, takeMVar, putMVar )
import Control.Applicative ( Applicative )
import Control.Monad ( liftM, when )
import "mtl" Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import Data.Maybe ( fromMaybe )
import Data.IORef ( newIORef, readIORef, writeIORef )
import qualified Data.Vector as V

import Casadi.Wrappers.Enums ( InputOutputScheme(..) )
import Casadi.Callback
import Casadi.Wrappers.Classes.FX ( FX, fx_getStat, castFX )
import Casadi.Wrappers.Classes.OptionsFunctionality ( optionsFunctionality_setOption )
import Casadi.Wrappers.Classes.PrintableObject ( printableObject_getDescription )
import Casadi.Wrappers.Classes.GenericType
import Casadi.Wrappers.Classes.SXFunction ( sxFunction''' )
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.NLPSolver
import Casadi.Wrappers.Classes.IOInterfaceFX

import Dyno.Casadi.SX
import Dyno.Casadi.DMatrix
import Dyno.Casadi.SXElement
import Dyno.Casadi.IOSchemes

import Dyno.NlpMonad ( reifyNlp )
import Dyno.Vectorize
import Dyno.Nlp ( Nlp(..), NlpFun(..), NlpInputs(..), NlpOut(..), Multipliers(..) )
import Dyno.DirectCollocation ( CollTraj, makeCollNlp )
import Dyno.DirectCollocation.Dynamic ( DynCollTraj, ctToDynamic )
import Data.Proxy
import qualified Dyno.TypeVecs as TV
import Dyno.OcpMonad ( reifyOcp )
import Dyno.Ocp ( OcpPhase )

data NlpSolverStuff nlp =
  NlpSolverStuff
  { nlpConstructor :: FX -> IO nlp
  , defaultOptions :: [(String,Opt)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  }
data Opt where
  Opt :: NlpOption a => a -> Opt

setInput :: (NlpState -> Int) -> String -> V.Vector Double -> NlpSolver x p g ()
setInput getLen name x = do
  nlpState <- ask
  let nx' = V.length x
      nx = getLen nlpState
  when (nx /= nx') $ error $ name ++ " dimension mismatch, " ++ show nx ++ " (true) /= " ++ show nx' ++ " (given)"
  liftIO $ ioInterfaceFX_setInput''''' (isSolver nlpState) x name
  return ()

setX0 :: Vectorize x => x Double -> NlpSolver x p g ()
setX0 = (setInput isNx "x0") . vectorize

setLbx :: Vectorize x => x Double -> NlpSolver x p g ()
setLbx = setInput isNx "lbx" . vectorize

setUbx :: Vectorize x => x Double -> NlpSolver x p g ()
setUbx = setInput isNx "ubx" . vectorize

setLbg :: Vectorize g => g Double -> NlpSolver x p g ()
setLbg = setInput isNg "lbg" . vectorize

setUbg :: Vectorize g => g Double -> NlpSolver x p g ()
setUbg = setInput isNg "ubg" . vectorize

setP :: Vectorize p => p Double -> NlpSolver x p g ()
setP = setInput isNp "p" . vectorize


getInput :: String -> NlpSolver x p g (V.Vector Double)
getInput name = do
  nlpState <- ask
  liftIO $ fmap ddata $ ioInterfaceFX_input'' (isSolver nlpState) name

getX0 :: Vectorize x => NlpSolver x p g (x Double)
getX0 = liftM devectorize $ getInput "x0"

getLbx :: Vectorize x => NlpSolver x p g (x Double)
getLbx = liftM devectorize $ getInput "lbx"

getUbx :: Vectorize x => NlpSolver x p g (x Double)
getUbx = liftM devectorize $ getInput "ubx"

getLbg :: Vectorize g => NlpSolver x p g (g Double)
getLbg = liftM devectorize $ getInput "lbg"

getUbg :: Vectorize g => NlpSolver x p g (g Double)
getUbg = liftM devectorize $ getInput "ubg"

getP :: Vectorize p => NlpSolver x p g (p Double)
getP = liftM devectorize $ getInput "p"

getOutput :: String -> NlpSolver x p g (V.Vector Double)
getOutput name = do
  nlpState <- ask
  liftIO $ fmap ddata $ ioInterfaceFX_output'' (isSolver nlpState) name

getF :: NlpSolver x p g Double
getF = liftM V.head $ getOutput "f"

getX :: Vectorize x => NlpSolver x p g (x Double)
getX = liftM devectorize $ getOutput "x"

getG :: Vectorize g => NlpSolver x p g (g Double)
getG = liftM devectorize $ getOutput "g"

getLamX :: Vectorize x => NlpSolver x p g (x Double)
getLamX = liftM devectorize $ getOutput "lam_x"

getLamG :: Vectorize g => NlpSolver x p g (g Double)
getLamG = liftM devectorize $ getOutput "lam_g"


class NlpOption a where
  mkGeneric :: a -> IO GenericType

instance NlpOption Bool where
  mkGeneric = genericType'
instance NlpOption Int where
  mkGeneric = genericType''
instance NlpOption Double where
  mkGeneric = genericType'''
instance NlpOption String where
  mkGeneric = genericType''''
instance NlpOption (V.Vector Bool) where
  mkGeneric = genericType'''''
instance NlpOption (V.Vector Int) where
  mkGeneric = genericType''''''
instance NlpOption (V.Vector Double) where
  mkGeneric = genericType'''''''
instance NlpOption (V.Vector String) where
  mkGeneric = genericType''''''''
instance NlpOption GenericType where
  mkGeneric = return

setOption :: NlpOption a => String -> a -> NlpSolver x p g ()
setOption name val = do
  nlpState <- ask
  let nlp = isSolver nlpState
  liftIO $ do
    gt <- mkGeneric val
    optionsFunctionality_setOption nlp name gt

reinit :: NlpSolver x p g ()
reinit = do
  nlpState <- ask
  let nlp = isSolver nlpState
  liftIO $ sharedObject_init' nlp

-- | solve with current inputs, return success or failure code
solve :: NlpSolver x p g (Either String String)
solve = do
  nlpState <- ask
  let nlp = isSolver nlpState
  solveStatus <- liftIO $ do

    stop <- newEmptyMVar -- mvar that will be filled when nlp finishes
    _ <- forkIO (fxSolveSafe nlp >> putMVar stop ())
    -- wait until nlp finishes
    ret <- try (takeMVar stop)
    case ret of Right () -> return () -- no exceptions
                Left UserInterrupt -> do -- got ctrl-C
                  isInterrupt nlpState -- tell nlp to stop iterations
                  _ <- takeMVar stop -- wait for nlp to return
                  return ()
                Left _ -> takeMVar stop >> return () -- don't handle this one
    fx_getStat nlp "return_status"  >>= printableObject_getDescription

  return $ if solveStatus `elem` (isSuccessCodes nlpState)
    then Right solveStatus
    else Left solveStatus

-- | solve with current inputs, return lots of info on success, or message on failure
solve' :: (Vectorize x, Vectorize g) => NlpSolver x p g (Either String String, NlpOut x g Double)
solve' = do
  solveStatus <- solve

  fopt <- getF
  xopt <- getX
  gopt <- getG
  lamXOpt <- getLamX
  lamGOpt <- getLamG
  let lambdaOut = Multipliers { lambdaX = lamXOpt
                              , lambdaG = lamGOpt
                              }
      nlpOut = NlpOut { fOpt = fopt
                      , xOpt = xopt
                      , gOpt = gopt
                      , lambdaOpt = lambdaOut
                      }

  --liftIO $ putStrLn $ "solve status: " ++ show solveStatus
  return (solveStatus, nlpOut)


data NlpState = NlpState { isNx :: Int
                         , isNg :: Int
                         , isNp :: Int
                         , isSolver :: NLPSolver
                         , isInterrupt :: IO ()
                         , isSuccessCodes :: [String]
                         }
newtype NlpSolver (x :: * -> *) (p :: * -> *) (g :: * -> *) a =
  NlpSolver (ReaderT NlpState IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader NlpState
           , MonadIO
           )

runNlpSolver :: (NLPSolverClass nlp, Vectorize x, Vectorize p, Vectorize g) =>
  NlpSolverStuff nlp ->
  (NlpInputs x p SXElement -> NlpFun g SXElement)
  -> Maybe (x Double -> IO Bool)
  -> NlpSolver x p g a
  -> IO a
runNlpSolver solverStuff nlpFun callback' (NlpSolver nlpMonad) = do
  (NlpInputs inputsX' inputsP', NlpFun obj g') <- funSXToSX nlpFun

  let inputsX = vectorize inputsX'
      inputsP = vectorize inputsP'
      g = vectorize g'
      inputsXMat = svector inputsX
      inputsPMat = svector inputsP
      objMat     = svector (V.singleton obj)
      gMat       = svector g

  inputScheme <- mkSchemeSXMatrix SCHEME_NLPInput [("x", inputsXMat), ("p", inputsPMat)]
  outputScheme <- mkSchemeSXMatrix SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  f <- sxFunction''' inputScheme outputScheme
  sharedObject_init' f
  nlp <- fmap castNLPSolver $ (nlpConstructor solverStuff) (castFX f)

  -- add callback if user provides it
  intref <- newIORef False
  let cb fx' = do
        callbackRet <- case callback' of
          Nothing -> return True
          Just callback -> do
            xval <- fmap ddata $ ioInterfaceFX_output fx' 0
            callback (devectorize xval)
        interrupt <- readIORef intref
        return $ if callbackRet && not interrupt then 0 else fromIntegral (solverInterruptCode solverStuff)
  casadiCallback <- makeCallback cb >>= genericType''''''''''''
  optionsFunctionality_setOption nlp "iteration_callback" casadiCallback

  -- set all the user options
  mapM_ (\(l,Opt r) -> mkGeneric r >>= optionsFunctionality_setOption nlp l) (defaultOptions solverStuff)
  sharedObject_init' nlp

  let nlpState = NlpState { isNx = V.length inputsX
                          , isNp = V.length inputsP
                          , isNg = V.length g
                          , isSolver = nlp
                          , isInterrupt = writeIORef intref True
                          , isSuccessCodes = successCodes solverStuff
                          }
  liftIO $ runReaderT nlpMonad nlpState


-- | convenience function to solve a pure Nlp
solveNlp ::
  (NLPSolverClass nlp, Vectorize x, Vectorize p, Vectorize g) =>
  NlpSolverStuff nlp ->
  Nlp x p g -> Maybe (x Double -> IO Bool)
  -> IO (Either String String, NlpOut x g Double)
solveNlp solverStuff nlp callback = do
  runNlpSolver solverStuff (nlpFG nlp) callback $ do

    let (lbx,ubx) = toBnds (nlpBX nlp)
        (lbg,ubg) = toBnds (nlpBG nlp)

    setX0 (nlpX0 nlp)
    setP (nlpP nlp)
    setLbx lbx
    setUbx ubx
    setLbg lbg
    setUbg ubg
    solve'


inf :: Double
inf = read "Infinity"

toBnds :: Vectorize f => f (Maybe Double, Maybe Double) -> (f Double, f Double)
toBnds vs = (devectorize (V.map (fromMaybe (-inf)) lb), devectorize (V.map (fromMaybe inf) ub))
  where
    (lb,ub) = V.unzip (vectorize vs)


solveStaticNlp ::
  NLPSolverClass nlp =>
  NlpSolverStuff nlp ->
  Nlp V.Vector V.Vector V.Vector ->
  Maybe (V.Vector Double -> IO Bool) -> IO (Either String String, NlpOut V.Vector V.Vector Double)
solveStaticNlp solverStuff nlp callback = reifyNlp nlp callback foo
  where
    foo ::
      (Vectorize x, Vectorize p, Vectorize g) =>
      Nlp x p g -> Maybe (x Double -> IO Bool) ->
      IO (Either String String, NlpOut V.Vector V.Vector Double)
    foo nlp' cb' = do
      (ret,nlpOut) <- solveNlp solverStuff nlp' cb'
      let nlpOut' = NlpOut { fOpt = fOpt nlpOut
                           , xOpt = vectorize (xOpt nlpOut)
                           , gOpt = vectorize (gOpt nlpOut)
                           , lambdaOpt = Multipliers { lambdaX = vectorize (lambdaX (lambdaOpt nlpOut))
                                                     , lambdaG = vectorize (lambdaG (lambdaOpt nlpOut))
                                                     }}
      return (ret, nlpOut')


solveOcp ::
  forall x z u p r o c h s sh sc nlp .
  (NLPSolverClass nlp, Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize o, Vectorize c, Vectorize h, Vectorize s, Vectorize sc, Vectorize sh)
  => NlpSolverStuff nlp -> Int -> Int -> Maybe (DynCollTraj Double -> IO Bool) -> OcpPhase x z u p r o c h s sh sc -> IO ()
solveOcp solverStuff n deg cb ocp =
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) -> do
    let guess :: CollTraj x z u p s n deg Double
        guess = fill 1
    _ <- solveNlp solverStuff ((makeCollNlp ocp) {nlpX0 = guess}) (fmap (. ctToDynamic) cb)
    return ()

solveStaticOcp ::
  NLPSolverClass nlp =>
  NlpSolverStuff nlp -> Int -> Int -> Maybe (DynCollTraj Double -> IO Bool) -> OcpPhase V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector -> IO ()
solveStaticOcp solverStuff n deg cb ocp = reifyOcp ocp (solveOcp solverStuff n deg cb)
