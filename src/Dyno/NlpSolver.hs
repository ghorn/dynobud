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
--       , solveStaticNlp
       , solveOcp
--       , solveStaticOcp
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
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Casadi.Wrappers.Enums ( InputOutputScheme(..) )
import Casadi.Callback
import Casadi.Wrappers.Classes.FX ( FX, fx_getStat, castFX )
import Casadi.Wrappers.Classes.OptionsFunctionality ( optionsFunctionality_setOption )
import Casadi.Wrappers.Classes.PrintableObject ( printableObject_getDescription )
import Casadi.Wrappers.Classes.GenericType
import Casadi.Wrappers.Classes.SharedObject
import Casadi.Wrappers.Classes.NLPSolver
import Casadi.Wrappers.Classes.IOInterfaceFX

import Dyno.Casadi.DMatrix

--import Dyno.NlpMonad ( reifyNlp )
import Dyno.View.View
import Dyno.View.Symbolic
import Dyno.Nlp ( Nlp(..), NlpOut(..), Multipliers(..), Bounds )
import Dyno.DirectCollocation ( CollTraj, makeCollNlp )
import Dyno.DirectCollocation.Dynamic ( DynCollTraj, ctToDynamic )
import Data.Proxy
import qualified Dyno.TypeVecs as TV
--import Dyno.OcpMonad ( reifyOcp )
import Dyno.Ocp ( OcpPhase )

type VD a = J a (Vector Double)

data NlpSolverStuff nlp =
  NlpSolverStuff
  { nlpConstructor :: FX -> IO nlp
  , defaultOptions :: [(String,Opt)]
  , options :: [(String,Opt)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  }
data Opt where
  Opt :: NlpOption a => a -> Opt

setInput :: (NlpState -> Int) -> String -> Vector Double -> NlpSolver x p g ()
setInput getLen name x = do
  nlpState <- ask
  let nx' = V.length x
      nx = getLen nlpState
  when (nx /= nx') $ error $ name ++ " dimension mismatch, " ++ show nx ++ " (true) /= " ++ show nx' ++ " (given)"
  liftIO $ ioInterfaceFX_setInput''''' (isSolver nlpState) x name
  return ()

setX0 :: forall x p g. View x => VD x -> NlpSolver x p g ()
setX0 = (setInput isNx "x0") . unJ

setLbx :: View x => VD x -> NlpSolver x p g ()
setLbx = setInput isNx "lbx" . unJ

setUbx :: View x => VD x -> NlpSolver x p g ()
setUbx = setInput isNx "ubx" . unJ

setLbg :: View g => VD g -> NlpSolver x p g ()
setLbg = setInput isNg "lbg" . unJ

setUbg :: View g => VD g -> NlpSolver x p g ()
setUbg = setInput isNg "ubg" . unJ

setP :: View p => VD p -> NlpSolver x p g ()
setP = setInput isNp "p" . unJ


getInput :: String -> NlpSolver x p g (Vector Double)
getInput name = do
  nlpState <- ask
  liftIO $ fmap ddata $ ioInterfaceFX_input'' (isSolver nlpState) name

getX0 :: View x => NlpSolver x p g (VD x)
getX0 = liftM mkJ $ getInput "x0"

getLbx :: View x => NlpSolver x p g (VD x)
getLbx = liftM mkJ $ getInput "lbx"

getUbx :: View x => NlpSolver x p g (VD x)
getUbx = liftM mkJ $ getInput "ubx"

getLbg :: View g => NlpSolver x p g (VD g)
getLbg = liftM mkJ $ getInput "lbg"

getUbg :: View g => NlpSolver x p g (VD g)
getUbg = liftM mkJ $ getInput "ubg"

getP :: View p => NlpSolver x p g (VD p)
getP = liftM mkJ $ getInput "p"

getOutput :: String -> NlpSolver x p g (Vector Double)
getOutput name = do
  nlpState <- ask
  liftIO $ fmap ddata $ ioInterfaceFX_output'' (isSolver nlpState) name

getF :: NlpSolver x p g (VD S)
getF = liftM mkJ $ getOutput "f"

getX :: View x => NlpSolver x p g (VD x)
getX = liftM mkJ $ getOutput "x"

getG :: View g => NlpSolver x p g (VD g)
getG = liftM mkJ $ getOutput "g"

getLamX :: View x => NlpSolver x p g (VD x)
getLamX = liftM mkJ $ getOutput "lam_x"

getLamG :: View g => NlpSolver x p g (VD g)
getLamG = liftM mkJ $ getOutput "lam_g"


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
instance NlpOption (Vector Bool) where
  mkGeneric = genericType'''''
instance NlpOption (Vector Int) where
  mkGeneric = genericType''''''
instance NlpOption (Vector Double) where
  mkGeneric = genericType'''''''
instance NlpOption (Vector String) where
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
solve' :: (View x, View g) => NlpSolver x p g (Either String String, NlpOut x g (Vector Double))
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

runNlpSolver ::
  forall nlp x p g a s .
  (NLPSolverClass nlp, View x, View p, View g, Symbolic s)
  => NlpSolverStuff nlp
  -> ((J x s, J p s) -> (J S s, J g s))
  -> Maybe (J x (Vector Double) -> IO Bool)
  -> NlpSolver x p g a
  -> IO a
runNlpSolver solverStuff nlpFun callback' (NlpSolver nlpMonad) = do
  inputsX <- sym "x"
  inputsP <- sym "p"

  let (obj, g) = nlpFun (inputsX, inputsP)

  let inputsXMat = unJ inputsX
      inputsPMat = unJ inputsP
      objMat     = unJ obj
      gMat       = unJ g

  inputScheme <- mkScheme SCHEME_NLPInput [("x", inputsXMat), ("p", inputsPMat)]
  outputScheme <- mkScheme SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  f <- mkFunction inputScheme outputScheme
  nlp <- fmap castNLPSolver $ (nlpConstructor solverStuff) (castFX f)

  -- add callback if user provides it
  intref <- newIORef False
  let cb fx' = do
        callbackRet <- case callback' of
          Nothing -> return True
          Just callback -> do
            xval <- fmap (mkJ . ddata . ddense) $ ioInterfaceFX_output fx' 0
            callback xval
        interrupt <- readIORef intref
        return $ if callbackRet && not interrupt then 0 else fromIntegral (solverInterruptCode solverStuff)
  casadiCallback <- makeCallback cb >>= genericType''''''''''''
  optionsFunctionality_setOption nlp "iteration_callback" casadiCallback

  -- set all the user options
  mapM_ (\(l,Opt r) -> mkGeneric r >>= optionsFunctionality_setOption nlp l)
    (defaultOptions solverStuff)
  mapM_ (\(l,Opt r) -> mkGeneric r >>= optionsFunctionality_setOption nlp l)
    (options solverStuff)
  sharedObject_init' nlp

  let nlpState = NlpState { isNx = size (proxy inputsX)
                          , isNp = size (proxy inputsP)
                          , isNg = size (proxy g)
                          , isSolver = nlp
                          , isInterrupt = writeIORef intref True
                          , isSuccessCodes = successCodes solverStuff
                          }
  liftIO $ runReaderT nlpMonad nlpState
proxy :: J a b -> Proxy a
proxy = const Proxy

-- | convenience function to solve a pure Nlp
solveNlp ::
  (NLPSolverClass nlp, View x, View p, View g, Symbolic a) =>
  NlpSolverStuff nlp ->
  Nlp x p g a -> Maybe (J x (Vector Double) -> IO Bool)
  -> IO (Either String String, NlpOut x g (Vector Double))
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

toBnds :: View f => J f (Vector Bounds) -> (VD f, VD f)
toBnds vs = (mkJ (V.map (fromMaybe (-inf)) lbs), mkJ (V.map (fromMaybe inf) ubs))
  where
    (lbs, ubs) = V.unzip (unJ vs)


--solveStaticNlp ::
--  NLPSolverClass nlp =>
--  NlpSolverStuff nlp ->
--  Nlp Vector Vector Vector ->
--  Maybe (Vector Double -> IO Bool) -> IO (Either String String, NlpOut Vector Vector Double)
--solveStaticNlp solverStuff nlp callback = reifyNlp nlp callback foo
--  where
--    foo ::
--      (View x, View p, View g) =>
--      Nlp x p g -> Maybe (x Double -> IO Bool) ->
--      IO (Either String String, NlpOut Vector Vector Double)
--    foo nlp' cb' = do
--      (ret,nlpOut) <- solveNlp solverStuff nlp' cb'
--      let nlpOut' = NlpOut { fOpt = fOpt nlpOut
--                           , xOpt = vectorize (xOpt nlpOut)
--                           , gOpt = vectorize (gOpt nlpOut)
--                           , lambdaOpt = Multipliers { lambdaX = vectorize (lambdaX (lambdaOpt nlpOut))
--                                                     , lambdaG = vectorize (lambdaG (lambdaOpt nlpOut))
--                                                     }}
--      return (ret, nlpOut')

solveOcp ::
  forall x z u p r o c h s sh sc nlp .
  (NLPSolverClass nlp, View x, View z, View u, View p, View r, View o, View c, View h, View s, View sc, View sh)
  => NlpSolverStuff nlp -> Int -> Int -> Maybe (DynCollTraj (Vector Double) -> IO Bool) -> OcpPhase x z u p r o c h s sh sc -> IO ()
solveOcp solverStuff n deg cb ocp =
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) -> do
    let guess :: J (CollTraj x z u p s n deg) (Vector Double)
        guess = jfill 1
    nlp <- makeCollNlp ocp
    _ <- solveNlp solverStuff (nlp {nlpX0 = guess}) (fmap (. ctToDynamic) cb)
    return ()

--solveStaticOcp ::
--  NLPSolverClass nlp =>
--  NlpSolverStuff nlp -> Int -> Int -> Maybe (DynCollTraj Double -> IO Bool) -> OcpPhase Vector Vector Vector Vector Vector Vector Vector Vector Vector Vector Vector -> IO ()
--solveStaticOcp solverStuff n deg cb ocp = reifyOcp ocp (solveOcp solverStuff n deg cb)
