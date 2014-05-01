{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PackageImports #-}
{-# Language KindSignatures #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Dyno.NlpSolver
       ( NlpSolver
       , NLPSolverClass
       , runNlpSolver
         -- * solve
       , solveNlp
       , solveNlp'
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
       , Op.Opt(..)
       , setOption
       , reinit
       ) where

--import System.Process ( callProcess, showCommandForUser )
import Control.Exception ( AsyncException( UserInterrupt ), try )
import Control.Concurrent ( forkIO, newEmptyMVar, takeMVar, putMVar )
import Control.Applicative ( Applicative(..) )
import Control.Monad ( liftM, when, void )
import "mtl" Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import Data.Maybe ( fromMaybe )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Casadi.Core.Enums ( InputOutputScheme(..) )
import Casadi.Core.Classes.Function ( function_getStat, castFunction )
import Casadi.Core.Classes.PrintableObject ( printableObject_getDescription )
import Casadi.Core.Classes.GenericType
import Casadi.Core.Classes.NLPSolver
import Casadi.Core.Classes.IOInterfaceFunction
--import Casadi.Wrappers.Classes.CasadiOptions

import Dyno.Casadi.Callback ( makeCallback )
import Dyno.Casadi.DMatrix
import Dyno.Casadi.SX
import Dyno.Casadi.SXElement ( SXElement )
import Dyno.Casadi.Function
import qualified Dyno.Casadi.Option as Op
import Dyno.Casadi.SharedObject ( soInit )
import qualified Casadi.Core.Classes.Function as C
--import qualified Casadi.Wrappers.Classes.Sparsity as C

import Dyno.Vectorize ( Vectorize(..) )
import Dyno.View.View
import Dyno.View.Symbolic
import Dyno.Nlp ( Nlp(..), NlpOut(..), Multipliers(..), Nlp'(..), NlpOut'(..), Multipliers'(..), Bounds )
import Data.Proxy

type VD a = J a (Vector Double)

data NlpSolverStuff nlp =
  NlpSolverStuff
  { nlpConstructor :: Function -> IO nlp
  , defaultOptions :: [(String,Op.Opt)]
  , options :: [(String,Op.Opt)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  }

setInput :: (NlpState -> Int) -> String -> Vector Double -> NlpSolver x p g ()
setInput getLen name x = do
  nlpState <- ask
  let nx' = V.length x
      nx = getLen nlpState
  when (nx /= nx') $ error $ name ++ " dimension mismatch, " ++ show nx ++ " (true) /= " ++ show nx' ++ " (given)"
  liftIO $ ioInterfaceFunction_setInput__3 (isSolver nlpState) x name
  return ()

setX0 :: forall x p g. View x => VD x -> NlpSolver x p g ()
setX0 = setInput isNx "x0" . unJ

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
  liftIO $ fmap ddata $ ioInterfaceFunction_input__0 (isSolver nlpState) name

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
  liftIO $ fmap ddata $ ioInterfaceFunction_output__0 (isSolver nlpState) name

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


setOption :: Op.Option a => String -> a -> NlpSolver x p g ()
setOption name val = do
  nlpState <- ask
  let nlp = isSolver nlpState
  liftIO $ Op.setOption nlp name val


reinit :: NlpSolver x p g ()
reinit = do
  nlpState <- ask
  let nlp = isSolver nlpState
  liftIO $ soInit nlp

-- | solve with current inputs, return success or failure code
solve :: NlpSolver x p g (Either String String)
solve = do
  nlpState <- ask
  let nlp = isSolver nlpState
  solveStatus <- liftIO $ do

    stop <- newEmptyMVar -- mvar that will be filled when nlp finishes
    _ <- forkIO (C.function_solve nlp >> putMVar stop ())
    -- wait until nlp finishes
    ret <- try (takeMVar stop)
    case ret of Right () -> return () -- no exceptions
                Left UserInterrupt -> do -- got ctrl-C
                  isInterrupt nlpState -- tell nlp to stop iterations
                  _ <- takeMVar stop -- wait for nlp to return
                  return ()
                Left _ -> void (takeMVar stop) -- don't handle this one
    function_getStat nlp "return_status"  >>= printableObject_getDescription

  return $ if solveStatus `elem` isSuccessCodes nlpState
    then Right solveStatus
    else Left solveStatus

-- | solve with current inputs, return lots of info on success, or message on failure
solve' :: (View x, View g) => NlpSolver x p g (Either String String, NlpOut' x g (Vector Double))
solve' = do
  solveStatus <- solve

  fopt <- getF
  xopt <- getX
  gopt <- getG
  lamXOpt <- getLamX
  lamGOpt <- getLamG
  let lambdaOut = Multipliers' { lambdaX' = lamXOpt
                               , lambdaG' = lamGOpt
                               }
      nlpOut = NlpOut' { fOpt' = fopt
                       , xOpt' = xopt
                       , gOpt' = gopt
                       , lambdaOpt' = lambdaOut
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

--generateAndCompile :: String -> Function -> IO Function
--generateAndCompile name f = do
--  writeFile (name ++ ".c") (generateCode f)
--  let cmd = "clang"
--      args = ["-fPIC","-shared","-O","-Wall","-Wno-unused-variable",name++".c","-o",name++".so"]
--  putStrLn (showCommandForUser cmd args)
--  callProcess cmd args
--  externalFunction ("./"++name++".so")

runNlpSolver ::
  forall nlp x p g a s .
  (NLPSolverClass nlp, View x, View p, View g, Symbolic s)
  => NlpSolverStuff nlp
  -> (J x s -> J p s -> (J S s, J g s))
--  -> (J x (Vector Double))
  -> Maybe (J x (Vector Double) -> IO Bool)
  -> NlpSolver x p g a
  -> IO a
--runNlpSolver solverStuff nlpFun nlpX0' callback' (NlpSolver nlpMonad) = do
runNlpSolver solverStuff nlpFun callback' (NlpSolver nlpMonad) = do
  inputsX <- sym "x"
  inputsP <- sym "p"

  let (obj, g) = nlpFun inputsX inputsP

  let inputsXMat = unJ inputsX
      inputsPMat = unJ inputsP
      objMat     = unJ obj
      gMat       = unJ g

  inputScheme <- mkScheme SCHEME_NLPInput [("x", inputsXMat), ("p", inputsPMat)]
  outputScheme <- mkScheme SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  nlp <- mkFunction "nlp" inputScheme outputScheme
--  Op.setOption nlp "verbose" True
  soInit nlp

--  let eval 0 = error "finished"
--      eval k = do
--        putStrLn "setting input"
--        ioInterfaceFunction_setInput''' nlp (unJ nlpX0') (0::Int)
--        putStrLn $ "evaluating " ++ show k
--        C.function_evaluate nlp
--        eval (k-1 :: Int)
--  eval (300::Int)
--  casadiOptions_stopProfiling
--  _ <- error "done"


--  jac_sparsity <- C.function_jacSparsity nlp 0 1 True False
--  C.sparsity_spyMatlab jac_sparsity "jac_sparsity_reorder.m"


  solver <- fmap castNLPSolver $ nlpConstructor solverStuff (castFunction nlp)

  -- add callback if user provides it
  intref <- newIORef False
  let cb function' = do
        callbackRet <- case callback' of
          Nothing -> return True
          Just callback -> do
            xval <- fmap (mkJ . ddata . ddense) $ ioInterfaceFunction_output__2 function' 0
            callback xval
        interrupt <- readIORef intref
        return $ if callbackRet && not interrupt then 0 else fromIntegral (solverInterruptCode solverStuff)
  casadiCallback <- makeCallback cb >>= genericType__0
  Op.setOption solver "iteration_callback" casadiCallback
--  grad_f <- gradient nlp 0 0
--  soInit grad_f
--  jac_g <- jacobian nlp 0 1 True False
--  soInit jac_g
--
--  let eval 0 = error "finished"
--      eval k = do
--        putStrLn "setting input"
--        ioInterfaceFunction_setInput''' jac_g (unJ nlpX0') (0::Int)
--        putStrLn $ "evaluating " ++ show k
--        C.function_evaluate jac_g
--        eval (k-1 :: Int)
--  eval (40::Int)

--  nlp' <- generateAndCompile "nlp" nlp
--  grad_f' <- generateAndCompile "grad_f" grad_f
--  jac_g' <- generateAndCompile "jac_g" jac_g
--  _ <- error "lal"
--  Op.setOption solver "grad_f" grad_f'
--  Op.setOption solver "jac_g" jac_g'

  -- set all the user options
  mapM_ (\(l,Op.Opt o) -> Op.setOption solver l o) (defaultOptions solverStuff ++ options solverStuff)
  putStrLn "initializing nlp solver..."
  soInit solver
  putStrLn "solver initialized"

  let nlpState = NlpState { isNx = size (proxy inputsX)
                          , isNp = size (proxy inputsP)
                          , isNg = size (proxy g)
                          , isSolver = solver
                          , isInterrupt = writeIORef intref True
                          , isSuccessCodes = successCodes solverStuff
                          }
  liftIO $ runReaderT nlpMonad nlpState
proxy :: J a b -> Proxy a
proxy = const Proxy

-- | convenience function to solve a pure Nlp'
solveNlp :: forall x p g nlp .
  (NLPSolverClass nlp, Vectorize x, Vectorize p, Vectorize g) =>
  NlpSolverStuff nlp ->
  Nlp x p g SXElement -> Maybe (x Double -> IO Bool)
  -> IO (Either String String, NlpOut x g Double)
solveNlp solverStuff nlp callback = do
  let nlp' :: Nlp' (JV x) (JV p) (JV g) SX
      nlp' = Nlp' { nlpFG' = \x' p' -> let x = devectorize (sdata (sdense (unJ x'))) :: x SXElement
                                           p = devectorize (sdata (sdense (unJ p'))) :: p SXElement
                                           (obj,g) = nlpFG nlp x p
                                           obj' = mkJ (svector (V.singleton obj))
                                           g' = mkJ (svector (vectorize g))
                                       in (obj',g')
                  , nlpBX' = mkJ $ vectorize (nlpBX nlp) :: J (JV x) (V.Vector Bounds)
                  , nlpBG' = mkJ $ vectorize (nlpBG nlp) :: J (JV g) (V.Vector Bounds)
                  , nlpX0' = mkJ $ vectorize (nlpX0 nlp) :: J (JV x) (V.Vector Double)
                  , nlpP'  = mkJ $ vectorize (nlpP  nlp) :: J (JV p) (V.Vector Double)
                  }

      callback' :: Maybe (J (JV x) (Vector Double) -> IO Bool)
      callback' = fmap (. devectorize . unJ) callback

  (r0, r1') <- solveNlp' solverStuff nlp' callback'

  let lambda :: Multipliers x g Double
      lambda = Multipliers { lambdaX = devectorize $ unJ $ lambdaX' $ lambdaOpt' r1'
                           , lambdaG = devectorize $ unJ $ lambdaG' $ lambdaOpt' r1'
                           }
      r1 :: NlpOut x g Double
      r1 = NlpOut { fOpt = V.head $ unJ (fOpt' r1')
                  , xOpt = devectorize $ unJ (xOpt' r1')
                  , gOpt = devectorize $ unJ (gOpt' r1')
                  , lambdaOpt = lambda
                  }

  return (r0, r1)

-- | convenience function to solve a pure Nlp'
solveNlp' ::
  (NLPSolverClass nlp, View x, View p, View g, Symbolic a) =>
  NlpSolverStuff nlp ->
  Nlp' x p g a -> Maybe (J x (Vector Double) -> IO Bool)
  -> IO (Either String String, NlpOut' x g (Vector Double))
solveNlp' solverStuff nlp callback =
--  runNlpSolver solverStuff (nlpFG' nlp) (nlpX0' nlp) callback $ do
  runNlpSolver solverStuff (nlpFG' nlp) callback $ do
    let (lbx,ubx) = toBnds (nlpBX' nlp)
        (lbg,ubg) = toBnds (nlpBG' nlp)

    setX0 (nlpX0' nlp)
    setP (nlpP' nlp)
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
