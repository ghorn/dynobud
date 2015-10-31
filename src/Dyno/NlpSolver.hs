{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dyno.NlpSolver
       ( NlpSolver
       , runNlpSolver
       , RunNlpOptions(..)
       , runNlpSolverWith
       , defaultRunnerOptions
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
       , setLamX0
       , setLamG0
       , getX0
       , getP
       , getLbx
       , getUbx
       , getLbg
       , getUbg
       , getLamX0
       , getLamG0
         -- * outputs
       , getF
       , getX
       , getG
       , getLamX
       , getLamG
       , getStat
       , getNlpOut
         -- * kkt conditions, evalKKT is in user units, evalScaledKKT is the internal one
       , evalGradF
       , evalJacG
       , evalHessF
       , evalHessLambdaG
       , evalKKT
       , evalScaledGradF
       , evalScaledJacG
       , evalScaledHessLag
       , evalScaledHessF
       , evalScaledHessLambdaG
       , evalScaledKKT
         -- * options
       , Op.Opt(..)
         -- * other
       , MonadIO
       , liftIO
       , generateAndCompile
       ) where

import Control.Exception ( AsyncException( UserInterrupt ), try )
import Control.Concurrent ( forkIO, newEmptyMVar, takeMVar, putMVar )
import qualified Control.Applicative as A
import Control.Monad ( when, void )
import "mtl" Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import Data.IORef ( newIORef, readIORef, writeIORef )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy(..) )
import qualified Data.Traversable as T
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Foreign.C.Types ( CInt )

import System.Process ( callProcess, showCommandForUser )
import Text.Printf ( printf )

import Casadi.Core.Enums ( InputOutputScheme(..) )
import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.NlpSolver as C
import qualified Casadi.Core.Classes.GenericType as C
import qualified Casadi.Core.Classes.IOInterfaceFunction as C

import Casadi.Callback ( makeCallback )
import Casadi.CMatrix ( CMatrix )
import qualified Casadi.CMatrix as CM
import Casadi.DMatrix ( DMatrix, dnonzeros )
import Casadi.Function ( Function, evalDMatrix', externalFunction, generateCode )
import Casadi.IOScheme ( mxFunctionWithSchemes )
import Casadi.MX ( MX, symV )
import qualified Casadi.Option as Op
import qualified Casadi.GenericC as Gen

import Dyno.FormatTime ( formatSeconds )
import qualified Dyno.View.M as M
import Dyno.Nlp ( NlpOut(..), KKT(..) )
import Dyno.NlpScaling ( ScaleFuns(..), scaledFG, mkScaleFuns )
import Dyno.SolverInternal ( SolverInternal(..) )
import Dyno.Solvers ( Solver(..), getSolverInternal )
import Dyno.Vectorize ( Id(..) )
import Dyno.View.JV ( JV )
import Dyno.View.View ( View(..), J, fmapJ, d2v, v2d, jfill )
import Dyno.View.M ( M )
import Dyno.View.Unsafe ( unJ, mkJ, mkM )

type VD a = J a (Vector Double)
type VMD a = J a (Vector (Maybe Double))

timeIt :: IO a -> IO (a, Double)
timeIt action = do
  t0 <- getCurrentTime
  ret <- action
  t1 <- getCurrentTime
  return (ret, realToFrac (diffUTCTime t1 t0))

getStat :: String -> NlpSolver x p g C.GenericType
getStat name = do
  nlpState <- ask
  liftIO $ C.function_getStat (isSolver nlpState) name

setInput ::
  View xg
  => (ScaleFuns x g DMatrix -> (J xg DMatrix -> J xg DMatrix))
  -> (NlpState x p g -> Int)
  -> String
  -> J xg (V.Vector Double)
  -> NlpSolver x p g ()
setInput scaleFun getLen name x0 = do
  nlpState <- ask
  let x = unJ $ scaleFun (isScale nlpState) $ mkJ $ CM.fromDVector (unJ x0)
  let nActual = (CM.size1 x, CM.size2 x)
      nTypeLevel = (getLen nlpState, 1)
  when (nTypeLevel /= nActual) $ error $
    name ++ " dimension mismatch, " ++ show nTypeLevel ++
    " (type-level) /= " ++ show nActual ++ " (given)"
  liftIO $ C.ioInterfaceFunction_setInput__0 (isSolver nlpState) x name
  return ()

setX0 :: forall x p g. View x => VD x -> NlpSolver x p g ()
setX0 = setInput xToXBar isNx "x0"

inf :: Double
inf = read "Infinity"

toLb :: View x => J x (Vector (Maybe Double)) -> J x (Vector Double)
toLb = fmapJ (fromMaybe (-inf))

toUb :: View x => J x (Vector (Maybe Double)) -> J x (Vector Double)
toUb = fmapJ (fromMaybe   inf )

setLbx :: View x => VMD x -> NlpSolver x p g ()
setLbx = setInput xToXBar isNx "lbx" . toLb

setUbx :: View x => VMD x -> NlpSolver x p g ()
setUbx = setInput xToXBar isNx "ubx" . toUb

setLbg :: View g => VMD g -> NlpSolver x p g ()
setLbg = setInput gToGBar isNg "lbg" . toLb

setUbg :: View g => VMD g -> NlpSolver x p g ()
setUbg = setInput gToGBar isNg "ubg" . toUb

setP :: View p => VD p -> NlpSolver x p g ()
setP p = do
  nlpState <- ask
  isSetParam nlpState p
  setInput (const id) isNp "p" p

setLamX0 :: View x => VD x -> NlpSolver x p g ()
setLamX0 = setInput lamXToLamXBar isNx "lam_x0"

setLamG0 :: View g => VD g -> NlpSolver x p g ()
setLamG0 = setInput lamGToLamGBar isNg "lam_g0"

getInput ::
  View xg
  => (ScaleFuns x g DMatrix -> (J xg DMatrix -> J xg DMatrix))
  -> String -> NlpSolver x p g (J xg (Vector Double))
getInput scaleFun name = do
  nlpState <- ask
  dmat <- liftIO $ C.ioInterfaceFunction_getInput__0 (isSolver nlpState) name
  let scale = scaleFun (isScale nlpState)
  return (mkJ $ dnonzeros $ unJ $ scale (mkJ dmat))

getX0 :: View x => NlpSolver x p g (VD x)
getX0 = getInput xbarToX "x0"

getLbx :: View x => NlpSolver x p g (VD x)
getLbx = getInput xbarToX "lbx"

getUbx :: View x => NlpSolver x p g (VD x)
getUbx = getInput xbarToX "ubx"

getLbg :: View g => NlpSolver x p g (VD g)
getLbg = getInput gbarToG "lbg"

getUbg :: View g => NlpSolver x p g (VD g)
getUbg = getInput gbarToG "ubg"

getP :: View p => NlpSolver x p g (VD p)
getP = getInput (const id) "p"

getLamX0 :: View x => NlpSolver x p g (VD x)
getLamX0 = getInput lamXBarToLamX "lam_x0"

getLamG0 :: View g => NlpSolver x p g (VD g)
getLamG0 = getInput lamGBarToLamG "lam_g0"

getOutput ::
  View xg
  => (ScaleFuns x g DMatrix -> (J xg DMatrix -> J xg DMatrix))
  -> String -> NlpSolver x p g (J xg (Vector Double))
getOutput scaleFun name = do
  nlpState <- ask
  dmat <- liftIO $ C.ioInterfaceFunction_getOutput__0 (isSolver nlpState) name
  let scale = scaleFun (isScale nlpState)
  return (mkJ $ dnonzeros $ unJ $ scale (mkJ dmat))

getF :: NlpSolver x p g (VD (JV Id))
getF = getOutput fbarToF "f"

getX :: View x => NlpSolver x p g (VD x)
getX = getOutput xbarToX "x"

getG :: View g => NlpSolver x p g (VD g)
getG = getOutput gbarToG "g"

getLamX :: View x => NlpSolver x p g (VD x)
getLamX = getOutput lamXBarToLamX "lam_x"

getLamG :: View g => NlpSolver x p g (VD g)
getLamG = getOutput lamGBarToLamG "lam_g"


evalScaledGradF :: forall x p g . (View x, View g, View p)
                   => NlpSolver x p g (J x DMatrix, J (JV Id) DMatrix)
evalScaledGradF = do
  x0bar <- getInput (const id) "x0" :: NlpSolver x p g (J x (Vector Double))
  pbar <- getInput (const id) "p" :: NlpSolver x p g (J p (Vector Double))

  nlpState <- ask
  let solver = isSolver nlpState :: C.NlpSolver
  liftIO $ do
    gradF <- C.nlpSolver_gradF solver
    result <- evalDMatrix' gradF (M.fromList [("x", unJ (v2d x0bar)), ("p", unJ (v2d pbar))])
    let mret = do
          grad <- M.lookup "grad" result
          f <- M.lookup "f" result
          return (mkJ grad, mkJ f)
    case mret of
      Nothing -> error $ "evalScaledGradF: error looking up output\n"
                 ++ "fields available: " ++ show (M.keys result)
      Just r -> return r

evalGradF :: forall x p g . (View x, View g, View p)
             => NlpSolver x p g (J x DMatrix, J (JV Id) DMatrix)
evalGradF = do
  nlpState <- ask
  let scale = isScale nlpState
  (gradF, f) <- evalScaledGradF
  return (gradFBarToGradF scale gradF, fbarToF scale f)

evalScaledJacG :: forall x p g . (View x, View g, View p)
                  => NlpSolver x p g (M g x DMatrix, J g DMatrix)
evalScaledJacG = do
  x0bar <- getInput (const id) "x0" :: NlpSolver x p g (J x (Vector Double))
  pbar <- getInput (const id) "p" :: NlpSolver x p g (J p (Vector Double))

  nlpState <- ask
  let solver = isSolver nlpState :: C.NlpSolver
  -- todo: remove this workaround when casadi fixes https://github.com/casadi/casadi/issues/1345
  if size (Proxy :: Proxy g) == 0
    then return (M.zeros, M.zeros)
    else liftIO $ do
    jacG <- C.nlpSolver_jacG solver
    result <- evalDMatrix' jacG (M.fromList [("x", unJ (v2d x0bar)), ("p", unJ (v2d pbar))])
    let mret = do
          jac <- M.lookup "jac" result
          g <- M.lookup "g" result
          return (mkM jac, mkJ g)
    case mret of
      Nothing -> error $ "evalScaledJacG: error looking up output\n"
                 ++"fields available: " ++ show (M.keys result)
      Just r -> return r

evalJacG :: forall x p g . (View x, View g, View p)
            => NlpSolver x p g (M g x DMatrix, J g DMatrix)
evalJacG = do
  (jacG, g) <- evalScaledJacG

  nlpState <- ask
  let scale = isScale nlpState
  return (jacGBarToJacG scale jacG, gbarToG scale g)

evalScaledHessLag :: forall x p g . (View x, View g, View p)
                     => NlpSolver x p g (M x x DMatrix)
evalScaledHessLag = do
  x0bar <- getInput (const id) "x0" :: NlpSolver x p g (J x (Vector Double))
  pbar <- getInput (const id) "p" :: NlpSolver x p g (J p (Vector Double))
  lamGbar <- getInput (const id) "lam_g0" :: NlpSolver x p g (J g (Vector Double))

  nlpState <- ask
  let solver = isSolver nlpState :: C.NlpSolver
  liftIO $ do
    hessLag <- C.nlpSolver_hessLag solver
    result <- evalDMatrix' hessLag $
              M.fromList
              [ ("der_x", unJ (v2d x0bar))
              , ("der_p", unJ (v2d pbar))
              , ("adj0_f", 1.0)
              , ("adj0_g", unJ (v2d lamGbar))
              ]
    case M.lookup "jac" result of -- ????????????????
      Nothing -> error $ "evalScaledHessLag: error looking up hess lag output\n"
                 ++ "available fields are: " ++ show (M.keys result)
      Just r -> return (mkM r)

-- | only valid at the solution
evalHessLag :: forall x p g . (View x, View g, View p)
                   => NlpSolver x p g (M x x DMatrix)
evalHessLag = do
  hess <- evalScaledHessLambdaG
  nlpState <- ask
  let scale = isScale nlpState
  return (hessLagBarToHessLag scale hess)


evalScaledHessF :: forall x p g . (View x, View g, View p)
                   => NlpSolver x p g (M x x DMatrix)
evalScaledHessF = do
  x0bar <- getInput (const id) "x0" :: NlpSolver x p g (J x (Vector Double))
  pbar <- getInput (const id) "p" :: NlpSolver x p g (J p (Vector Double))
  let lamGbar = jfill 0 :: J g (Vector Double)
  nlpState <- ask
  let solver = isSolver nlpState :: C.NlpSolver
  liftIO $ do
    hessLag <- C.nlpSolver_hessLag solver
    result <- evalDMatrix' hessLag $
              M.fromList
              [ ("der_x", unJ (v2d x0bar))
              , ("der_p", unJ (v2d pbar))
              , ("adj0_f", 1.0)
              , ("adj0_g", unJ (v2d lamGbar))
              ]
    case M.lookup "jac" result of -- ????????????????
      Nothing -> error $ "evalScaledHessF: error looking up hess lag output\n"
                 ++ "available fields are: " ++ show (M.keys result)
      Just r -> return (mkM r)

evalHessF :: forall x p g . (View x, View g, View p)
             => NlpSolver x p g (M x x DMatrix)
evalHessF = do
  hess <- evalScaledHessLag
  nlpState <- ask
  let scale = isScale nlpState
  return (hessFBarToHessF scale hess)


evalScaledHessLambdaG :: forall x p g . (View x, View g, View p)
                         => NlpSolver x p g (M x x DMatrix)
evalScaledHessLambdaG = do
  x0bar <- getInput (const id) "x0" :: NlpSolver x p g (J x (Vector Double))
  pbar <- getInput (const id) "p" :: NlpSolver x p g (J p (Vector Double))
  lamGbar <- getInput (const id) "lam_g0" :: NlpSolver x p g (J g (Vector Double))
  nlpState <- ask
  let solver = isSolver nlpState :: C.NlpSolver
  liftIO $ do
    hessLag <- C.nlpSolver_hessLag solver
    result <- evalDMatrix' hessLag $
              M.fromList
              [ ("der_x", unJ (v2d x0bar))
              , ("der_p", unJ (v2d pbar))
              , ("adj0_f", 0.0)
              , ("adj0_g", unJ (v2d lamGbar))
              ]
    case M.lookup "jac" result of -- ????????????????
      Nothing -> error $ "evalScaledHessLambdaG: error looking up hess lag output\n"
                 ++ "available fields are: " ++ show (M.keys result)
      Just r -> return (mkM r)


-- | only valid at solution
evalHessLambdaG :: forall x p g . (View x, View g, View p)
                   => NlpSolver x p g (M x x DMatrix)
evalHessLambdaG = do
  hess <- evalScaledHessLambdaG
  nlpState <- ask
  let scale = isScale nlpState
  return (hessLamGBarToHessLamG scale hess)



evalKKT :: (View x, View p, View g) => NlpSolver x p g (KKT x g)
evalKKT = do
  (gradF,f) <- evalGradF
  (jacG, g) <- evalJacG
  hessF <- evalHessF
  hessLambdaG <- evalHessLambdaG
  hessLag <- evalHessLag
  return $
    KKT
    { kktF = f
    , kktJacG = jacG
    , kktG = g
    , kktGradF = gradF
    , kktHessLag = hessLag
    , kktHessF = hessF
    , kktHessLambdaG = hessLambdaG
    }


evalScaledKKT :: (View x, View p, View g) => NlpSolver x p g (KKT x g)
evalScaledKKT = do
  (gradF,f) <- evalScaledGradF
  (jacG, g) <- evalScaledJacG
  hessL <- evalScaledHessLag
  hessF <- evalScaledHessF
  hessLambdaG <- evalScaledHessLambdaG
  return $
    KKT
    { kktF = f
    , kktJacG = jacG
    , kktG = g
    , kktGradF = gradF
    , kktHessLag = hessL
    , kktHessF = hessF
    , kktHessLambdaG = hessLambdaG
    }


-- | solve with current inputs, return success or failure code
solve :: NlpSolver x p g (Either String String)
solve = do
  nlpState <- ask
  let nlp = isSolver nlpState
  solveStatus <- liftIO $ do

    stop <- newEmptyMVar -- mvar that will be filled when nlp finishes
    _ <- forkIO (C.function_evaluate nlp >> putMVar stop ())
    -- wait until nlp finishes
    ret <- try (takeMVar stop)
    case ret of Right () -> return () -- no exceptions
                Left UserInterrupt -> do -- got ctrl-C
                  isInterrupt nlpState -- tell nlp to stop iterations
                  _ <- takeMVar stop -- wait for nlp to return
                  return ()
                Left _ -> void (takeMVar stop) -- don't handle this one
    genericStat <- C.function_getStat nlp "return_status"
    strStat <- Gen.fromGeneric genericStat :: IO (Maybe String)
    intStat <- Gen.fromGeneric genericStat :: IO (Maybe Int)
    statDescription <- Gen.getDescription genericStat
    case strStat of
      Just strStat' -> return strStat'
      Nothing -> case intStat of
        Just intStat' -> return (show intStat')
        Nothing -> error $ "nlp solver error: return status is not {string,int}, it's " ++
                   statDescription

  return $ if solveStatus `elem` isSuccessCodes nlpState
    then Right solveStatus
    else Left solveStatus

-- | solve with current inputs, return lots of info on success, or message on failure
solve' :: (View x, View g) => NlpSolver x p g (Either String String, NlpOut x g (Vector Double))
solve' = do
  solveStatus <- solve
  nlpOut <- getNlpOut
  return (solveStatus, nlpOut)

getNlpOut :: (View x, View g) => NlpSolver x p g (NlpOut x g (Vector Double))
getNlpOut = do
  fopt <- getF
  xopt <- getX
  gopt <- getG
  lamXOpt <- getLamX
  lamGOpt <- getLamG
  let nlpOut = NlpOut { fOpt = fopt
                      , xOpt = xopt
                      , gOpt = gopt
                      , lambdaXOpt = lamXOpt
                      , lambdaGOpt = lamGOpt
                      }
  return nlpOut


data NlpState (x :: * -> *) (p :: * -> *) (g :: * -> *) =
  NlpState
  { isNx :: Int
  , isNg :: Int
  , isNp :: Int
  , isSolver :: C.NlpSolver
  , isInterrupt :: IO ()
  , isSuccessCodes :: [String]
  , isScale :: ScaleFuns x g DMatrix
  , isSetParam :: J p (Vector Double) -> NlpSolver x p g ()
  }
newtype NlpSolver (x :: * -> *) (p :: * -> *) (g :: * -> *) a =
  NlpSolver (ReaderT (NlpState x p g) IO a)
  deriving ( Functor
           , A.Applicative
           , Monad
           , MonadReader (NlpState x p g)
           , MonadIO
           )

generateAndCompile :: String -> Function -> IO Function
generateAndCompile name f = do
  putStrLn $ "generating " ++ name ++ ".c"
  let opts = M.fromList [("generate_main", Op.Opt True)]
  writeFile (name ++ ".c") (generateCode f opts)
  let cmd = "clang"
      args = ["-fPIC","-shared","-Wall","-Wno-unused-variable",name++".c","-o",name++".so"]
  putStrLn (showCommandForUser cmd args)
  callProcess cmd args
  externalFunction ("./"++name++".so") M.empty

data RunNlpOptions =
  RunNlpOptions
  { verbose :: Bool
  }

defaultRunnerOptions :: RunNlpOptions
defaultRunnerOptions =
  RunNlpOptions
  { verbose = False
  }

runNlpSolver ::
  forall x p g a .
  (View x, View p, View g)
  => Solver
  -> (J x MX -> J p MX -> (J (JV Id) MX, J g MX))
  -> Maybe (J x (Vector Double))
  -> Maybe (J g (Vector Double))
  -> Maybe Double
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> NlpSolver x p g a
  -> IO a
runNlpSolver = runNlpSolverWith defaultRunnerOptions

runNlpSolverWith ::
  forall x p g a .
  (View x, View p, View g)
  => RunNlpOptions
  -> Solver
  -> (J x MX -> J p MX -> (J (JV Id) MX, J g MX))
  -> Maybe (J x (Vector Double))
  -> Maybe (J g (Vector Double))
  -> Maybe Double
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> NlpSolver x p g a
  -> IO a
runNlpSolverWith runnerOptions solverStuff nlpFun scaleX scaleG scaleF callback' (NlpSolver nlpMonad) = do
  inputsX <- mkJ <$> symV "x" (size (Proxy :: Proxy x))
  inputsP <- mkJ <$> symV "p" (size (Proxy :: Proxy p))

  let scale :: forall sfa . CMatrix sfa => ScaleFuns x g sfa
      scale = mkScaleFuns scaleX scaleG scaleF

      (obj, g) = scaledFG scale nlpFun inputsX inputsP

      inputsXMat = unJ inputsX
      inputsPMat = unJ inputsP
      objMat     = unJ obj
      gMat       = unJ g

  when (verbose runnerOptions) $ do
    putStrLn "************** initializing dynobud runNlpSolver ******************"
    putStrLn "making nlp..."

  (nlp, nlpTime) <- timeIt $ mxFunctionWithSchemes "nlp"
    (SCHEME_NLPInput, M.fromList [("x", inputsXMat), ("p", inputsPMat)])
    (SCHEME_NLPOutput, M.fromList [("f", objMat), ("g", gMat)])
    (M.fromList (functionOptions solverStuff))
  when (verbose runnerOptions) $ printf "nlp initialized in %s\n"
    (formatSeconds nlpTime)

  when (verbose runnerOptions) $ putStrLn "function call..."
  -- in case the user wants to do something (like codegen?)
  (_, functionCallTime) <- timeIt $ functionCall solverStuff nlp
  when (verbose runnerOptions) $ printf "function called in %s\n" (formatSeconds functionCallTime)

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

  -- add callback if user provides it
  when (verbose runnerOptions) $ putStrLn "create callback..."
  intref <- newIORef False
  paramRef <- newIORef (jfill 0)
  let cb :: Function -> IO CInt
      cb function' = do
        callbackRet <- case callback' of
          Nothing -> return True
          Just callback -> do
            xval <- fmap (d2v . xbarToX scale . mkJ . CM.densify) $
                    C.ioInterfaceFunction_getOutput__2 function' 0
            pval <- readIORef paramRef
            callback xval pval
        interrupt <- readIORef intref
        return $ if callbackRet && not interrupt then 0
                 else fromIntegral (solverInterruptCode (getSolverInternal solverStuff))
  casadiCallback <- makeCallback cb >>= C.genericType__1

  -- make the solver
  solverOptions <-
    T.mapM Gen.mkGeneric $
    M.fromList $
    ("iteration_callback", Op.Opt casadiCallback)
    : defaultSolverOptions (getSolverInternal solverStuff)
    ++ options solverStuff
  when (verbose runnerOptions) $ putStrLn "create solver..."
  (solver, solverInitTime) <-
    timeIt $ C.nlpSolver__5 "nlpSolver"
    (solverName (getSolverInternal solverStuff)) (C.castFunction nlp)
    solverOptions
  when (verbose runnerOptions) $
    printf "solver initialized in %s\n" (formatSeconds solverInitTime)


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


  let proxy :: J f b -> Proxy f
      proxy = const Proxy

      nlpState = NlpState { isNx = size (proxy inputsX)
                          , isNp = size (proxy inputsP)
                          , isNg = size (proxy g)
                          , isSolver = solver
                          , isInterrupt = writeIORef intref True
                          , isSuccessCodes = successCodes (getSolverInternal solverStuff)
                          , isScale = scale
                          , isSetParam = liftIO . writeIORef paramRef
                          }
  when (verbose runnerOptions) $ putStrLn "run NLP monad..."
  (ret, retTime) <- timeIt $ liftIO $ runReaderT nlpMonad nlpState
  when (verbose runnerOptions) $ printf "ran NLP monad in %s\n" (formatSeconds retTime)
  return ret
