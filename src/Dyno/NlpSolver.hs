{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PackageImports #-}
{-# Language KindSignatures #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiWayIf #-}

module Dyno.NlpSolver
       ( NlpSolver
       , SXElement
       , runNlpSolver
       , runNlp
         -- * solve
       , solveNlp
       , solveNlp'
       , solveNlpHomotopy'
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
       , NlpSolverStuff(..)
         -- * options
       , Op.Opt(..)
       , setOption
       , reinit
       , MonadIO
       , liftIO
       , generateAndCompile
       ) where

import System.Process ( callProcess, showCommandForUser )
import Control.Exception ( AsyncException( UserInterrupt ), try )
import Control.Concurrent ( forkIO, newEmptyMVar, takeMVar, putMVar )
import Control.Applicative ( Applicative(..) )
import Control.Monad ( when, void )
import "mtl" Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import Data.Maybe ( fromMaybe )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO ( hFlush, stdout )
import Text.Printf ( printf )

import Casadi.Core.Enums ( InputOutputScheme(..) )
import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.NlpSolver as C
import qualified Casadi.Core.Classes.GenericType as C
import qualified Casadi.Core.Classes.IOInterfaceFunction as C

import Casadi.Callback ( makeCallback )
import Casadi.DMatrix
import Casadi.SX
import Casadi.Function ( Function, externalFunction )
import qualified Casadi.Option as Op
import qualified Casadi.GenericC as Gen
import Casadi.SharedObject ( soInit )

import Dyno.SXElement ( SXElement, sxElementToSX )
import Dyno.Vectorize ( Vectorize(..), Id )
import Dyno.View.JV

import Dyno.View.View
import Dyno.View.Symbolic
import Dyno.View.Viewable ( Viewable )
import Dyno.View.CasadiMat ( CasadiMat )
import qualified Dyno.View.CasadiMat as CM
import Dyno.Nlp ( Nlp(..), NlpOut(..), Nlp'(..), NlpOut'(..), Bounds )
import Dyno.NlpScaling ( ScaleFuns(..), scaledFG, mkScaleFuns )
import Data.Proxy

type VD a = J a (Vector Double)
type VMD a = J a (Vector (Maybe Double))

data NlpSolverStuff =
  NlpSolverStuff
  { solverName :: String
  , defaultOptions :: [(String,Op.Opt)]
  , options :: [(String,Op.Opt)]
  , solverInterruptCode :: Int
  , successCodes :: [String]
  , functionOptions :: [(String, Op.Opt)]
  , functionCall :: C.Function -> IO ()
  }

getStat :: String -> NlpSolver x p g C.GenericType
getStat name = do
  nlpState <- ask
  liftIO $ C.function_getStat (isSolver nlpState) name

setInput ::
  View xg
  => (ScaleFuns x g DMatrix -> (J xg DMatrix -> J xg DMatrix))
  -> (NlpState x g -> Int)
  -> String
  -> J xg (V.Vector Double)
  -> NlpSolver x p g ()
setInput scaleFun getLen name x0 = do
  nlpState <- ask
  let x = unJ $ scaleFun (isScale nlpState) $ mkJ $ CM.fromDVector (unJ x0)
  let nActual = (dsize1 x, dsize2 x)
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
toLb = mkJ . V.map (fromMaybe (-inf)) . unJ

toUb :: View x => J x (Vector (Maybe Double)) -> J x (Vector Double)
toUb = mkJ . V.map (fromMaybe   inf ) . unJ

setLbx :: View x => VMD x -> NlpSolver x p g ()
setLbx = setInput xToXBar isNx "lbx" . toLb

setUbx :: View x => VMD x -> NlpSolver x p g ()
setUbx = setInput xToXBar isNx "ubx" . toUb

setLbg :: View g => VMD g -> NlpSolver x p g ()
setLbg = setInput gToGBar isNg "lbg" . toLb

setUbg :: View g => VMD g -> NlpSolver x p g ()
setUbg = setInput gToGBar isNg "ubg" . toUb

setP :: View p => VD p -> NlpSolver x p g ()
setP = setInput (const id) isNp "p"

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
  dmat <- liftIO $ C.ioInterfaceFunction_input__0 (isSolver nlpState) name
  let scale = scaleFun (isScale nlpState)
  return (mkJ $ ddata $ unJ $ scale (mkJ dmat))

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
  dmat <- liftIO $ C.ioInterfaceFunction_output__0 (isSolver nlpState) name
  let scale = scaleFun (isScale nlpState)
  return (mkJ $ ddata $ unJ $ scale (mkJ dmat))

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


setOption :: Gen.GenericC a => String -> a -> NlpSolver x p g ()
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
solve' :: (View x, View g) => NlpSolver x p g (Either String String, NlpOut' x g (Vector Double))
solve' = do
  solveStatus <- solve
  nlpOut <- getNlpOut'
  return (solveStatus, nlpOut)

getNlpOut' :: (View x, View g) => NlpSolver x p g (NlpOut' x g (Vector Double))
getNlpOut' = do
  fopt <- getF
  xopt <- getX
  gopt <- getG
  lamXOpt <- getLamX
  lamGOpt <- getLamG
  let nlpOut = NlpOut' { fOpt' = fopt
                       , xOpt' = xopt
                       , gOpt' = gopt
                       , lambdaXOpt' = lamXOpt
                       , lambdaGOpt' = lamGOpt
                       }
  return nlpOut


data NlpState (x :: * -> *) (g :: * -> *) =
  NlpState
  { isNx :: Int
  , isNg :: Int
  , isNp :: Int
  , isSolver :: C.NlpSolver
  , isInterrupt :: IO ()
  , isSuccessCodes :: [String]
  , isScale :: ScaleFuns x g DMatrix
  }
newtype NlpSolver (x :: * -> *) (p :: * -> *) (g :: * -> *) a =
  NlpSolver (ReaderT (NlpState x g) IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (NlpState x g)
           , MonadIO
           )

generateAndCompile :: String -> Function -> IO Function
generateAndCompile name f = do
  putStrLn $ "generating " ++ name ++ ".c"
--  writeFile (name ++ ".c") (generateCode f)
  C.function_generateCode__3 f (name ++ ".c") True
  let cmd = "clang"
      args = ["-fPIC","-shared","-Wall","-Wno-unused-variable",name++".c","-o",name++".so"]
  putStrLn (showCommandForUser cmd args)
  callProcess cmd args
  externalFunction ("./"++name++".so")

runNlpSolver ::
  forall x p g a s .
  (View x, View p, View g, Symbolic s)
  => NlpSolverStuff
  -> (J x s -> J p s -> (J (JV Id) s, J g s))
  -> Maybe (J x (Vector Double))
  -> Maybe (J g (Vector Double))
  -> Maybe Double
  -> Maybe (J x (Vector Double) -> IO Bool)
  -> NlpSolver x p g a
  -> IO a
runNlpSolver solverStuff nlpFun scaleX scaleG scaleF callback' (NlpSolver nlpMonad) = do
  inputsX <- sym "x"
  inputsP <- sym "p"

  let scale :: forall sfa . (CasadiMat sfa, Viewable sfa) => ScaleFuns x g sfa
      scale = mkScaleFuns scaleX scaleG scaleF

  let (obj, g) = scaledFG scale nlpFun inputsX inputsP

  let inputsXMat = unJ inputsX
      inputsPMat = unJ inputsP
      objMat     = unJ obj
      gMat       = unJ g

  inputScheme <- mkScheme SCHEME_NLPInput [("x", inputsXMat), ("p", inputsPMat)]
  outputScheme <- mkScheme SCHEME_NLPOutput [("f", objMat), ("g", gMat)]
  nlp <- mkFunction "nlp" inputScheme outputScheme
--  Op.setOption nlp "verbose" True
  mapM_ (\(l,Op.Opt o) -> Op.setOption nlp l o) (functionOptions solverStuff)
  soInit nlp

  functionCall solverStuff nlp

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


  solver <- C.nlpSolver__0 (solverName solverStuff) nlp

  -- add callback if user provides it
  intref <- newIORef False
  let cb function' = do
        callbackRet <- case callback' of
          Nothing -> return True
          Just callback -> do
            xval <- fmap (mkJ . ddata . unJ . xbarToX scale . mkJ . ddense) $
                    C.ioInterfaceFunction_output__2 function' 0
            callback xval
        interrupt <- readIORef intref
        return $ if callbackRet && not interrupt then 0 else fromIntegral (solverInterruptCode solverStuff)
  casadiCallback <- makeCallback cb >>= C.genericType__0
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
  soInit solver

  let nlpState = NlpState { isNx = size (proxy inputsX)
                          , isNp = size (proxy inputsP)
                          , isNg = size (proxy g)
                          , isSolver = solver
                          , isInterrupt = writeIORef intref True
                          , isSuccessCodes = successCodes solverStuff
                          , isScale = scale
                          }
  liftIO $ runReaderT nlpMonad nlpState
proxy :: J a b -> Proxy a
proxy = const Proxy

-- | convenience function to solve a pure Nlp
solveNlp :: forall x p g .
  (Vectorize x, Vectorize p, Vectorize g)
  => NlpSolverStuff
  -> Nlp x p g SXElement -> Maybe (x Double -> IO Bool)
  -> IO (Either String String, NlpOut x g Double)
solveNlp solverStuff nlp callback = do
  let nlp' :: Nlp' (JV x) (JV p) (JV g) SX
      nlp' = Nlp' { nlpFG' = \x' p' -> let x = sxSplitJV x' :: x SXElement
                                           p = sxSplitJV p' :: p SXElement
                                           (obj,g) = nlpFG nlp x p :: (SXElement, g SXElement)
                                           obj' = mkJ (sxElementToSX obj) :: J (JV Id) SX
                                           g' = sxCatJV g :: J (JV g) SX
                                       in (obj',g')
                  , nlpBX' = mkJ $ vectorize (nlpBX nlp) :: J (JV x) (V.Vector Bounds)
                  , nlpBG' = mkJ $ vectorize (nlpBG nlp) :: J (JV g) (V.Vector Bounds)
                  , nlpX0' = mkJ $ vectorize (nlpX0 nlp) :: J (JV x) (V.Vector Double)
                  , nlpP'  = mkJ $ vectorize (nlpP  nlp) :: J (JV p) (V.Vector Double)
                  , nlpLamX0' = fmap (mkJ . vectorize) (nlpLamX0 nlp)
                                :: Maybe (J (JV x) (V.Vector Double))
                  , nlpLamG0' = fmap (mkJ . vectorize) (nlpLamG0 nlp)
                                :: Maybe (J (JV g) (V.Vector Double))
                  , nlpScaleF' = nlpScaleF nlp
                  , nlpScaleX' = fmap (mkJ . vectorize) (nlpScaleX nlp)
                                :: Maybe (J (JV x) (V.Vector Double))
                  , nlpScaleG' = fmap (mkJ . vectorize) (nlpScaleG nlp)
                                :: Maybe (J (JV g) (V.Vector Double))
                  }

      callback' :: Maybe (J (JV x) (Vector Double) -> IO Bool)
      callback' = fmap (. devectorize . unJ) callback

  (r0, r1') <- solveNlp' solverStuff nlp' callback'

  let r1 :: NlpOut x g Double
      r1 = NlpOut { fOpt = V.head $ unJ (fOpt' r1')
                  , xOpt = devectorize $ unJ (xOpt' r1')
                  , gOpt = devectorize $ unJ (gOpt' r1')
                  , lambdaXOpt = devectorize $ unJ $ lambdaXOpt' r1'
                  , lambdaGOpt = devectorize $ unJ $ lambdaGOpt' r1'
                  }

  return (r0, r1)


fmapJ :: View x => (a -> b) -> J x (Vector a) -> J x (Vector b)
fmapJ f (UnsafeJ v) = mkJ (V.map f v)

junzip :: View x => J x (Vector (a,b)) -> (J x (Vector a), J x (Vector b))
junzip (UnsafeJ v) = (mkJ x, mkJ y)
  where
    (x,y) = V.unzip v

-- | convenience function to solve a pure Nlp'
solveNlp' ::
  (View x, View p, View g, Symbolic a)
  => NlpSolverStuff
  -> Nlp' x p g a -> Maybe (J x (Vector Double) -> IO Bool)
  -> IO (Either String String, NlpOut' x g (Vector Double))
solveNlp' solverStuff nlp callback =
  runNlp solverStuff nlp callback solve'


-- | set all inputs, handle scaling, and let the user run a NlpMonad
runNlp ::
  (View x, View p, View g, Symbolic a)
  => NlpSolverStuff
  -> Nlp' x p g a -> Maybe (J x (Vector Double) -> IO Bool)
  -> NlpSolver x p g b
  -> IO b
runNlp solverStuff nlp callback runMe =
  runNlpSolver solverStuff (nlpFG' nlp) (nlpScaleX' nlp) (nlpScaleG' nlp) (nlpScaleF' nlp) callback $ do
    let (lbx,ubx) = junzip (nlpBX' nlp)
        (lbg,ubg) = junzip (nlpBG' nlp)

    setX0 (nlpX0' nlp)
    setP (nlpP' nlp)
    setLbx lbx
    setUbx ubx
    setLbg lbg
    setUbg ubg
    case nlpLamX0' nlp of
      Just lam -> setLamX0 lam
      Nothing -> return ()
    case nlpLamG0' nlp of
      Just lam -> setLamG0 lam
      Nothing -> return ()
    runMe

-- | solve a homotopy nlp
solveNlpHomotopy' ::
  forall x p g a .
  (View x, View p, View g, Symbolic a)
  => Double -> (Double, Double, Int, Int)
  -> NlpSolverStuff
  -> Nlp' x p g a -> J p (Vector Double) -> Maybe (J (JTuple x p) (Vector Double) -> IO Bool)
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> Double -> IO ())
  -> IO (Either String String, NlpOut' (JTuple x p) g (Vector Double))
solveNlpHomotopy' userStep (reduction, increase, iterIncrease, iterDecrease)
  solverStuff nlp (UnsafeJ pF) callback callbackP = do
  when (reduction >= 1) $ error $ "homotopy reduction factor " ++ show reduction ++ " >= 1"
  when (increase  <= 1) $ error $ "homotopy increase factor "  ++ show increase  ++ " <= 1"
  let fg :: J (JTuple x p) a -> J JNone a -> (J (JV Id) a, J g a)
      fg xp _ = nlpFG' nlp x p
        where
          JTuple x p = split xp
  runNlpSolver solverStuff fg Nothing (nlpScaleG' nlp) (nlpScaleF' nlp) callback $ do
    let (lbx,ubx) = junzip (nlpBX' nlp)
        (lbg,ubg) = junzip (nlpBG' nlp)
        UnsafeJ p0 = nlpP' nlp

        setAlpha :: Double -> NlpSolver (JTuple x p) JNone g ()
        setAlpha alpha = do
          let p = mkJ $ V.zipWith (+) p0 (V.map (alpha*) (V.zipWith (-) pF p0))
          setLbx $ cat (JTuple lbx (fmapJ Just p))
          setUbx $ cat (JTuple ubx (fmapJ Just p))

    -- initial solve
    setX0 $ cat $ JTuple (nlpX0' nlp) (nlpP' nlp)
    setP $ cat JNone
    setAlpha 0
    setLbg lbg
    setUbg ubg
    case nlpLamX0' nlp of
      Just lam -> setLamX0 $ cat (JTuple lam (jfill 0))
      Nothing -> return ()
    case nlpLamG0' nlp of
      Just lam -> setLamG0 lam
      Nothing -> return ()
    (ret0, _) <- solve'
    case ret0 of
      Right _ -> return ()
      Left msg -> error $ "error: homotopy solver initial guess not good enough\n" ++ msg
    getX >>= setX0
    getLamX >>= setLamX0
    getLamG >>= setLamG0

    -- run the homotopy
    let runCallback alphaTrial = case callbackP of
          Nothing -> return ()
          Just cbp -> do
            xp <- getX
            let JTuple x p = split xp
            liftIO $ void (cbp x p alphaTrial)

        tryStep :: Int -> Double -> Double
                   -> NlpSolver (JTuple x p) JNone g
                      (Either String String, NlpOut' (JTuple x p) g (Vector Double))
        tryStep majorIter alpha0 step
          | step < 1e-12 = do no <- getNlpOut'
                              return (Left "step size too small", no)
          | otherwise = do
            liftIO $ printf "%4d, alpha: %.2e, step: %.2e " majorIter alpha0 step
            liftIO $ hFlush stdout
            let (alphaTrial, alphaIsOne)
                  | alpha0 + step >= 1 = (1, True)
                  | otherwise = (alpha0 + step, False)
            setAlpha alphaTrial
            ret <- solve'
            case ret of
              (Left msg,_) -> do
                liftIO $ putStrLn $ "step failed to solve: " ++ msg
                tryStep (majorIter+1) alpha0 (reduction*step)
              (Right _,_) -> do
                itersStat <- getStat "iter_count"
                mk <- liftIO (Gen.fromGeneric itersStat :: IO (Maybe Int))
                iters <- case mk of
                  Nothing ->
                    liftIO (Gen.getDescription itersStat) >>=
                    error . ("homotopy solver: iters is not an Int, it is: " ++) . show
                  Just k' -> return k'
                liftIO $ putStrLn $ "step successful (" ++ show iters ++ " iterations)"
                runCallback alphaTrial
                if alphaIsOne
                  then return ret
                  else do getX >>= setX0
                          getLamX >>= setLamX0
                          getLamG >>= setLamG0
                          if | iters < iterIncrease -> tryStep (majorIter + 1) alphaTrial (step*increase)
                             | iters < iterDecrease -> tryStep (majorIter + 1) alphaTrial step
                             | otherwise            -> tryStep (majorIter + 1) alphaTrial (step*reduction)

    ret <- tryStep 0 0 userStep
    liftIO $ putStrLn "homotopy successful"
    return ret
