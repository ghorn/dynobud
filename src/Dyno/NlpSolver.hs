{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.NlpSolver
       ( NlpSol
       , callNlpsol
       , toNlpSol
         -- * options
       , GType(..)
       ) where

import Control.Exception ( AsyncException( UserInterrupt ), SomeException, try )
import Control.Concurrent ( MVar, forkIO, newEmptyMVar, takeMVar, putMVar )
import Control.Monad ( when, void )
import Data.IORef ( newIORef, readIORef, writeIORef )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Proxy ( Proxy(..) )
import qualified Data.Traversable as T
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import System.IO ( stdout, hFlush )
import Text.Printf ( printf )

import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Tools as C

import Casadi.Callback ( makeCallback )
import Casadi.Matrix ( CMatrix )
import qualified Casadi.Matrix as CM
import Casadi.DM ( DM )
import Casadi.Function ( Function, callDM' )
import Casadi.GenericType ( GType(..), fromGType, toGType )
import Casadi.MX ( MX )
import Casadi.Sparsity ( Sparsity, dense, scalar )

import Dyno.FormatTime ( formatSeconds )
import Dyno.Nlp ( NlpOut(..), NlpIn(..), Bounds )
import Dyno.NlpScaling ( ScaleFuns(..), scaledFG, mkScaleFuns )
import Dyno.SolverInternal ( SolverInternal(..) )
import Dyno.Solvers ( Solver(..), RunNlpOptions(..), getSolverInternal )
import Dyno.View.View ( View(..), J, S, d2v, fmapJ, jfill, v2d )
import Dyno.View.Unsafe ( mkM, unM )

timeIt :: IO a -> IO (a, Double)
timeIt action = do
  t0 <- getCurrentTime
  ret <- action
  t1 <- getCurrentTime
  return (ret, realToFrac (diffUTCTime t1 t0))

fromNlpSolIn :: (View x, View p, View g) => NlpSol x p g -> NlpIn x p g -> M.Map String DM
fromNlpSolIn nlpSol nsi =
  M.fromList $
  [ toInput xToXBar isNx "x0" nlpSol (nlpX0 nsi)
  , toInput xToXBar isNx "lbx" nlpSol (toLb (nlpBX nsi))
  , toInput xToXBar isNx "ubx" nlpSol (toUb (nlpBX nsi))
  , toInput gToGBar isNg "lbg" nlpSol (toLb (nlpBG nsi))
  , toInput gToGBar isNg "ubg" nlpSol (toUb (nlpBG nsi))
  , toInput (const id) isNp "p" nlpSol (nlpP nsi)
  ] ++
  catMaybes
  [ toInput lamXToLamXBar isNx "lam_x0" nlpSol <$> (nlpLamX0 nsi)
  , toInput lamGToLamGBar isNg "lam_g0" nlpSol <$> (nlpLamG0 nsi)
  ]
  where
    inf :: Double
    inf = read "Infinity"

    toLb :: View x => J x (Vector Bounds) -> J x (Vector Double)
    toLb = fmapJ (fromMaybe (-inf) . fst)

    toUb :: View x => J x (Vector Bounds) -> J x (Vector Double)
    toUb = fmapJ (fromMaybe   inf  . snd)

toInput ::
  View xg
  => (ScaleFuns x g DM -> (J xg DM -> J xg DM))
  -> (NlpSol x p g -> Int)
  -> String
  -> NlpSol x p g
  -> J xg (V.Vector Double)
  -> (String, DM)
toInput scaleFun getLen name nlpSol x0
  | nTypeLevel == nActual = (name, x)
  | otherwise =
      error $
      name ++ " dimension mismatch, " ++ show nTypeLevel ++
      " (type-level) /= " ++ show nActual ++ " (given)"
  where
    x = unM $ scaleFun (isScale nlpSol) (v2d x0)
    nActual = (CM.size1 x, CM.size2 x)
    nTypeLevel = (getLen nlpSol, 1)


toNlpSolOut :: (View x, View g) => NlpSol x p g -> M.Map String DM -> NlpOut x g (V.Vector Double)
toNlpSolOut nlpSol dmMap =
  NlpOut
  { fOpt = toOutput fbarToF "f" nlpSol dmMap
  , xOpt = toOutput xbarToX "x" nlpSol dmMap
  , gOpt = toOutput gbarToG "g" nlpSol dmMap
  , lambdaXOpt = toOutput lamXBarToLamX "lam_x" nlpSol dmMap
  , lambdaGOpt = toOutput lamGBarToLamG "lam_g" nlpSol dmMap
  }

toOutput ::
  View xg
  => (ScaleFuns x g DM -> (J xg DM -> J xg DM))
  -> String -> NlpSol x p g -> M.Map String DM -> J xg (V.Vector Double)
toOutput scaleFun name nlpSol dmMap = case M.lookup name dmMap of
  Nothing -> error $ "couldn't find output " ++ show name ++ " in outputs " ++ show (M.keys dmMap)
  Just r -> d2v $ scaleFun (isScale nlpSol) (mkM r)
  -- (d2v used to be dnonzeros)

data NlpSol x p g =
  NlpSol
  { isNx :: Int
  , isNg :: Int
  , isNp :: Int
  , isSolver :: Function
  , isVerbose :: Bool
  , isInterrupt :: IO ()
  , isSetParam :: J p (Vector Double) -> IO ()
  , isSuccessCodes :: [String]
  , isScale :: ScaleFuns x g DM
  }

-- | solve with given inputs, return success or failure code
callNlpsol :: (View x, View p, View g)
              => NlpSol x p g -> NlpIn x p g
              -> IO (M.Map String GType, Either String (NlpOut x g (Vector Double)))
callNlpsol nlpSol nlpInputs = do
  let solver = isSolver nlpSol

  -- put the param for callbacks https://github.com/casadi/casadi/issues/1770
  isSetParam nlpSol (nlpP nlpInputs)

  -- mvar that will be filled when nlp finishes
  stop <- newEmptyMVar :: IO (MVar (Maybe (M.Map String DM)))
  -- Flush stdout so that solver output comes after user output
  -- See https://github.com/haskell/process/issues/53
  hFlush stdout
  _ <- forkIO $ do
    when (isVerbose nlpSol) $
      putStrLn "calling nlpsol..."
    eoutputMap' <- try $ callDM' solver (fromNlpSolIn nlpSol nlpInputs)

    case eoutputMap' of
      Left (e :: SomeException) -> do
        when (isVerbose nlpSol) $
          putStrLn $ "solver caught exception: " ++ show e
        putMVar stop Nothing
      Right outputMap' -> do
        when (isVerbose nlpSol) $
          putStrLn "solver returned without exception"
        putMVar stop (Just outputMap')

  -- wait until nlp finishes
  when (isVerbose nlpSol) $
    putStrLn "waiting until nlp finishes..."
  ret <- try (takeMVar stop) :: IO (Either AsyncException (Maybe (M.Map String DM)))
  when (isVerbose nlpSol) $
    putStrLn "took stop var"

  moutputMap <- case ret of
    Right r -> return r -- no exceptions
    Left UserInterrupt -> do -- got ctrl-C
      isInterrupt nlpSol -- tell nlp to stop iterations
      _ <- takeMVar stop -- wait for nlp to return
      return Nothing
    Left _ -> do
      void (takeMVar stop) -- don't handle this one
      return Nothing
  stats <- C.function_stats__0 solver >>= mapM toGType
  let solveStatus = case M.lookup "return_status" stats of
        Nothing -> error "no \"return_status\" in stats"
        Just (GInt r) -> show r
        Just (GString r) -> r
        Just r -> error $ "nlp solver error: return status is not {string,int}, it's " ++ show r

  return $ case (solveStatus `elem` (isSuccessCodes nlpSol), moutputMap) of
    (True, Just outputMap) -> (stats, Right $ toNlpSolOut nlpSol outputMap)
    (False, _) -> (stats, Left solveStatus)
    (_, Nothing) -> (stats, Left "(Solver didn't return any answers)")


toNlpSol ::
  forall x p g .
  (View x, View p, View g)
  => Solver
  -> (J x MX -> J p MX -> (S MX, J g MX))
  -> Maybe (J x (Vector Double))
  -> Maybe (J g (Vector Double))
  -> Maybe Double
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> M.Map String GType -> IO Bool)
  -> IO (NlpSol x p g)
toNlpSol solverStuff nlpFun scaleX scaleG scaleF userCallback = do
  inputsX <- mkM <$> CM.sym "x" (size (Proxy :: Proxy x)) 1
  inputsP <- mkM <$> CM.sym "p" (size (Proxy :: Proxy p)) 1

  let scale :: forall sfa . CMatrix sfa => ScaleFuns x g sfa
      scale = mkScaleFuns scaleX scaleG scaleF

      (obj, g) = scaledFG scale nlpFun inputsX inputsP

      inputsXMat = unM inputsX
      inputsPMat = unM inputsP
      objMat     = unM obj
      gMat       = unM g

  -- add callback if user provides it
  when (verbose (runnerOptions solverStuff)) $ putStrLn "Creating callback..."
  intref <- newIORef False -- TODO(greg): make this in the solve function for concurrent solves
  paramRef <- newIORef (jfill 0) -- TODO(greg): get rid of this after https://github.com/casadi/casadi/issues/1770
  nlpsolOut <- C.nlpsol_out__1 :: IO (Vector String)

  let cb :: Vector DM -> M.Map String GType -> IO (Vector DM)
      cb cbInputs stats =
        case userCallback of
          Nothing -> return (V.singleton 0)
          Just callback
            | V.length cbInputs /= V.length nlpsolOut ->
                error $
                "length of callback inputs " ++ show (V.length cbInputs) ++
                " /= length of nlpsol outputs " ++ show (V.length nlpsolOut)
            | otherwise -> do
                let inputMap :: M.Map String DM
                    inputMap = M.fromList $ zip (V.toList nlpsolOut) (V.toList cbInputs)

                    lookupError name =
                      error $
                      "in nlpsol callback, error looking up " ++ show name ++ ": " ++
                      "available keys: " ++ show (V.toList nlpsolOut)
                    xval = case M.lookup "x" inputMap of
                      Nothing -> lookupError "x"
                      Just r -> d2v (xbarToX scale (mkM r))
                pval <- readIORef paramRef
--                    pval = case M.lookup "p" inputMap of
--                      Nothing -> lookupError "p"
--                      Just r -> d2v (mkM r)

                callbackRet <- callback xval pval stats
                -- Flush stdout so that solver output comes after user output
                -- See https://github.com/haskell/process/issues/53
                hFlush stdout

                interrupt <- readIORef intref
                return $ V.singleton $
                  if callbackRet && not interrupt
                  then 0
                  else fromIntegral (solverInterruptCode (getSolverInternal solverStuff))

  let spIn :: Vector Sparsity
      spIn = fmap toSpIn nlpsolOut
        where
          nx = size (Proxy :: Proxy x)
          ng = size (Proxy :: Proxy g)
          np = size (Proxy :: Proxy p)
          toSpIn :: String -> Sparsity
          toSpIn "f" = scalar
          toSpIn "x" = dense nx 1
          toSpIn "lam_x" = dense nx 1
          toSpIn "g" = dense ng 1
          toSpIn "lam_g" = dense ng 1
          toSpIn "lam_p" = dense np 1
          toSpIn r = error $ "when creating callback for nlpsol, got unhandled nlpsol output: " ++ show r
  let spOut :: Vector Sparsity
      spOut = V.singleton scalar
  casadiCallback <- makeCallback spIn spOut cb

  -- make the solver
  solverOptions <-
    T.mapM fromGType $
    M.fromList $
    ("iteration_callback", GFunction casadiCallback)
    : defaultSolverOptions (getSolverInternal solverStuff)
    ++ options solverStuff
  when (verbose (runnerOptions solverStuff)) $ putStrLn "create solver..."
  (solver, solverInitTime) <-
    timeIt $
    C.nlpsol__7
    "nlp_solver"
    (solverName (getSolverInternal solverStuff))
    (M.fromList [("x", inputsXMat), ("p", inputsPMat), ("f", objMat), ("g", gMat)])
    solverOptions

  when (verbose (runnerOptions solverStuff)) $
    printf "solver initialized in %s\n" (formatSeconds solverInitTime)

  let proxy :: J f b -> Proxy f
      proxy = const Proxy

      nlpSol =
        NlpSol
        { isNx = size (proxy inputsX)
        , isNp = size (proxy inputsP)
        , isNg = size (proxy g)
        , isSolver = solver
        , isVerbose = verbose (runnerOptions solverStuff)
        , isInterrupt = writeIORef intref True
        , isSetParam = writeIORef paramRef
        , isSuccessCodes = successCodes (getSolverInternal solverStuff)
        , isScale = scale
        }
  return nlpSol
