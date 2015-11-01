{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Dyno.NlpUtils
       ( HomotopyParams(..)
       , solveNlpHomotopy
       , solveNlpHomotopyWith
       , solveNlp
       , solveNlpWith
       , solveNlpV
       , setNlpInputs
       , runNlp
       , runNlpWith
       ) where

import qualified Control.Applicative as A
import qualified Data.Traversable as T
import Control.Monad ( when, void )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO ( hFlush, stdout )
import Text.Printf ( printf )

import Casadi.MX ( MX )
import qualified Casadi.GenericC as Gen

import Dyno.View.M ( vcat, vsplit )
import Dyno.View.Unsafe ( mkM, unM )
import Dyno.Vectorize ( Vectorize(..), Id(..) )
import Dyno.View.View ( View(..), J, JV, JNone(..), catJV, splitJV, unzipJ )
import Dyno.Nlp ( Nlp(..), NlpOut(..), Bounds )
import Dyno.Solvers ( Solver )
import Dyno.NlpSolver

-- for mapAccumL'
newtype StateL m s a = StateL { runStateL :: s -> m (s, a) }
instance Monad m => Functor (StateL m s) where
    fmap f (StateL k) = StateL $ \ s -> do
      (s', v) <- k s
      return (s', f v)
instance Monad m => A.Applicative (StateL m s) where
    pure x = StateL (\s -> return (s, x))
    StateL kf <*> StateL kv = StateL $ \ s -> do
      (s', f)  <- kf s
      (s'', v) <- kv s'
      return (s'', f v)

-- mapAccumL with monads
mapAccumL' :: (T.Traversable t, Monad m) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumL' f s t = runStateL (T.traverse (StateL . flip f) t) s

data HomotopyParams =
  HomotopyParams
  { reduction :: Double
  , increase :: Double
  , iterIncrease :: Int
  , iterDecrease :: Int
  }


-- | solve a homotopy nlp
solveNlpHomotopy ::
  forall x p g t .
  (View x, View p, View g, T.Traversable t)
  => Double -> HomotopyParams
  -> Solver
  -> Nlp x p g MX -> t (J p (Vector Double))
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> Double -> IO ())
  -> IO (t (NlpOut x g (Vector Double)))
solveNlpHomotopy = solveNlpHomotopyWith defaultRunnerOptions

-- | solve a homotopy nlp
solveNlpHomotopyWith ::
  forall x p g t .
  (View x, View p, View g, T.Traversable t)
  => RunNlpOptions
  -> Double -> HomotopyParams
  -> Solver
  -> Nlp x p g MX -> t (J p (Vector Double))
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> Double -> IO ())
  -> IO (t (NlpOut x g (Vector Double)))
solveNlpHomotopyWith options userStep hp
  solverStuff nlp pFs callback callbackP = do
  when ((reduction hp) >= 1) $ error $ "homotopy reduction factor " ++ show (reduction hp) ++ " >= 1"
  when ((increase hp)  <= 1) $ error $ "homotopy increase factor "  ++ show (increase hp)  ++ " <= 1"
  let fg :: J x MX -> J p MX -> (J (JV Id) MX, J g MX)
      fg x p = nlpFG nlp x p

  runNlpSolverWith options solverStuff fg (nlpScaleX nlp) (nlpScaleG nlp) (nlpScaleF nlp) callback $ do
    let (lbx,ubx) = unzipJ (nlpBX nlp)
        (lbg,ubg) = unzipJ (nlpBG nlp)
        p0 = nlpP nlp

    _ <- case callback of
     Nothing -> return True
     Just cb -> liftIO $ cb (nlpX0 nlp) (nlpP nlp)

    -- initial solve
    setX0 $ nlpX0 nlp
    setP $ nlpP nlp
    setLbx lbx
    setUbx ubx
    setLbg lbg
    setUbg ubg
    -- todo(greg): clean up redundancy?
    case nlpLamX0 nlp of
      Just lam -> setLamX0 lam
      Nothing -> return ()
    case nlpLamG0 nlp of
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
            x <- getX
            p <- getP
            liftIO $ void (cbp x p alphaTrial)

    let solveOneStage ::
          (Int, Double, J p (Vector Double))
          -> J p (Vector Double)
          -> NlpSolver x p g
               ((Int, Double, J p (Vector Double)), NlpOut x g (Vector Double))
        solveOneStage (stage, step0, p0') pF' = do
          ((msg, ret'), stepF) <- tryStep 0 0 step0
          ret <- case msg of
            Left x -> error x
            Right _ -> return ret'
          return ((stage + 1, stepF, pF'), ret)
          where
            setAlpha :: Double -> NlpSolver x p g ()
            setAlpha alpha = do
              let p0'' = unM p0'
              let p = mkM $ V.zipWith (+) p0'' (V.map (alpha*) (V.zipWith (-) (unM pF') p0''))
              setP p

            tryStep :: Int -> Double -> Double
                    -> NlpSolver x p g
                    ((Either String String, NlpOut x g (Vector Double)), Double)
            tryStep majorIter alpha0 step
              | step < 1e-12 = do _no <- getNlpOut
                                  error "step size too small"
--                                  return (Left "step size too small", no)
              | otherwise = do
                liftIO $ printf "%3d %4d, alpha: %.2e, step: %.2e " stage majorIter alpha0 step
                liftIO $ hFlush stdout
                let (alphaTrial, alphaIsOne)
                      | alpha0 + step >= 1 = (1, True)
                      | otherwise = (alpha0 + step, False)
                setAlpha alphaTrial
                ret <- solve'
                case ret of
                  (Left msg,_) -> do
                    liftIO $ putStrLn $ "step failed to solve: " ++ msg
                    tryStep (majorIter+1) alpha0 ((reduction hp)*step)
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
                    getX >>= setX0
                    getLamX >>= setLamX0
                    getLamG >>= setLamG0

                    if alphaIsOne
                      then return (ret, step)
                      else do let nextStep
                                    | iters < (iterIncrease hp) = step*(increase hp)
                                    | iters < (iterDecrease hp) = step
                                    | otherwise                 = step*(reduction hp)
                              tryStep (majorIter + 1) alphaTrial nextStep

    (_, ret) <- mapAccumL' solveOneStage (0, userStep, p0) pFs
    liftIO $ putStrLn "homotopy successful"
    return ret


-- | convenience function to solve a simple Nlp
-- .
-- For better performance and more options, use the View-based interfaces instead
solveNlpV :: forall x g .
  (Vectorize x, Vectorize g)
  => Solver
  -> (forall a . Floating a => x a -> (a, g a))
  -> x Bounds
  -> g Bounds
  -> x Double
  -> Maybe (x Double -> IO Bool)
  -> IO (Either String (Double, x Double))
solveNlpV solverStuff fg bx bg x0 cb = do
  let nlp :: Nlp (JV x) JNone (JV g) MX
      nlp =
        Nlp
        { nlpFG = \x' _ ->
           let _ = x' :: J (JV x) MX
               x = vsplit x' :: x (J (JV Id) MX)
               (obj,g) = fg x :: (J (JV Id) MX, g (J (JV Id) MX))
               --obj' = sxCatJV (Id obj) :: J (JV Id) MX
               --g' = sxCatJV g :: J (JV g) MX
           in (obj, vcat g)
        , nlpBX = catJV bx
        , nlpBG = catJV bg
        , nlpX0 = catJV x0
        , nlpP  = cat JNone -- mkM $ vectorize (nlpP  nlp) :: J (JV p) (V.Vector Double)
        , nlpLamX0 = Nothing --fmap (mkM . vectorize) (nlpLamX0 nlp)
                              -- :: Maybe (J (JV x) (V.Vector Double))
        , nlpLamG0 = Nothing -- fmap (mkM . vectorize) (nlpLamG0 nlp)
                              -- :: Maybe (J (JV g) (V.Vector Double))
        , nlpScaleF = Nothing -- nlpScaleF nlp
        , nlpScaleX = Nothing -- fmap (mkM . vectorize) (nlpScaleX nlp)
                               -- :: Maybe (J (JV x) (V.Vector Double))
        , nlpScaleG = Nothing -- fmap (mkM . vectorize) (nlpScaleG nlp)
                      -- :: Maybe (J (JV g) (V.Vector Double))
        }

      callback :: Maybe (J (JV x) (Vector Double) -> J JNone (Vector Double) -> IO Bool)
      callback = case cb of
        Nothing -> Nothing
        Just cb' -> Just $ \x _ -> cb' (splitJV x)

  (r0, r1) <- solveNlp solverStuff nlp callback
  return $ case r0 of
    Left m  -> Left m
    Right _ -> Right $ (unId (splitJV (fOpt r1)), splitJV (xOpt r1))

--  let r1 :: NlpOut x g Double
--      r1 = NlpOut { fOpt = V.head $ unM (fOpt' r1')
--                  , xOpt = devectorize $ unM (xOpt' r1')
--                  , gOpt = devectorize $ unM (gOpt' r1')
--                  , lambdaXOpt = devectorize $ unM $ lambdaXOpt' r1'
--                  , lambdaGOpt = devectorize $ unM $ lambdaGOpt' r1'
--                  }
--
--  return (r0, r1)


-- | convenience function to solve a pure Nlp
solveNlp ::
  (View x, View p, View g)
  => Solver
  -> Nlp x p g MX -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> IO (Either String String, NlpOut x g (Vector Double))
solveNlp solverStuff nlp callback =
  runNlp solverStuff nlp callback solve'

-- | convenience function to solve a pure Nlp
solveNlpWith ::
  (View x, View p, View g)
  => RunNlpOptions
  -> Solver
  -> Nlp x p g MX -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> IO (Either String String, NlpOut x g (Vector Double))
solveNlpWith opts solverStuff nlp callback =
  runNlpWith opts solverStuff nlp callback solve'


-- | set all inputs
setNlpInputs :: (View x, View p, View g) => Nlp x p g MX -> NlpSolver x p g ()
setNlpInputs nlp = do
  let (lbx,ubx) = unzipJ (nlpBX nlp)
      (lbg,ubg) = unzipJ (nlpBG nlp)

  setX0 (nlpX0 nlp)
  setP (nlpP nlp)
  setLbx lbx
  setUbx ubx
  setLbg lbg
  setUbg ubg
  case nlpLamX0 nlp of
    Just lam -> setLamX0 lam
    Nothing -> return ()
  case nlpLamG0 nlp of
    Just lam -> setLamG0 lam
    Nothing -> return ()


-- | set all inputs, handle scaling, and let the user run a NlpMonad
runNlp ::
  (View x, View p, View g)
  => Solver
  -> Nlp x p g MX -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> NlpSolver x p g b
  -> IO b
runNlp = runNlpWith defaultRunnerOptions

-- | set all inputs, handle scaling, and let the user run a NlpMonad
runNlpWith ::
  (View x, View p, View g)
  => RunNlpOptions
  -> Solver
  -> Nlp x p g MX -> Maybe (J x (Vector Double) -> J p (Vector Double) -> IO Bool)
  -> NlpSolver x p g b
  -> IO b
runNlpWith options solverStuff nlp callback runMe =
  runNlpSolverWith options solverStuff (nlpFG nlp) (nlpScaleX nlp) (nlpScaleG nlp) (nlpScaleF nlp) callback $ do
    setNlpInputs nlp
    runMe
