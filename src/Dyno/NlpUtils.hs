{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiWayIf #-}

module Dyno.NlpUtils
       ( solveNlpHomotopy
       , solveNlp
       , solveNlp'
       ) where

import Control.Monad ( when, void )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO ( hFlush, stdout )
import Text.Printf ( printf )

import Casadi.SX ( SX )
import qualified Casadi.GenericC as Gen

import Dyno.View.Unsafe.View ( unJ, mkJ )

import Dyno.SXElement ( sxSplitJV, sxCatJV )
import Dyno.Vectorize ( Vectorize(..), Id(..) )
import Dyno.View.JV ( JV )
import Dyno.View.View ( View(..), J, JNone(..), JTuple(..), jfill, unzipJ, fmapJ )
import Dyno.View.Symbolic ( Symbolic )
import Dyno.Nlp ( Nlp(..), NlpOut(..), Nlp'(..), NlpOut'(..), Bounds )
import Dyno.Solvers ( Solver )
import Dyno.NlpSolver


-- | solve a homotopy nlp
solveNlpHomotopy ::
  forall x p g a .
  (View x, View p, View g, Symbolic a)
  => Double -> (Double, Double, Int, Int)
  -> Solver
  -> Nlp' x p g a -> J p (Vector Double) -> Maybe (J (JTuple x p) (Vector Double) -> IO Bool)
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> Double -> IO ())
  -> IO (Either String String, NlpOut' (JTuple x p) g (Vector Double))
solveNlpHomotopy userStep (reduction, increase, iterIncrease, iterDecrease)
  solverStuff nlp pF callback callbackP = do
  when (reduction >= 1) $ error $ "homotopy reduction factor " ++ show reduction ++ " >= 1"
  when (increase  <= 1) $ error $ "homotopy increase factor "  ++ show increase  ++ " <= 1"
  let fg :: J (JTuple x p) a -> J JNone a -> (J (JV Id) a, J g a)
      fg xp _ = nlpFG' nlp x p
        where
          JTuple x p = split xp
  runNlpSolver solverStuff fg Nothing (nlpScaleG' nlp) (nlpScaleF' nlp) callback $ do
    let (lbx,ubx) = unzipJ (nlpBX' nlp)
        (lbg,ubg) = unzipJ (nlpBG' nlp)
        p0 = unJ $ nlpP' nlp

        setAlpha :: Double -> NlpSolver (JTuple x p) JNone g ()
        setAlpha alpha = do
          let p = mkJ $ V.zipWith (+) p0 (V.map (alpha*) (V.zipWith (-) (unJ pF) p0))
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










-- | convenience function to solve a pure Nlp
solveNlp :: forall x p g .
  (Vectorize x, Vectorize p, Vectorize g)
  => Solver
  -> Nlp x p g SXElement -> Maybe (x Double -> IO Bool)
  -> IO (Either String String, NlpOut x g Double)
solveNlp solverStuff nlp callback = do
  let nlp' :: Nlp' (JV x) (JV p) (JV g) SX
      nlp' = Nlp' { nlpFG' = \x' p' -> let x = sxSplitJV x' :: x SXElement
                                           p = sxSplitJV p' :: p SXElement
                                           (obj,g) = nlpFG nlp x p :: (SXElement, g SXElement)
                                           obj' = sxCatJV (Id obj) :: J (JV Id) SX
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


-- | convenience function to solve a pure Nlp'
solveNlp' ::
  (View x, View p, View g, Symbolic a)
  => Solver
  -> Nlp' x p g a -> Maybe (J x (Vector Double) -> IO Bool)
  -> IO (Either String String, NlpOut' x g (Vector Double))
solveNlp' solverStuff nlp callback =
  runNlp solverStuff nlp callback solve'
