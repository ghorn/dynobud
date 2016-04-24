{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Dyno.NlpUtils
       ( HomotopyParams(..)
       , solveNlpHomotopy
       , solveNlp
       , solveNlpV
       ) where

import qualified Control.Applicative as A
import qualified Data.Traversable as T
import Control.Monad ( when, void )
import qualified Data.Map as M
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO ( hFlush, stdout )
import Text.Printf ( printf )

import Casadi.MX ( MX )

import Dyno.View.M ( vcat, vsplit )
import Dyno.View.Unsafe ( mkM, unM )
import Dyno.Vectorize ( Vectorize(..), Id(..) )
import Dyno.View.View ( View(..), J, S, JV, JNone(..), catJV, splitJV )
import Dyno.Nlp ( Nlp(..), NlpIn(..), NlpOut(..), Bounds )
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
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> M.Map String GType -> IO Bool)
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> Double -> IO ())
  -> IO (t (NlpOut x g (Vector Double)))
solveNlpHomotopy userStep hp
  solverStuff nlp pFs callback callbackP = do
  when ((reduction hp) >= 1) $ error $ "homotopy reduction factor " ++ show (reduction hp) ++ " >= 1"
  when ((increase hp)  <= 1) $ error $ "homotopy increase factor "  ++ show (increase hp)  ++ " <= 1"

  let p0 = nlpP (nlpIn nlp)

  nlpSol <- toNlpSol solverStuff (nlpFG nlp) (nlpScaleX nlp) (nlpScaleG nlp) (nlpScaleF nlp) callback

  _ <- case callback of
   Nothing -> return True
   Just cb -> cb (nlpX0 (nlpIn nlp)) (nlpP (nlpIn nlp)) M.empty

  -- initial solve
  (_, ret0) <- callNlpsol nlpSol (nlpIn nlp)

  initialSol <- case ret0 of
    Right r -> return r
    Left msg -> error $ "error: homotopy solver initial guess not good enough\n" ++ msg

  -- run the homotopy
  let runCallback :: J x (Vector Double) -> J p (Vector Double) -> Double -> IO ()
      runCallback x p alphaTrial = case callbackP of
        Nothing -> return ()
        Just cbp -> void (cbp x p alphaTrial)

      solveOneStage ::
        (Int, Double, J p (Vector Double), NlpOut x g (Vector Double))
        -> J p (Vector Double)
        -> IO ((Int, Double, J p (Vector Double), NlpOut x g (Vector Double)), NlpOut x g (Vector Double))
      solveOneStage (stage, step0, p0', stageNlpOut0) pF' = do

        (eret, stepF) <- tryStep stageNlpOut0 0 0 step0
        ret <- case eret of
          Left msg -> error msg
          Right r -> return r
        return ((stage + 1, stepF, pF', ret), ret)
        where
          toAlpha :: Double -> J p (Vector Double)
          toAlpha alpha = mkM $ V.zipWith (+) p0'' (V.map (alpha*) (V.zipWith (-) (unM pF') p0''))
            where
              p0'' = unM p0'

          tryStep :: NlpOut x g (Vector Double)
                  -> Int -> Double -> Double
                  -> IO ((Either String (NlpOut x g (Vector Double))), Double)
          tryStep oldNlpOut majorIter alpha0 step
            | step < 1e-12 = error "step size too small"
            | otherwise = do
              printf "%3d %4d, alpha: %.2e, step: %.2e " stage majorIter alpha0 step
              hFlush stdout
              let (alphaTrial, alphaIsOne)
                    | alpha0 + step >= 1 = (1, True)
                    | otherwise = (alpha0 + step, False)
                  trialP = toAlpha alphaTrial
                  trialNlpIn =
                    NlpIn
                    { nlpX0 = xOpt oldNlpOut
                    , nlpBX = nlpBX (nlpIn nlp)
                    , nlpBG = nlpBG (nlpIn nlp)
                    , nlpP = trialP
                    , nlpLamX0 = Just (lambdaXOpt oldNlpOut)
                    , nlpLamG0 = Just (lambdaGOpt oldNlpOut)
                    }
              (stats, ret) <- callNlpsol nlpSol trialNlpIn
              case ret of
                Left msg -> do
                  putStrLn $ "step failed to solve: " ++ msg
                  tryStep oldNlpOut (majorIter+1) alpha0 ((reduction hp)*step)
                Right newNlpOut -> do
                  let iters = case M.lookup "iter_count" stats of
                        Nothing -> error "error getting number of iterations"
                        Just (GInt r) -> r
                        Just _ -> error "number of iterations is not an int"
                  putStrLn $ "step successful (" ++ show iters ++ " iterations)"
                  runCallback (xOpt newNlpOut) trialP alphaTrial

                  if alphaIsOne
                    then return (Right newNlpOut, step)
                    else do let nextStep
                                  | iters < (iterIncrease hp) = step*(increase hp)
                                  | iters < (iterDecrease hp) = step
                                  | otherwise                 = step*(reduction hp)
                            tryStep newNlpOut (majorIter + 1) alphaTrial nextStep

  (_, ret) <- mapAccumL' solveOneStage (0, userStep, p0, initialSol) pFs
  putStrLn "homotopy successful"
  return ret


-- | convenience function to solve a simple Nlp
-- .
-- For better performance on large problems and more options, use the View-based interfaces instead
solveNlpV :: forall x g .
  (Vectorize x, Vectorize g)
  => Solver
  -> (forall a . Floating a => x a -> (a, g a))
  -> x Bounds
  -> g Bounds
  -> x Double
  -> Maybe (x Double -> M.Map String GType -> IO Bool)
  -> IO (Either String (Double, x Double))
solveNlpV solverStuff fg bx bg x0 cb = do
  let nlp :: Nlp (JV x) JNone (JV g) MX
      nlp =
        Nlp
        { nlpFG = \x' _ ->
           let _ = x' :: J (JV x) MX
               x = vsplit x' :: x (S MX)
               (obj,g) = fg x :: (S MX, g (S MX))
               --obj' = sxCatJV (Id obj) :: S MX
               --g' = sxCatJV g :: J (JV g) MX
           in (obj, vcat g)
        , nlpIn =
            NlpIn
            { nlpBX = catJV bx
            , nlpBG = catJV bg
            , nlpX0 = catJV x0
            , nlpP  = cat JNone -- mkM $ vectorize (nlpP  nlp) :: J (JV p) (V.Vector Double)
            , nlpLamX0 = Nothing --fmap (mkM . vectorize) (nlpLamX0 nlp)
                                 -- :: Maybe (J (JV x) (V.Vector Double))
            , nlpLamG0 = Nothing -- fmap (mkM . vectorize) (nlpLamG0 nlp)
                                 -- :: Maybe (J (JV g) (V.Vector Double))
            }
        , nlpScaleF = Nothing -- nlpScaleF nlp
        , nlpScaleX = Nothing -- fmap (mkM . vectorize) (nlpScaleX nlp)
                               -- :: Maybe (J (JV x) (V.Vector Double))
        , nlpScaleG = Nothing -- fmap (mkM . vectorize) (nlpScaleG nlp)
                      -- :: Maybe (J (JV g) (V.Vector Double))
        }

      callback :: Maybe (J (JV x) (Vector Double) -> J JNone (Vector Double) -> M.Map String GType
                         -> IO Bool)
      callback = case cb of
        Nothing -> Nothing
        Just cb' -> Just $ \x _ stats -> cb' (splitJV x) stats

  (_stats, ret0) <- solveNlp solverStuff nlp callback
  return $ case ret0 of
    Left m  -> Left m
    Right sol -> Right $ (unId (splitJV (fOpt sol)), splitJV (xOpt sol))


-- | convenience function to solve a pure Nlp
solveNlp ::
  (View x, View p, View g)
  => Solver
  -> Nlp x p g MX
  -> Maybe (J x (Vector Double) -> J p (Vector Double) -> M.Map String GType -> IO Bool)
  -> IO (M.Map String GType, Either String (NlpOut x g (Vector Double)))
solveNlp solverStuff nlp callback = do
  nlpSol <-
    toNlpSol solverStuff
    (nlpFG nlp) (nlpScaleX nlp) (nlpScaleG nlp) (nlpScaleF nlp) callback
  callNlpsol nlpSol (nlpIn nlp)
