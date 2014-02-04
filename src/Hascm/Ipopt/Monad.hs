{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language PolyKinds #-}

module Hascm.Ipopt.Monad
       ( Ipopt
       , runIpopt
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
         -- * options
       , IpoptOption
       , setOption
       ) where

import Control.Applicative ( Applicative )
import Control.Monad ( liftM, when )
import Control.Monad.Reader ( MonadIO(..), MonadReader(..), ReaderT(..) )
import qualified Data.Vector as V

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

import Hascm.Casadi.SX
import Hascm.Casadi.DMatrix
import Hascm.Casadi.SXMatrix
import Hascm.Casadi.IOSchemes

import Hascm.Vectorize
import Hascm.Nlp ( NlpFun(..), NlpInputs(..), NlpOut(..), Multipliers(..) )

setInput :: (IpoptState -> Int) -> String -> V.Vector Double -> Ipopt x p g ()
setInput getLen name x = do
  ipoptState <- ask
  let nx' = V.length x
      nx = getLen ipoptState
  when (nx /= nx') $ error $ name ++ " dimension mismatch, " ++ show nx ++ " (true) /= " ++ show nx' ++ " (given)"
  liftIO $ ioInterfaceFX_setInput''''' (isSolver ipoptState) x name
  return ()

setX0 :: Vectorize x => x Double -> Ipopt x p g ()
setX0 = (setInput isNx "x0") . vectorize

setLbx :: Vectorize x => x Double -> Ipopt x p g ()
setLbx = setInput isNx "lbx" . vectorize

setUbx :: Vectorize x => x Double -> Ipopt x p g ()
setUbx = setInput isNx "ubx" . vectorize

setLbg :: Vectorize g => g Double -> Ipopt x p g ()
setLbg = setInput isNg "lbg" . vectorize

setUbg :: Vectorize g => g Double -> Ipopt x p g ()
setUbg = setInput isNg "ubg" . vectorize

setP :: Vectorize p => p Double -> Ipopt x p g ()
setP = setInput isNp "p" . vectorize


getInput :: String -> Ipopt x p g (V.Vector Double)
getInput name = do
  ipoptState <- ask
  liftIO $ fmap ddata $ ioInterfaceFX_input'' (isSolver ipoptState) name

getX0 :: Vectorize x => Ipopt x p g (x Double)
getX0 = liftM devectorize $ getInput "x0"

getLbx :: Vectorize x => Ipopt x p g (x Double)
getLbx = liftM devectorize $ getInput "lbx"

getUbx :: Vectorize x => Ipopt x p g (x Double)
getUbx = liftM devectorize $ getInput "ubx"

getLbg :: Vectorize g => Ipopt x p g (g Double)
getLbg = liftM devectorize $ getInput "lbg"

getUbg :: Vectorize g => Ipopt x p g (g Double)
getUbg = liftM devectorize $ getInput "ubg"

getP :: Vectorize p => Ipopt x p g (p Double)
getP = liftM devectorize $ getInput "p"

getOutput :: String -> Ipopt x p g (V.Vector Double)
getOutput name = do
  ipoptState <- ask
  liftIO $ fmap ddata $ ioInterfaceFX_output'' (isSolver ipoptState) name

getF :: Ipopt x p g Double
getF = liftM V.head $ getOutput "f"

getX :: Vectorize x => Ipopt x p g (x Double)
getX = liftM devectorize $ getOutput "x"

getG :: Vectorize g => Ipopt x p g (g Double)
getG = liftM devectorize $ getOutput "g"

getLamX :: Vectorize x => Ipopt x p g (x Double)
getLamX = liftM devectorize $ getOutput "lam_x"

getLamG :: Vectorize g => Ipopt x p g (g Double)
getLamG = liftM devectorize $ getOutput "lam_g"


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

setOption :: IpoptOption a => String -> a -> Ipopt x p g ()
setOption name val = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  liftIO $ do
    gt <- mkGeneric val
    optionsFunctionality_setOption ipopt name gt
  reinit

reinit :: Ipopt x p g ()
reinit = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  liftIO $ sharedObject_init ipopt

-- | solve with current inputs, return success or failure code
solve :: Ipopt x p g (Either String String)
solve = do
  ipoptState <- ask
  let ipopt = isSolver ipoptState
  solveStatus <- liftIO $ do
    fxSolveSafe ipopt
    fx_getStat ipopt "return_status"  >>= printableObject_getDescription
  return $ if solveStatus `elem` ["Solve_Succeeded", "Solved_To_Acceptable_Level"]
    then Right solveStatus
    else Left solveStatus

-- | solve with current inputs, return lots of info on success, or message on failure
solve' :: (Vectorize x, Vectorize g) => Ipopt x p g (Either String (NlpOut x g Double))
solve' = do
  solveStatus <- solve
  case solveStatus of
    Left ss -> return (Left ss)
    Right _ -> do
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
      return (Right nlpOut)


data IpoptState = IpoptState { isNx :: Int
                             , isNg :: Int
                             , isNp :: Int
                             , isSolver :: IpoptSolver
                             }
newtype Ipopt x p g a =
  Ipopt (ReaderT IpoptState IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader IpoptState
           , MonadIO
           )

runIpopt :: (Vectorize x, Vectorize p, Vectorize g) =>
  (forall b . Floating b => NlpInputs x p b -> NlpFun g b)
  -> Maybe (x Double -> IO Bool)
  -> Ipopt x p g a
  -> IO a
runIpopt nlpFun callback' (Ipopt ipoptMonad) = do
  (NlpInputs inputsX' inputsP', NlpFun obj g') <- funToSX nlpFun
  
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
  ipopt <- ipoptSolver'' (castFX f)

  -- add callback if user provides it
  case callback' of
    Nothing -> return ()
    Just callback -> do
      let cb fx' = do
            xval <- fmap ddata $ ioInterfaceFX_output fx' 0
            callbackRet <- callback (devectorize xval)
            return $ if callbackRet then 0 else 1
      casadiCallback <- makeCallback cb >>= genericTypeCallback
      optionsFunctionality_setOption ipopt "iteration_callback" casadiCallback

  sharedObject_init ipopt

  let ipoptState = IpoptState { isNx = V.length inputsX
                              , isNp = V.length inputsP
                              , isNg = V.length g
                              , isSolver = ipopt
                              }
  liftIO $ runReaderT ipoptMonad ipoptState
