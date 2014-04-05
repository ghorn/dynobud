{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PackageImports #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RankNTypes #-}

module Dyno.NlpMonad
       ( NlpMonad
       , (===)
       , (<==)
       , (>==)
       , bound
       , minimize
       , designVar
       , reifyNlp
       ) where

import Control.Applicative ( Applicative )
import Control.Monad ( when )
import "mtl" Control.Monad.Reader ( MonadIO(..) )
import "mtl" Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import "mtl" Control.Monad.State ( StateT, MonadState, runStateT, get, put )
import "mtl" Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Data.Sequence ( (|>) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Linear.V ( Dim(..) )
import Data.Proxy

import Dyno.Casadi.SXElement ( SXElement, sxElement_sym )
import Dyno.Casadi.SX ( svector )
import Dyno.Casadi.SharedObject ( soInit )
import Dyno.Casadi.MX ( MX )
import Dyno.Casadi.SXFunction
import Dyno.Casadi.Function
import Dyno.Vectorize
import Dyno.Nlp ( Nlp'(..), Bounds)
import Dyno.TypeVecs ( Vec )
import Dyno.View.View
import qualified Dyno.TypeVecs as TV
import Dyno.Interface.LogsAndErrors
import Dyno.Interface.Types

withEllipse :: Int -> String -> String
withEllipse n blah
  | length blah <= n = blah
  | otherwise = take n blah ++ "..."

newtype NlpMonad a =
  NlpMonad
  { runNlp :: ErrorT ErrorMessage (WriterT [LogMessage] (StateT NlpMonadState IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorMessage
             , MonadState NlpMonadState
             , MonadWriter [LogMessage]
             , MonadIO
             )

emptySymbolicNlp :: NlpMonadState
emptySymbolicNlp = NlpMonadState S.empty HS.empty S.empty ObjectiveUnset HomotopyParamUnset

build :: NlpMonad a -> IO (Either ErrorMessage a, [LogMessage], NlpMonadState)
build = build' emptySymbolicNlp
  where
    build' :: NlpMonadState -> NlpMonad a -> IO (Either ErrorMessage a, [LogMessage], NlpMonadState)
    build' nlp0 builder = do
      ((result,logs),state) <- flip runStateT nlp0 . runWriterT . runErrorT . runNlp $ builder
      return (result, logs, state)

designVar :: String -> NlpMonad SXElement
designVar name = do
  debug $ "adding design variable \""++name++"\""
  state0 <- get
  let map0 = nlpXSet state0
  sym <- liftIO (sxElement_sym name)
  when (HS.member name map0) $ err $ name ++ " already in symbol map"
  let state1 = state0 { nlpX = nlpX state0 |> (name, sym)
                      , nlpXSet =  HS.insert name map0
                      }
  put state1
  return sym

infix 4 ===
(===) :: SXElement -> SXElement -> NlpMonad ()
(===) lhs rhs = do
  debug $ "adding equality constraint: " ++
    withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Eq2 lhs rhs }

infix 4 <==
(<==) :: SXElement -> SXElement -> NlpMonad ()
(<==) lhs rhs = do
  debug $ "adding inequality constraint: " ++
     withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq2 lhs rhs }

infix 4 >==
(>==) :: SXElement -> SXElement -> NlpMonad ()
(>==) lhs rhs = do
  debug $ "adding inequality constraint: " ++
     withEllipse 30 (show lhs) ++ " >= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq2 rhs lhs }

bound :: SXElement -> (Double,Double) -> NlpMonad ()
bound mid (lhs, rhs) = do
  debug $ "adding inequality bound: " ++
    withEllipse 30 (show lhs) ++ " <= " ++
    withEllipse 30 (show mid) ++ " <= " ++
    withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> (Ineq3 mid (lhs, rhs)) }

minimize :: SXElement -> NlpMonad ()
minimize obj = do
  debug $ "setting objective function: " ++ withEllipse 30 (show obj)
  state0 <- get
  case nlpObj state0 of
    Objective x -> err $ init $ unlines
                   [ "you set the objective function twice"
                   , "    old val: " ++ show x
                   , "    new val: " ++ show obj
                   ]
    ObjectiveUnset -> put $ state0 { nlpObj = Objective obj }


constr :: Constraint SXElement -> (SXElement, Bounds)
constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))
constr (Ineq3 x (lhs,rhs)) = (x, (Just lhs, Just rhs))


toG :: Dim ng => S.Seq (Constraint SXElement) -> Vec ng (SXElement, Bounds)
toG nlpConstraints' = TV.mkSeq $ fmap constr nlpConstraints'

buildNlp :: forall nx ng .
            (Dim nx, Dim ng) => NlpMonadState -> IO (Nlp' (JVec nx S) JNone (JVec ng S) MX)
buildNlp state = do
  obj <- case nlpObj state of
    Objective obj' -> return obj'
    ObjectiveUnset -> error "solveNlp: objective unset"

  let inputs :: Vector SXElement
      inputs = V.fromList $ map snd $ F.toList (nlpX state)

      g :: Vec ng SXElement
      gbnd :: Vec ng Bounds
      (g, gbnd) = TV.tvunzip $ toG (nlpConstraints state)

      xbnd :: Vec nx Bounds
      xbnd = fill (Nothing, Nothing)
  sxfun <- sxFunction (V.fromList [svector inputs]) (V.fromList [svector (V.singleton obj), svector (TV.unVec g)])
  soInit sxfun
  let fg :: J (JVec nx S) MX -> J JNone MX -> (J S MX, J (JVec ng S) MX)
      fg x _ = (mkJ (ret V.! 0), mkJ (ret V.! 1))
        where
          ret = callMX sxfun (V.singleton (unJ x))

  return Nlp' { nlpFG' = fg
              , nlpBX' = mkJ (TV.unVec xbnd)
              , nlpBG' = mkJ (TV.unVec gbnd)
              , nlpX0' = jfill 0
              , nlpP' = cat JNone
              }


reifyNlp ::
  forall r .
  NlpMonad () -> Maybe (Vector Double -> IO Bool) -> M.Map String Double
  -> (forall x g . (View x, View g)
      => Nlp' x JNone g MX -> Maybe (J x (Vector Double) -> IO Bool) -> NlpMonadState -> IO r)
  -> IO r
reifyNlp nlpmonad cb x0map f = do
  (ret,logs,state) <- build nlpmonad
  case ret of
    Right _ -> return ()
    Left err' -> error $ unlines $ (map show logs) ++ [show err']

  let nx = S.length (nlpX state)
      ng = S.length (nlpConstraints state)

      lookupGuess = flip (M.findWithDefault 0) x0map
      x0 = V.fromList $ map (lookupGuess . fst) $ F.toList (nlpX state)
      
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
--  TV.reifyDim np $ \(Proxy :: Proxy np) ->
    TV.reifyDim ng $ \(Proxy :: Proxy ng) -> do
      nlp0 <- buildNlp state :: IO (Nlp' (JVec nx S) JNone (JVec ng S) MX)
      let nlp = nlp0 { nlpX0' = mkJ x0 }
      ret' <- f nlp (fmap (. unJ) cb) state
      return ret'
