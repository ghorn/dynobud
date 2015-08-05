{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module ExampleDsl.NlpMonad
       ( NlpMonad
       , (===)
       , (<==)
       , (>==)
       , bound
       , minimize
       , designVar
       , solveStaticNlp
       ) where

import qualified Control.Applicative as A
import Control.Monad ( when )
import "mtl" Control.Monad.Reader ( MonadIO(..) )
import "mtl" Control.Monad.Except ( ExceptT, MonadError, runExceptT )
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

import Casadi.SharedObject ( soInit )
import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.SXFunction
import Casadi.Function
import Casadi.CMatrix ( veccat )
import qualified Casadi.CMatrix as CM

import Dyno.View.Unsafe.View ( J(..), mkJ, unJ )

import Dyno.Vectorize ( Id, devectorize, fill )
import Dyno.TypeVecs ( Vec )
import Dyno.View.View ( View(..), JNone(..), jfill )
import Dyno.View.JV ( JV )
import Dyno.View.JVec ( JVec )
import qualified Dyno.View.Symbolic as Sym
import qualified Dyno.TypeVecs as TV
import Dyno.Solvers ( Solver )
import Dyno.NlpUtils ( solveNlp )
import Dyno.Nlp ( Nlp(..), NlpOut(..), Bounds)

import ExampleDsl.LogsAndErrors
import ExampleDsl.Types

type SXElement = J (JV Id) SX

sxElementSym :: String -> IO SXElement
sxElementSym = Sym.sym

sxElementToSX :: SXElement -> SX
sxElementToSX (UnsafeJ x)
  | (1,1) == sizes' = x
  | otherwise = error $ "sxElementToSX: got non-scalar of size " ++ show sizes'
  where
    sizes' = (CM.size1 x, CM.size2 x)

--withEllipse :: Int -> String -> String
--withEllipse n blah
--  | length blah <= n = blah
--  | otherwise = take n blah ++ "..."

newtype NlpMonad a =
  NlpMonad
  { runNlp :: ExceptT ErrorMessage (WriterT [LogMessage] (StateT NlpMonadState IO)) a
  } deriving ( Functor
             , A.Applicative
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
      ((result,logs),state) <- flip runStateT nlp0 . runWriterT . runExceptT . runNlp $ builder
      return (result, logs, state)

designVar :: String -> NlpMonad SXElement
designVar name = do
  debug $ "adding design variable \""++name++"\""
  state0 <- get
  let map0 = nlpXSet state0
  sym <- liftIO (sxElementSym name)
  when (HS.member name map0) $ err $ name ++ " already in symbol map"
  let state1 = state0 { nlpX = nlpX state0 |> (name, sym)
                      , nlpXSet =  HS.insert name map0
                      }
  put state1
  return sym

infix 4 ===
(===) :: SXElement -> SXElement -> NlpMonad ()
(===) lhs rhs = do
  debug $ "adding equality constraint: "
--    ++ withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Eq2 lhs rhs }

infix 4 <==
(<==) :: SXElement -> SXElement -> NlpMonad ()
(<==) lhs rhs = do
  debug $ "adding inequality constraint: "
--    ++ withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq2 lhs rhs }

infix 4 >==
(>==) :: SXElement -> SXElement -> NlpMonad ()
(>==) lhs rhs = do
  debug $ "adding inequality constraint: "
--    ++ withEllipse 30 (show lhs) ++ " >= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq2 rhs lhs }

bound :: SXElement -> (Double,Double) -> NlpMonad ()
bound mid (lhs, rhs) = do
  debug $ "adding inequality bound: " -- ++
--    withEllipse 30 (show lhs) ++ " <= " ++
--    withEllipse 30 (show mid) ++ " <= " ++
--    withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq3 mid (lhs, rhs) }

minimize :: SXElement -> NlpMonad ()
minimize obj = do
  debug $ "setting objective function: " -- ++ withEllipse 30 (show obj)
  state0 <- get
  case nlpObj state0 of
    Objective _x -> err $ init $ unlines
                   [ "you set the objective function twice"
--                   , "    old val: " ++ show x
--                   , "    new val: " ++ show obj
                   ]
    ObjectiveUnset -> put $ state0 { nlpObj = Objective obj }


constr :: Constraint SXElement -> (SXElement, Bounds)
constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))
constr (Ineq3 x (lhs,rhs)) = (x, (Just lhs, Just rhs))


toG :: Dim ng => S.Seq (Constraint SXElement) -> Vec ng (SXElement, Bounds)
toG nlpConstraints' = devectorize $ V.fromList $ F.toList $ fmap constr nlpConstraints'

buildNlp :: forall nx ng .
            (Dim nx, Dim ng) => NlpMonadState -> IO (Nlp (JVec nx (JV Id)) JNone (JVec ng (JV Id)) MX)
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

      svector = veccat . fmap sxElementToSX

  sxfun <- sxFunction (V.fromList [svector inputs]) (V.fromList [svector (V.singleton obj), svector (TV.unVec g)])
  soInit sxfun
  let fg :: J (JVec nx (JV Id)) MX -> J JNone MX -> (J (JV Id) MX, J (JVec ng (JV Id)) MX)
      fg x _ = (mkJ (ret V.! 0), mkJ (ret V.! 1))
        where
          ret = callMX sxfun (V.singleton (unJ x))

  return Nlp { nlpFG = fg
             , nlpBX = mkJ (TV.unVec xbnd)
             , nlpBG = mkJ (TV.unVec gbnd)
             , nlpX0 = jfill 0
             , nlpP = cat JNone
             , nlpScaleF = Nothing
             , nlpScaleX = Nothing
             , nlpScaleG = Nothing
             , nlpLamX0 = Nothing
             , nlpLamG0 = Nothing
             }


reifyNlp ::
  forall r .
  NlpMonad () -> Maybe (Vector Double -> IO Bool) -> M.Map String Double
  -> (forall x g . (View x, View g)
      => Nlp x JNone g MX -> Maybe (J x (Vector Double) -> J JNone (Vector Double) -> IO Bool) -> NlpMonadState -> IO r)
  -> IO r
reifyNlp nlpmonad cb0 x0map f = do
  (ret,logs,state) <- build nlpmonad
  case ret of
    Right _ -> return ()
    Left err' -> error $ unlines $ map show logs ++ [show err']

  let nx = S.length (nlpX state)
      ng = S.length (nlpConstraints state)

      lookupGuess = flip (M.findWithDefault 0) x0map
      x0 = V.fromList $ map (lookupGuess . fst) $ F.toList (nlpX state)
      
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
--  TV.reifyDim np $ \(Proxy :: Proxy np) ->
    TV.reifyDim ng $ \(Proxy :: Proxy ng) -> do
      nlp0 <- buildNlp state :: IO (Nlp (JVec nx (JV Id)) JNone (JVec ng (JV Id)) MX)
      let nlp = nlp0 { nlpX0 = mkJ x0 }
          cb = case cb0 of
            Nothing -> Nothing
            Just cb' -> Just $ \x _ -> cb' (unJ x)

      f nlp cb state


solveStaticNlp ::
  Solver
  -> NlpMonad () -> [(String,Double)] -> Maybe (Vector Double -> IO Bool)
  -> IO (Either String String, Double, [(String,Double)])
solveStaticNlp solverStuff nlp x0' callback = reifyNlp nlp callback x0 foo
  where
    x0 = M.fromListWithKey errlol x0'
    errlol name xx yy =
      error $ "solveStaticNlp: initial guess has variable \"" ++ name ++ "\" more than once: " ++
              show (xx,yy)

    foo ::
      (View x, View g) =>
      Nlp x JNone g MX -> Maybe (J x (Vector Double) -> J JNone (Vector Double) -> IO Bool) -> NlpMonadState ->
      IO (Either String String, Double, [(String,Double)])
    foo nlp' cb' state = do
      (ret,nlpOut) <- solveNlp solverStuff nlp' cb'
      let fopt = V.head (unJ (fOpt nlpOut)) :: Double
          xopt = F.toList $ unJ (xOpt nlpOut) :: [Double]
          xnames = map fst (F.toList (nlpX state)) :: [String]
      return (ret, fopt, zip xnames xopt)
