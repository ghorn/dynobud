{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language PackageImports #-}

module Dyno.NlpMonad
       ( NlpMonad
       , (===)
       , (<==)
       , (>==)
--       , leq3
       , minimize
       , designVar
       , buildNlp
       , buildNlp'
       , reifyNlp
       ) where

import Control.Applicative ( Applicative )
import Control.Monad ( when )
import "mtl" Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import "mtl" Control.Monad.State ( State, MonadState, runState, get, put )
import "mtl" Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Data.Sequence ( (|>) )
import qualified Data.Vector as V
import Linear.V ( Dim(..) )
import Data.Proxy

import Dvda.Expr
import Dvda.Algorithm

import Dyno.Casadi.SXElement ( SXElement )
import Dyno.Vectorize
import Dyno.AlgorithmV( convertAlgorithm )
import Dyno.Nlp
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.Interface.LogsAndErrors
import Dyno.Interface.Types

withEllipse :: Int -> String -> String
withEllipse n blah
  | length blah <= n = blah
  | otherwise = take n blah ++ "..."

newtype NlpMonad a =
  NlpMonad
  { runNlp :: ErrorT ErrorMessage (WriterT [LogMessage] (State NlpState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorMessage
             , MonadState NlpState
             , MonadWriter [LogMessage]
             )

emptySymbolicNlp :: NlpState
emptySymbolicNlp = NlpState S.empty HS.empty S.empty ObjectiveUnset HomotopyParamUnset

build' :: NlpState -> NlpMonad a -> (Either ErrorMessage a, [LogMessage], NlpState)
build' nlp0 builder = (result, logs, state)
  where
    ((result,logs),state) =
      flip runState nlp0 . runWriterT . runErrorT . runNlp $ builder

build :: NlpMonad a -> (Either ErrorMessage a, [LogMessage], NlpState)
build = build' emptySymbolicNlp

designVar :: String -> NlpMonad (Expr Double)
designVar name = do
  debug $ "adding design variable \""++name++"\""
  state0 <- get
  let map0 = nlpXSet state0
      sym' = Sym name
  when (HS.member sym' map0) $ err $ name ++ " already in symbol map"
  let state1 = state0 { nlpX = nlpX state0 |> sym'
                      , nlpXSet =  HS.insert sym' map0
                      }
  put state1
  return (ESym sym')

infix 4 ===
(===) :: Expr Double -> Expr Double -> NlpMonad ()
(===) lhs rhs = do
  debug $ "adding equality constraint: " ++
    withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Eq2 lhs rhs }

infix 4 <==
(<==) :: Expr Double -> Expr Double -> NlpMonad ()
(<==) lhs rhs = do
  debug $ "adding inequality constraint: " ++
     withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq2 lhs rhs }

infix 4 >==
(>==) :: Expr Double -> Expr Double -> NlpMonad ()
(>==) lhs rhs = do
  debug $ "adding inequality constraint: " ++
     withEllipse 30 (show lhs) ++ " >= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> Ineq2 rhs lhs }

--leq3 :: Expr Double -> Expr Double -> Expr Double -> NlpMonad ()
--leq3 lhs mid rhs = do
--  debug $ "adding inequality constraint bounds: " ++
--    withEllipse 30 (show lhs) ++ " <= " ++
--    withEllipse 30 (show mid) ++ " <= " ++
--    withEllipse 30 (show rhs)
--  state0 <- get
--  put $ state0 { nlpConstraints = nlpConstraints state0 |> (Ineq3 lhs mid rhs) }

minimize :: Expr Double -> NlpMonad ()
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


constr :: (Eq a, Num a) => Constraint (Expr a) -> (Expr a, (Maybe Double, Maybe Double))
constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))


toG :: (Eq a, Num a, Dim ng) => S.Seq (Constraint (Expr a)) -> Vec ng (Expr a, (Maybe Double, Maybe Double))
toG nlpConstraints' = TV.mkSeq $ fmap constr nlpConstraints'

buildNlp :: forall nx ng .
            (Dim nx, Dim ng) =>
            NlpMonad () -> IO (Nlp (Vec nx) None (Vec ng), [LogMessage])
buildNlp nlp = do
  let (_,logs,state) = build nlp
  obj <- case nlpObj state of
    Objective obj' -> return obj'
    ObjectiveUnset -> error "solveNlp: objective unset"

  let inputs :: [Expr Double]
      inputs = map ESym (F.toList (nlpX state))

      g :: Vec ng (Expr Double)
      gbnd :: Vec ng (Maybe Double, Maybe Double)
      (g, gbnd) = TV.tvunzip $ toG (nlpConstraints state)

      xbnd :: Vec nx (Maybe Double, Maybe Double)
      xbnd = fill (Nothing, Nothing)
      nlpFun = NlpFun obj g
  alg' <- constructAlgorithm (V.fromList inputs) (vectorize nlpFun)
  let alg :: forall b . Floating b => Algorithm b
      alg = convertAlgorithm alg'
      fg :: forall b . Floating b => NlpInputs (Vec nx) None b -> NlpFun (Vec ng) b
      fg (NlpInputs x _) = case runAlgorithm alg (vectorize x) of
        Right ret -> devectorize ret
        Left errmsg -> error $ "buildNlp: algorithm: " ++ errmsg
  --mapM_ print (algOps alg)
  --print inputs

      nlp' = Nlp { nlpFG = fg
                 , nlpBX = xbnd
                 , nlpBG = gbnd
                 , nlpX0 = fmap (const 0) xbnd
                 , nlpP = None
                 }
  return (nlp', logs)



toG' :: (Eq a, Num a) => S.Seq (Constraint (Expr a)) -> V.Vector (Expr a, (Maybe Double, Maybe Double))
toG' nlpConstraints' = V.fromList $ F.toList $ fmap constr nlpConstraints'


buildNlp' :: NlpMonad a -> IO (Nlp V.Vector V.Vector V.Vector, [LogMessage])
buildNlp' nlp = do
  let (_,logs,state) = build nlp
  obj <- case nlpObj state of
    Objective obj' -> return obj'
    ObjectiveUnset -> error "solveNlp: objective unset"

  let inputs :: [Expr Double]
      inputs = map ESym (F.toList (nlpX state))
      nx = length inputs

      parameters :: [Expr Double]
      parameters = []
      np = 0

      g :: V.Vector (Expr Double)
      gbnd :: V.Vector (Maybe Double, Maybe Double)
      (g, gbnd) = V.unzip $ toG' (nlpConstraints state)
      ng = V.length g

      xbnd :: V.Vector (Maybe Double, Maybe Double)
      xbnd = V.replicate nx (Nothing, Nothing)
  alg' <- constructAlgorithm (V.fromList inputs V.++ V.fromList parameters) (V.singleton obj V.++ g)
  let alg :: forall b . Floating b => Algorithm b
      alg = convertAlgorithm alg'
      fg :: forall b . Floating b => NlpInputs V.Vector V.Vector b -> NlpFun V.Vector b
      fg (NlpInputs x p)
        | V.length x /= nx = error $ "static nlp: V.length x /= nx " ++ show (V.length x, nx)
        | V.length p /= np = error $ "static nlp: V.length p /= np " ++ show (V.length p, np)
        | V.length g' /= ng = error $ "static nlp: V.length g /= ng " ++ show (V.length g', ng)
        | otherwise = NlpFun (V.head vout) g'
        where
          g' = V.tail vout
          vout = case runAlgorithm alg (x V.++ p) of
            Right ret -> ret
            Left errmsg -> error $ "buildNlp': algorithm: " ++ errmsg

      nlp' = Nlp { nlpFG = fg
                 , nlpBX = xbnd
                 , nlpBG = gbnd
                 , nlpX0 = fmap (const 0) xbnd
                 , nlpP = V.empty
                 }
  return (nlp', logs)



reifyNlp ::
  forall r .
  Nlp V.Vector V.Vector V.Vector -> Maybe (V.Vector Double -> IO Bool) ->
  (forall x p g . (Vectorize x, Vectorize p, Vectorize g) =>
   Nlp x p g -> Maybe (x Double -> IO Bool) -> r) ->
  r
reifyNlp nlp cb f =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim ng $ \(Proxy :: Proxy ng) ->
  f (Nlp
     ((\(NlpInputs x' p') ->
        fout devectorize (fg (NlpInputs (vectorize x') (vectorize p')))) :: NlpInputs (Vec nx) (Vec np) SXElement -> NlpFun (Vec ng) SXElement)
     (TV.mkVec bx :: Vec nx (Maybe Double, Maybe Double))
     (TV.mkVec bg :: Vec ng (Maybe Double, Maybe Double))
     (TV.mkVec x0 :: Vec nx Double)
     (TV.mkVec p :: Vec np Double)
    ) (fmap (. vectorize) cb)
  where
    fout :: (f a -> g a) -> NlpFun f a -> NlpFun g a
    fout f' (NlpFun obj g) = NlpFun obj (f' g)

    nx = V.length bx
    ng = V.length bg
    np = V.length p

    bx = nlpBX nlp
    bg = nlpBG nlp
    x0 = nlpX0 nlp
    p = nlpP nlp

    fg :: NlpInputs V.Vector V.Vector SXElement -> NlpFun V.Vector SXElement
    fg = nlpFG nlp
