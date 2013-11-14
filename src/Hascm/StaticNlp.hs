{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}

module Hascm.StaticNlp
       ( NlpMonad
       , (===)
       , (<==)
--       , leq3
       , minimize
       , designVar
       , buildNlp
       ) where

import Control.Monad ( when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState, get, put )
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Data.Sequence ( (|>) )
import qualified Data.Vector as V

import Hascm.Vectorize
import Hascm.TypeNats
import Hascm.Nlp
import Hascm.TypeVecs ( Vec )
import qualified Hascm.TypeVecs as TV

import Hascm.StaticNlp.LogsAndErrors
import Hascm.StaticNlp.StaticNlpTypes


--import Hascm.AlgorithmV
import Dvda.Expr
import Dvda.Algorithm.Construct ( Algorithm(..), AlgOp(..) )
import Dvda.Algorithm.FunGraph ( Node(..) )

import Dvda.Algorithm

withEllipse :: Int -> String -> String
withEllipse n blah
  | length blah <= n = blah
  | otherwise = take n blah ++ "..."

newtype NlpMonad a =
  NlpMonad
  { runNlp :: ErrorT ErrorMessage (WriterT [LogMessage] (State NlpState)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState NlpState
             , MonadWriter [LogMessage]
             )

emptySymbolicNlp :: NlpState
emptySymbolicNlp = NlpState S.empty HS.empty S.empty ObjectiveUnset

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
  put $ state0 { nlpConstraints = nlpConstraints state0 |> (Eq2 lhs rhs) }

infix 4 <==
(<==) :: Expr Double -> Expr Double -> NlpMonad ()
(<==) lhs rhs = do
  debug $ "adding inequality constraint: " ++
     withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ state0 { nlpConstraints = nlpConstraints state0 |> (Ineq2 lhs rhs) }

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


toG :: (Eq a, Num a, NaturalT ng) => S.Seq (Constraint (Expr a)) -> Vec ng (Expr a, (Maybe Double, Maybe Double))
toG nlpConstraints' = TV.mkSeq $ fmap constr (nlpConstraints')

convertAlgorithm :: Floating a => Algorithm Double -> Algorithm a
convertAlgorithm alg = alg { algOps = newAlgOps }
  where
    newAlgOps = map convert (algOps alg)

    convert :: Floating a => AlgOp Double -> AlgOp a
    convert (InputOp k x) = InputOp k x
    convert (OutputOp k x) = OutputOp k x
    convert (NormalOp k x) = NormalOp k (convertG x)
    
    convertG :: Floating a => GExpr Double Node -> GExpr a Node
    convertG (GSym x) = GSym x
    convertG (GConst c) = GConst (fromRational (toRational c))
    convertG (GNum x) = GNum x
    convertG (GFractional x) = GFractional x
    convertG (GFloating x) = GFloating x

buildNlp :: forall nx ng .
            (NaturalT nx, NaturalT ng) =>
            NlpMonad () -> IO (Nlp (Vec nx) (Vec ng), [LogMessage])
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
      fg :: forall b . Floating b => Vec nx b -> NlpFun (Vec ng) b
      fg x = devectorize $ runAlgorithm alg (vectorize x)
  --mapM_ print (algOps alg)
  --print inputs
  return (Nlp fg xbnd gbnd, logs)
