{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}
{-# Language KindSignatures #-}

module Dyno.AlgorithmV
       ( AlgorithmV(..)
       , runAlgorithmV
       , toCallAlgorithmV
       , toSymbolicAlgV
       , constructAlgorithmV
       , constructAlgorithmV'
       , convertAlgorithm
       ) where

import qualified Data.Vector as V

import Dvda.Algorithm.Construct ( Algorithm(..), AlgOp(..), constructAlgorithm )
import Dvda.Algorithm.Eval ( runAlgorithm )
import Dvda.Algorithm.FunGraph ( Node(..) )
import Dvda.Algorithm ( toSymbolicAlg )
import Dvda.Expr

import Dyno.Vectorize

newtype AlgorithmV (f :: * -> *) (g :: * -> *) a = AlgorithmV (Algorithm a)

toSymbolicAlgV :: Eq a => AlgorithmV f g a -> AlgorithmV f g (Expr a)
toSymbolicAlgV (AlgorithmV alg) = AlgorithmV (toSymbolicAlg alg)

constructAlgorithmV :: (Vectorize f, Vectorize g) =>
                       (f (Expr a) -> g (Expr a)) -> IO (AlgorithmV f g a)
constructAlgorithmV f = fmap AlgorithmV (constructAlgorithm vinputs voutputs)
  where
    vinputs = ssyms n "x"
    n = vlength inputs
    inputs = devectorize vinputs
    outputs = f inputs

    voutputs = vectorize outputs

constructAlgorithmV' :: (Vectorize f, Vectorize g) =>
                        (forall a . Floating a => f a -> g a) -> IO (AlgorithmV f g Double)
constructAlgorithmV' f = fmap AlgorithmV (constructAlgorithm vinputs voutputs)
  where
    vinputs = ssyms n "x"
    n = vlength inputs

    inputs = devectorize vinputs
    outputs = f inputs

    voutputs = vectorize outputs

ssyms :: Int -> String -> V.Vector (Expr a)
ssyms k name = V.fromList $ take k allSyms
  where
    allSyms = map (sym . ((name ++ "_") ++) . show) [(0::Int)..]

runAlgorithmV :: (Vectorize f, Vectorize g) => AlgorithmV f g a -> f a -> g a
runAlgorithmV (AlgorithmV alg) inputs = case outputVec of
  Right ret -> devectorize ret
  Left err -> error $ "runAlgorithmV: " ++ err
  where
    inputVec = vectorize inputs
    outputVec = runAlgorithm alg inputVec

toCallAlgorithmV :: (Vectorize f, Vectorize g) => (f (Expr a) -> g (Expr a)) -> IO (f a -> g a)
toCallAlgorithmV f = do
  alg <- constructAlgorithmV f
  return (runAlgorithmV alg)

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
