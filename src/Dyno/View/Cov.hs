{-# OPTIONS_GHC -Wall -fno-cse #-}
{-# Language ScopedTypeVariables #-}
{-# Language KindSignatures #-}

module Dyno.View.Cov
       ( Cov(..)
       , toMat
       , fromMat
       , toMatrix
       , toHMatrix
       , toHMatrix'
       , fromMatrix
       , diag
       , diag'
       , nOfVecLen
       ) where

import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Packed.Matrix as Mat

import qualified Casadi.Sparsity as Sparsity
import Casadi.Slice ( slice' )
import Casadi.DMatrix ( DMatrix )
import Casadi.CMatrix ( CMatrix )
import qualified Casadi.CMatrix as CM

import Dyno.View.Unsafe.View ( unJ, mkJ )
import Dyno.View.Unsafe.M ( M(UnsafeM), mkM )

import Dyno.Vectorize ( Vectorize(..) )
import Dyno.View.View ( View(..), J )
import Dyno.View.JV ( JV )
import Dyno.View.Viewable ( Viewable(..) )
import Dyno.View.M ( toHMat )

newtype Cov (f :: * -> *) a = Cov a
instance View f => View (Cov f) where
  cat (Cov x) = mkJ x
  split x = Cov (unJ x)
  size = const $ (n*n + n) `div` 2
    where
      n = size (Proxy :: Proxy f)
  sizes k0 = const (Seq.singleton (k0 + n))
    where
      n = size (Proxy :: Proxy f)

nOfVecLen :: Int -> Int
nOfVecLen m
  | (n*n + n) `div` 2 == m = n
  | otherwise = error $ "nOfVecLen fail: " ++ show m
  where
    m' = fromIntegral m :: Double
    n = round $ sqrt (2*m' + 1/4) - 1/2

toMat :: (View f, CMatrix a, Viewable a) => J (Cov f) a -> M f f a
toMat c = mkM (toMatrix c)
{-# NOINLINE toMat #-}

toMatrix :: forall f a . (View f, CMatrix a, Viewable a) => J (Cov f) a -> a
toMatrix c = unsafePerformIO $ do
  let n = size (Proxy :: Proxy f)
  m <- CM.copy (CM.zerosSp (Sparsity.upper n))
  --CM.setNZ m (CM.dense (unJ c)) slice'
  CM.setNZ m (unJ c) slice' -- Joel says that "dense" isn't required here
  return (CM.triu2symm m)
{-# NOINLINE toMatrix #-}

toHMatrix :: forall f . View f => J (Cov f) DMatrix -> Mat.Matrix Double
toHMatrix m = toHMat (toMat m)

toHMatrix' :: forall f . View f => J (Cov f) (Vector Double) -> Mat.Matrix Double
toHMatrix' v = toHMatrix $ (mkJ (CM.fromDVector (unJ v)) :: J (Cov f) DMatrix)

diag :: (View f, CMatrix a, Viewable a) => J f a -> J (Cov f) a
diag = fromMatrix . CM.diag . unJ

diag' :: Vectorize f => f a -> a -> J (Cov (JV f)) (Vector a)
diag' x offDiag = mkJ $ V.fromList $ concat $ zipWith f vx [0..]
  where
    f y k = replicate k offDiag ++ [y]
    vx = V.toList $ vectorize x

--data X a = X (J S a) (J S a) deriving (Generic, Show)
--instance View X
--xx = X (mkJ 1) (mkJ 2) :: X DMatrix
--xx' = cat xx
--
--dd :: J (Cov X) DMatrix
--dd = diag xx'
--
--sp :: DMatrix
--sp = toMatrix dd
--
--dd2 :: J (Cov X) DMatrix
--dd2 = fromMatrix sp

fromMat :: (View f, CMatrix a, Viewable a) => M f f a -> J (Cov f) a
fromMat (UnsafeM c) = fromMatrix c

fromMatrix :: (View f, CMatrix a, Viewable a) => a -> J (Cov f) a
fromMatrix x = mkJ $ CM.getNZ (CM.triu (CM.dense x)) slice'
--fromMatrix x = mkJ $ CM.getNZ (CM.triu x) slice'
