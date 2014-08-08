{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language KindSignatures #-}
--{-# Language DeriveGeneric #-}

module Dyno.Cov
       ( Cov(..)
       , toMatrix
       , toMatrix'
       , toMatrix''
       , toHMatrix
       , toHMatrix'
       , fromMatrix
       , fromMatrix'
       , fromMatrix''
       , diag
       , diag'
       , diag''
       , nOfVecLen
       ) where

--import GHC.Generics ( Generic )
import Control.Monad ( when )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Serialize
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Packed.Matrix as Mat

import Casadi.Core.Classes.Sparsity ( sparsity_triu )
import Casadi.Core.Classes.SX --( sx''''''''' )
import Casadi.Core.Classes.MX --( mx'''''''''' )
import Casadi.Core.Classes.DMatrix
import qualified Casadi.Core.Tools as C

import qualified Dyno.Casadi.SX as SX
import qualified Dyno.Casadi.MX as MX
import qualified Dyno.Casadi.DMatrix as DMatrix
import Dyno.Casadi.SXElement
import Dyno.View
import qualified Dyno.View.Symbolic as S

newtype Cov (f :: * -> *) a = Cov { unCov :: Vector (J S a) } deriving (Eq, Show)
instance View f => View (Cov f) where
  cat = mkJ . vveccat . fmap unJ . unCov
  split = Cov . fmap mkJ . flip vvertsplit ks . unJ
    where
      ks = V.fromList $ take (1 + size (Proxy :: Proxy (Cov f))) [0..]
      --sizes kf 0 = [kf]
      --sizes k0 k = k0 : sizes (k0 + k) (k-1)
      --n = size (Proxy :: Proxy f)
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

-- THIS SKIPS THE DEVECTORIZE LENGTH CHECK!!
instance (Serialize a) => Serialize (Cov f a) where
  put = put . V.toList . unCov
  get = fmap (Cov . V.fromList) get

toMatrix :: forall f . View f => J (Cov f) SX -> SX
toMatrix c = unsafePerformIO $ do
  let c'@(Cov xs) = split c
      n = covN c'
      _ = xs :: Vector (J S SX)
      toScalar :: SX -> SXElement
      toScalar x = case V.toList (SX.sdata (SX.sdense x)) of
        [y] -> y
        ys -> error $ "Cov: toMatrix: toScalar: got non-scalar, length " ++ show (length ys)
      xs' = fmap (toScalar . unJ) xs :: Vector SXElement
      expected = size (Proxy :: Proxy (Cov f))
      actual = V.length xs
  when (expected /= actual) $ error $
    "toMatrix mismatch, dim: " ++ show n ++ ", expected: " ++
    show expected ++ ", actual: " ++ show actual
  sp <- sparsity_triu n
  triu <- sx__9 sp xs'
  C.triu2symm__1 triu
{-# NOINLINE toMatrix #-}

toMatrix' :: forall f . View f => J (Cov f) MX -> MX
toMatrix' c = unsafePerformIO $ do
  let mymx = unJ c
      n = covN (split c)
  sp <- sparsity_triu n
  triu <- mx__4 sp mymx -- :: Sparsity -> MX -> IO MX
  C.triu2symm__0 triu
{-# NOINLINE toMatrix' #-}

toMatrix'' :: forall f . View f => J (Cov f) DMatrix -> DMatrix
toMatrix'' c = unsafePerformIO $ do
  let c'@(Cov xs) = split c
      n = covN c'
      _ = xs :: Vector (J S DMatrix)
      toScalar :: DMatrix -> Double
      toScalar x = case V.toList (DMatrix.ddata (DMatrix.ddense x)) of
        [y] -> y
        ys -> error $ "Cov: toMatrix: toScalar: got non-scalar, length " ++ show (length ys)
      xs' = fmap (toScalar . unJ) xs :: Vector Double
      expected = size (Proxy :: Proxy (Cov f))
      actual = V.length xs
  when (expected /= actual) $ error $
    "toMatrix mismatch, dim: " ++ show n ++ ", expected: " ++
    show expected ++ ", actual: " ++ show actual
  sp <- sparsity_triu n
  triu <- dmatrix__6 sp xs'
  C.triu2symm__2 triu
{-# NOINLINE toMatrix'' #-}

toHMatrix :: forall f . View f => J (Cov f) DMatrix -> Mat.Matrix Double
toHMatrix m = (n Mat.>< n) (V.toList v)
  where
    v = DMatrix.ddata $ DMatrix.ddense $ toMatrix'' m
    n = size (Proxy :: Proxy f)

toHMatrix' :: forall f . View f => J (Cov f) (Vector Double) -> Mat.Matrix Double
toHMatrix' (UnsafeJ v) = toHMatrix $ (UnsafeJ (DMatrix.dvector v) :: J (Cov f) DMatrix)


diag :: View f => J f SX -> J (Cov f) SX
diag = fromMatrix . S.diag . unJ

diag' :: View f => J f MX -> J (Cov f) MX
diag' = fromMatrix' . MX.diag . unJ

diag'' :: View f => J f DMatrix -> J (Cov f) DMatrix
diag'' = fromMatrix'' . DMatrix.ddiag . unJ

--data X a = X (J S a) (J S a) deriving (Generic, Show)
--instance View X
--xx = X 1 2 :: X DMatrix
--xx' = cat xx
--
--dd :: J (Cov X) DMatrix
--dd = diag'' xx'
--
--sp :: DMatrix
--sp = toMatrix'' dd
--
--dd2 :: J (Cov X) DMatrix
--dd2 = fromMatrix'' sp

-- todo: this is way too dense
fromMatrix :: View f => SX -> J (Cov f) SX
fromMatrix x = unsafePerformIO $ fmap mkJ $ C.vecNZ__1 (SX.striu (SX.sdense x))
{-# NOINLINE fromMatrix #-}

fromMatrix' :: View f => MX -> J (Cov f) MX
fromMatrix' x = unsafePerformIO $ fmap mkJ $ C.vecNZ__0 (MX.triu (MX.dense x))
{-# NOINLINE fromMatrix' #-}

fromMatrix'' :: View f => DMatrix -> J (Cov f) DMatrix
fromMatrix'' x = unsafePerformIO $ fmap mkJ $ C.vecNZ__2 (DMatrix.dtriu (DMatrix.ddense x))
{-# NOINLINE fromMatrix'' #-}

covN :: forall f a . View f => Cov f a -> Int
covN = const $ size (Proxy :: Proxy f)
