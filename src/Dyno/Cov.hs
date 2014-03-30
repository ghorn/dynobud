{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language KindSignatures #-}
--{-# Language DeriveGeneric #-}

module Dyno.Cov
       ( Cov(..)
       , toMatrix
       , toMatrix'
       , toMatrix''
       , fromMatrix
       , fromMatrix'
       , fromMatrix''
       , diag
       , diag'
       , diag''
       ) where

--import GHC.Generics ( Generic )
import Control.Monad ( when )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Serialize
import System.IO.Unsafe ( unsafePerformIO )
import Casadi.Wrappers.Classes.Sparsity ( sparsity_triu )
import Casadi.Wrappers.Classes.SX --( sx''''''''' )
import Casadi.Wrappers.Classes.MX --( mx'''''''''' )
import Casadi.Wrappers.Classes.DMatrix
import Casadi.Wrappers.Classes.GenSX ( GenSXClass(..) )
import Casadi.Wrappers.Classes.GenMX ( GenMXClass(..) )
import Casadi.Wrappers.Classes.GenDMatrix ( GenDMatrixClass(..) )
import qualified Casadi.Wrappers.Tools as C

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
  triu <- sx''''' sp xs'
  C.triu2symm'' (castGenSX triu)
{-# NOINLINE toMatrix #-}

toMatrix' :: forall f . View f => J (Cov f) MX -> MX
toMatrix' c = unsafePerformIO $ do
  let mymx = unJ c
      n = covN (split c)
  sp <- sparsity_triu n
  triu <- mx'''' sp mymx -- :: Sparsity -> MX -> IO MX
  C.triu2symm''' (castGenMX triu)
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
  triu <- dmatrix''''' sp xs'
  C.triu2symm' (castGenDMatrix triu)
{-# NOINLINE toMatrix'' #-}

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
fromMatrix x = unsafePerformIO $ fmap mkJ $ (C.vecNZ'' (SX.striu (SX.sdense x)))
{-# NOINLINE fromMatrix #-}

fromMatrix' :: View f => MX -> J (Cov f) MX
fromMatrix' x = unsafePerformIO $ fmap mkJ $ (C.vecNZ''' (MX.triu (MX.dense x)))
{-# NOINLINE fromMatrix' #-}

fromMatrix'' :: View f => DMatrix -> J (Cov f) DMatrix
fromMatrix'' x = unsafePerformIO $ fmap mkJ $ (C.vecNZ' (DMatrix.dtriu (DMatrix.ddense x)))
{-# NOINLINE fromMatrix'' #-}

covN :: forall f a . View f => Cov f a -> Int
covN = const $ size (Proxy :: Proxy f)
