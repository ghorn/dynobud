{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language KindSignatures #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language InstanceSigs #-}

module Hascm.Cov
       ( Cov(..)
       , covN
       , toMatrix
       , fromMatrix
       , nOfVecLen
       , diag
       , diag'
       ) where

import Control.Monad ( when )
import qualified Data.Vector as V
import Data.Serialize
import GHC.Generics
import System.IO.Unsafe ( unsafePerformIO )
import Casadi.Wrappers.Classes.Sparsity ( sparsity_triu )
import Casadi.Wrappers.Classes.SX ( sx''''''''' )
import Casadi.Wrappers.Classes.GenSX ( GenSXClass(..) )
import Casadi.Wrappers.Tools ( triu2symm'' )

import Hascm.Casadi.SX
import Hascm.Casadi.SXElement
import Hascm.Vectorize

data Cov (f :: * -> *) a = Cov { unCov :: V.Vector a } deriving (Eq, Functor, Generic, Generic1, Show)

-- THIS SKIPS THE DEVECTORIZE LENGTH CHECK!!
instance (Serialize a) => Serialize (Cov f a) where
  put = put . V.toList . unCov
  get = fmap (Cov . V.fromList) get

instance Vectorize f => Vectorize (Cov f) where
  vectorize :: Cov f a -> V.Vector a
  vectorize = unCov
  devectorize :: forall a . V.Vector a -> Cov f a
  devectorize v
    | vl == tvl = ret
    | otherwise = error $ "Cov: devectorize dimension mismatch, want: " ++ show tvl ++ ", got: " ++ show vl
    where
      vl = V.length v
      tvl = vlength ret
      ret :: Cov f a
      ret = Cov v
  empty :: Cov f ()
  empty = ret
    where
      ret = Cov (V.replicate (covLength ret) ())

toMatrix :: Vectorize f => Cov f SXElement -> SX
toMatrix c@(Cov xs) = unsafePerformIO $ do
  let n = covN c
  when (covLength c /= V.length xs) $ error "toMatrix mismatch :("
  sp <- sparsity_triu n
  triu <- sx''''''''' sp xs
  triu2symm'' (castGenSX triu)
{-# NOINLINE toMatrix #-}

diag :: (Num a, Vectorize f) => f a -> Cov f a
diag = flip diag' 0

diag' :: forall f a . Vectorize f => f a -> a -> Cov f a
diag' v' offDiag = devectorize $ V.fromList (reverse (concat (foo v n)))
  where
    v = reverse (V.toList (vectorize v'))
    n = vlength v'

    blah :: Int -> a -> [a]
    blah k x = x : replicate (k-1) offDiag

    foo :: [a] -> Int -> [[a]]
    foo (v0:vs) k = blah k v0 : foo vs (k-1)
    foo [] 0 = []
    foo _ _ = error "Cov: diag mismatch"


fromMatrix :: Vectorize f => SX -> Cov f SXElement
fromMatrix x = devectorize (sdata (striu (sfull x)))

covLength :: Vectorize f => Cov f a -> Int
covLength c = (n*n + n) `div` 2
  where
    n = vlength (fOf c)

covN :: Vectorize f => Cov f a -> Int
covN c = vlength (fOf c)

-- inverse of covLength
nOfVecLen :: Int -> Int
nOfVecLen vl
  | abs (realToFrac intRet - doubleRet) < 1e-6 = intRet
  | otherwise = error $ "nOfVecLen: i don't think this is a valid length: " ++
                show (vl,doubleRet,intRet)
  where
    doubleRet = sqrt (2 * (fromIntegral vl :: Double) + 0.25) - 0.5
    intRet = round doubleRet

fOf :: Cov f a -> f a
fOf _ = undefined
