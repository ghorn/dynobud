{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dyno.View.MapFun
       ( MapStrategy(..)
       , mapFun
       , mapFun'
       ) where

import GHC.Generics ( Generic )
import GHC.TypeLits ( KnownNat, natVal )

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Proxy
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Vector as V

import qualified Casadi.Core.Classes.Function as C
import Casadi.GenericType ( GenericType, GType, fromGType )

import Dyno.TypeVecs ( Vec )
import Dyno.View.Fun
import Dyno.View.HList
import Dyno.View.JVec ( JVec )
import Dyno.View.M ( M )
import Dyno.View.Scheme ( Scheme )
import Dyno.View.Vectorize ( Id )
import Dyno.View.View ( JV )

data MapStrategy = Unroll | Serial | OpenMP deriving (Show, Eq, Ord, Generic)

mapStrategyString :: MapStrategy -> String
mapStrategyString Unroll = "unroll"
mapStrategyString Serial = "serial"
mapStrategyString OpenMP = "openmp"

class ParScheme f where
  type Par f (n :: k) :: * -> *

-- normal
instance ParScheme (M f g) where
  type Par (M f g) n = M f (JVec n g)

-- multiple inputs/outputs
instance ParScheme (f :*: g) where
  type Par (f :*: g) n = (Par f n) :*: (Par g n)

-- | symbolic fmap
mapFun :: forall f g n
          . ( Scheme (Par f n), Scheme (Par g n), KnownNat n )
          => Proxy n
          -> Fun f g
          -> MapStrategy
          -> IO (Fun (Par f n) (Par g n))
mapFun _ (Fun f) mapStrategy = do
  let n = fromIntegral (natVal (Proxy :: Proxy n))
  fm <- C.function_map__5 f n (mapStrategyString mapStrategy) :: IO C.Function
  checkFunDimensionsWith "mapFun'" (Fun fm)
-- {-# NOINLINE mapFun #-}


class ParScheme' f0 f1 where
  repeated :: Proxy f0 -> Proxy f1 -> Seq Bool

-- normal
instance ParScheme' (M f g) (M f (JVec n g)) where
  repeated _ _ = S.singleton True

instance ParScheme' (M f (JV Id)) (M f (JV (Vec n))) where
  repeated _ _ = S.singleton True

-- non-repeated
instance ParScheme' (M f g) (M f g) where
  repeated _ _ = S.singleton False

-- multiple inputs/output
instance (ParScheme' f0 f1, ParScheme' g0 g1) => ParScheme' (f0 :*: g0) (f1 :*: g1) where
  repeated pfg0 pfg1 = repeated pf0 pf1 S.>< repeated pg0 pg1
    where
      splitProxy :: Proxy (f :*: g) -> (Proxy f, Proxy g)
      splitProxy _ = (Proxy, Proxy)

      (pf0, pg0) = splitProxy pfg0
      (pf1, pg1) = splitProxy pfg1

-- | symbolic fmap which can do non-repeated inputs/outputs
mapFun' :: forall i0 i1 o0 o1 n
          . ( ParScheme' i0 i1, ParScheme' o0 o1
            , Scheme i0, Scheme o0
            , Scheme i1, Scheme o1
            , KnownNat n
            )
          => Proxy n
          -> Fun i0 o0
          -> String
          -> MapStrategy
          -> M.Map String GType
          -> IO (Fun i1 o1)
mapFun' _ f0 name mapStrategy opts0 = do
--  let fds = checkFunDimensions f0
--  putStrLn "mapFun'' input dimensions:"
--  case fds of
--   Left msg -> putStrLn msg
--   Right msg -> putStrLn msg
  _ <- checkFunDimensionsWith "mapFun'' input fun" f0
  opts <- T.mapM fromGType opts0 :: IO (M.Map String GenericType)
  let n = fromIntegral (natVal (Proxy :: Proxy n))
      repeatedIn :: V.Vector Bool
      repeatedIn =
        V.fromList $ F.toList $ repeated (Proxy :: Proxy i0) (Proxy :: Proxy i1)
      repeatedOut :: V.Vector Bool
      repeatedOut =
        V.fromList $ F.toList $ repeated (Proxy :: Proxy o0) (Proxy :: Proxy o1)

      toIndices :: V.Vector Bool -> V.Vector Int
      toIndices vbool = V.fromList $ f 0 (V.toList vbool)
        where
          f k (False:bs) = k : f (k+1) bs
          f k (True:bs) = f (k+1) bs
          f _ [] = []

--  putStrLn $ "repeated in: " ++ show repeatedIn
--  putStrLn $ "repeated out: " ++ show repeatedOut

  fm <- C.function_map__3 (unFun f0) name (mapStrategyString mapStrategy) n (toIndices repeatedIn) (toIndices repeatedOut) opts :: IO C.Function
  checkFunDimensionsWith "mapFun''" (Fun fm)
-- {-# NOINLINE mapFun' #-}
