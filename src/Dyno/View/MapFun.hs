{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dyno.View.MapFun
       ( mapFun
       , mapFun'
       ) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Proxy
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Vector as V

import qualified Casadi.Function as C
import Casadi.Option

import qualified Casadi.Core.Classes.Function as F
import qualified Casadi.Core.Classes.Map as C

import Dyno.TypeVecs ( Dim )
import qualified Dyno.TypeVecs as TV
import Dyno.View.Fun
import Dyno.View.HList
import Dyno.View.JVec ( JVec )
import Dyno.View.M ( M )
import Dyno.View.Scheme ( Scheme )
import Dyno.View.View ( View )

class ParScheme f where
  type Par f (n :: k) :: * -> *

-- normal
instance (View f, View g) => ParScheme (M f g) where
  type Par (M f g) n = M f (JVec n g)

-- multiple inputs/outputs
instance (ParScheme f, ParScheme g) => ParScheme (f :*: g) where
  type Par (f :*: g) n = (Par f n) :*: (Par g n)

-- | symbolic fmap
mapFun :: forall fun f g n
          . ( FunClass fun
            , Scheme (Par f n), Scheme (Par g n)
            , Dim n )
          => Proxy n
          -> String
          -> fun f g
          -> M.Map String Opt
          -> IO (Fun (Par f n) (Par g n))
mapFun _ name f' opts0 = do
  opts <- T.mapM mkGeneric opts0 :: IO (M.Map String GenericType)
  let Fun f = toFun f'
      n = TV.reflectDim (Proxy :: Proxy n)
  fm <- F.function_map__1 f name n opts :: IO C.Function
  checkFunDimensionsWith "mapFun'" (Fun fm)
-- {-# NOINLINE mapFun #-}


class ParScheme' f0 f1 where
  repeated :: Proxy f0 -> Proxy f1 -> Seq Bool

-- normal
instance (View f, View g) => ParScheme' (M f g) (M f (JVec n g)) where
  repeated _ _ = S.singleton True

-- non-repeated
instance View f => ParScheme' (M f g) (M f g) where
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
mapFun' :: forall fun i0 i1 o0 o1 n
          . ( FunClass fun
            , ParScheme' i0 i1, ParScheme' o0 o1
            , Scheme i0, Scheme o0
            , Scheme i1, Scheme o1
            , Dim n
            )
          => Proxy n
          -> String
          -> fun i0 o0
          -> M.Map String Opt
          -> IO (Fun i1 o1)
mapFun' _ name f0 opts0 = do
--  let fds = checkFunDimensions f0
--  putStrLn "mapFun'' input dimensions:"
--  case fds of
--   Left msg -> putStrLn msg
--   Right msg -> putStrLn msg
  _ <- checkFunDimensionsWith "mapFun'' input fun" (toFun f0)
  opts <- T.mapM mkGeneric opts0 :: IO (M.Map String GenericType)
  let n = TV.reflectDim (Proxy :: Proxy n)
      repeatedIn =
        V.fromList $ F.toList $ repeated (Proxy :: Proxy i0) (Proxy :: Proxy i1)
      repeatedOut =
        V.fromList $ F.toList $ repeated (Proxy :: Proxy o0) (Proxy :: Proxy o1)
--  putStrLn $ "repeated in: " ++ show repeatedIn
--  putStrLn $ "repeated out: " ++ show repeatedOut

  fm <- C.map__1 name (unFun (toFun f0)) n repeatedIn repeatedOut opts :: IO C.Map
  checkFunDimensionsWith "mapFun''" (Fun (F.castFunction fm))
-- {-# NOINLINE mapFun' #-}
