{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Dyno.View.MapFun
       ( mapFun
       ) where

import qualified Data.Map as M
import Data.Proxy
import qualified Data.Traversable as T

import qualified Casadi.Function as C
import Casadi.Option

import qualified Casadi.Core.Classes.Function as F

import Dyno.TypeVecs ( Dim )
import qualified Dyno.TypeVecs as TV
import Dyno.Vectorize ( Id )
import Dyno.View.Fun
import Dyno.View.JV ( JV )
import Dyno.View.JVec ( JVec )
import Dyno.View.Unsafe.View ( J(..) )
import Dyno.View.M ( M )
import Dyno.View.View ( View )

-- | symbolic fmap
mapFun :: forall fun f g n
          . (FunClass fun, View f, View g, Dim n)
          => String
          -> fun (J f) (J g)
          -> M.Map String Opt
          -> IO (Fun (M (JV Id) (JVec n f)) (M (JV Id) (JVec n g)))
mapFun name f' opts0 = do
  opts <- T.mapM mkGeneric opts0 :: IO (M.Map String GenericType)
  let Fun f = toFun f'
      n = TV.reflectDim (Proxy :: Proxy n)
  fm <- F.function_map__1 f name n opts :: IO C.Function
  checkFunDimensionsWith "funMap" (Fun fm)
