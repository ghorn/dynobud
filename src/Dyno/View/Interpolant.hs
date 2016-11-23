{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Dyno.View.Interpolant
       ( interpolant1, interpolant2
       ) where

import Data.Map ( Map )
import qualified Data.Vector as V
import Linear ( V2 )

import qualified Casadi.Interpolant as C
import Casadi.GenericType ( GType )

import Dyno.View.Fun ( Fun(..), checkFunDimensions )
import Dyno.View.Vectorize ( Vectorize(vectorize) )
import Dyno.View.View ( J, JV, S )

interpolant1 :: String -> String -> V.Vector (Double, Double) -> Map String GType -> Fun S S
interpolant1 name solver gridAndValues opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant1 " ++ name ++ ": " ++ err
  where
    fun :: Fun S S
    fun = Fun $ C.interpolant name solver (V.singleton grid) values opts
    (grid, values) = V.unzip gridAndValues

interpolant2 ::
  forall f g
  . (Vectorize f, Vectorize g, Traversable f, Applicative g)
  => String -> String -> f Double -> g Double -> f (g Double) -> Map String GType
  -> Fun (J (JV V2)) S
interpolant2 name solver grid0 grid1 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant2 " ++ name ++ ": " ++ err
  where
    -- transpose values
    values = sequenceA values0

    fun :: Fun (J (JV V2)) S
    fun = Fun $ C.interpolant name solver grid (V.concatMap vectorize (vectorize values)) opts
    grid = V.fromList [vectorize grid0, vectorize grid1]
