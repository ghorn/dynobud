{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Dyno.View.Interpolant
       ( interpolant1, interpolant2, interpolant3
       ) where

import Data.Map ( Map )
import qualified Data.Vector as V
import Linear ( V2, V3 )

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
    values :: g (f Double)
    values = sequenceA values0

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize (vectorize values)

    fun :: Fun (J (JV V2)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts
    grid = V.fromList [vectorize grid0, vectorize grid1]

interpolant3 ::
  forall f g h
  . ( Vectorize f, Traversable f
    , Vectorize g, Applicative g, Traversable g
    , Vectorize h, Applicative h
    )
  => String -> String -> f Double -> g Double -> h Double -> f (g (h Double)) -> Map String GType
  -> Fun (J (JV V3)) S
interpolant3 name solver grid0 grid1 grid2 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant3 " ++ name ++ ": " ++ err
  where
    -- transpose values
    values :: h (g (f Double))
    values = sequenceA $ fmap sequenceA (sequenceA values0)
    -- values = fmap sequenceA $ sequenceA (fmap sequenceA values0)

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize $ V.concatMap vectorize (vectorize values)

    fun :: Fun (J (JV V3)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts
    grid = V.fromList [vectorize grid0, vectorize grid1, vectorize grid2]
