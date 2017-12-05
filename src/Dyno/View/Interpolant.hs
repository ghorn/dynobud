{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Dyno.View.Interpolant
       ( interpolant1, interpolant2, interpolant3, interpolant4
       ) where

import Data.Map ( Map )
import qualified Data.Vector as V
import Linear ( V2, V3, V4 )

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
  forall f0 f1
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Applicative f1
    )
  => String -> String
  -> f0 Double -> f1 Double
  -> f0 (f1 Double)
  -> Map String GType
  -> Fun (J (JV V2)) S
interpolant2 name solver grid0 grid1 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant2 " ++ name ++ ": " ++ err
  where
    -- transpose values
    values :: f1 (f0 Double)
    values = sequenceA values0

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize (vectorize values)

    fun :: Fun (J (JV V2)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts
    grid = V.fromList [vectorize grid0, vectorize grid1]

interpolant3 ::
  forall f0 f1 f2
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Applicative f1, Traversable f1
    , Vectorize f2, Applicative f2
    )
  => String -> String
  -> f0 Double -> f1 Double -> f2 Double
  -> f0 (f1 (f2 Double))
  -> Map String GType
  -> Fun (J (JV V3)) S
interpolant3 name solver grid0 grid1 grid2 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant3 " ++ name ++ ": " ++ err
  where
    -- transpose values
    values :: f2 (f1 (f0 Double))
    values = v3
      where
        v0 :: f0 (f1 (f2 Double))
        v0 = values0

        v1 :: f1 (f0 (f2 Double))
        v1 = sequenceA v0

        v2 :: f1 (f2 (f0 Double))
        v2 = fmap sequenceA v1

        v3 :: f2 (f1 (f0 Double))
        v3 = sequenceA v2

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize $ V.concatMap vectorize (vectorize values)

    fun :: Fun (J (JV V3)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts
    grid = V.fromList [vectorize grid0, vectorize grid1, vectorize grid2]

interpolant4 ::
  forall f0 f1 f2 f3
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Applicative f1, Traversable f1
    , Vectorize f2, Applicative f2, Traversable f2
    , Vectorize f3
    )
  => String -> String
  -> f0 Double -> f1 Double -> f2 Double -> f3 Double
  -> f0 (f1 (f2 (f3 Double)))
  -> Map String GType
  -> Fun (J (JV V4)) S
interpolant4 name solver grid0 grid1 grid2 grid3 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant4 " ++ name ++ ": " ++ err
  where
    -- transpose values
    values :: f3 (f2 (f1 (f0 Double)))
    values = v6
      where
        v0 :: f0 (f1 (f2 (f3 Double)))
        v0 = values0

        v1 :: f1 (f0 (f2 (f3 Double)))
        v1 = sequenceA v0

        v2 :: f1 (f2 (f0 (f3 Double)))
        v2 = fmap sequenceA v1

        v3 :: f1 (f2 (f3 (f0 Double)))
        v3 = fmap (fmap sequenceA) v2

        v4 :: f2 (f1 (f3 (f0 Double)))
        v4 = sequenceA v3

        v5 :: f2 (f3 (f1 (f0 Double)))
        v5 = fmap sequenceA v4

        v6 :: f3 (f2 (f1 (f0 Double)))
        v6 = sequenceA v5

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize $ V.concatMap vectorize $ V.concatMap vectorize (vectorize values)

    fun :: Fun (J (JV V4)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts
    grid = V.fromList [vectorize grid0, vectorize grid1, vectorize grid2, vectorize grid3]
