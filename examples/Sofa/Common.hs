{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language PolyKinds #-}

module Sofa.Common
       ( SofaMessage(..)
       , Point(..)
       , url
       , sofaChannel
       , zipWithNext
       , zipWithNext'
       , cross
       , norm2
       ) where

import GHC.Generics ( Generic, Generic1 )

import qualified Data.Foldable as F
import Data.Serialize

import Dyno.TypeVecs ( Vec, Dim )
import qualified Dyno.TypeVecs as TV
import Dyno.Vectorize


data Point a = Point a a deriving (Functor, Generic, Generic1, Show)

instance Num a => Num (Point a) where
  Point x0 y0 + Point x1 y1 = Point (x0 + x1) (y0 + y1)
  Point x0 y0 - Point x1 y1 = Point (x0 - x1) (y0 - y1)
  Point x0 y0 * Point x1 y1 = Point (x0 * x1) (y0 * y1)
  abs = fmap abs
  signum = fmap signum
  fromInteger k' = Point k k
    where
      k = fromInteger k'

instance Fractional a => Fractional (Point a) where
  Point x0 y0 / Point x1 y1 = Point (x0 / x1) (y0 / y1)
  fromRational k' = Point k k
    where
      k = fromRational k'
  
instance Vectorize Point

data SofaMessage =
  SofaMessage
  { smIters :: Int
  , smSegmentLength :: Double
  , smPoints :: [Point Double]
  , smMeanThetas :: [(Point Double, Double)]
  } deriving Generic

instance Serialize SofaMessage
instance Serialize a => Serialize (Point a)

url :: String
url = "tcp://127.0.0.1:5563"

sofaChannel :: String
sofaChannel = "sofa_telemetry"


cross :: Num a => Point a -> Point a -> a
cross (Point x0 y0) (Point x1 y1) = x0*y1 - x1*y0

norm2s :: Num a => Point a -> a
norm2s (Point x y) = x*x + y*y

norm2 :: Floating a => Point a -> a
norm2 = sqrt . norm2s

zipWithNext :: Dim n => (a -> a -> b) -> Vec n a -> Vec n b
zipWithNext f v = TV.mkVec' $ diff' (v' ++ [v0])
  where
    diff' (x0:theRest@(x1:_)) = f x0 x1 : diff' theRest
    diff' _ = []

    v'@(v0:_) = F.toList v

zipWithNext' :: (a -> a -> b) -> [a] -> [b]
zipWithNext' f v'@(v0:_) = diff' (v' ++ [v0])
  where
    diff' (x0:theRest@(x1:_)) = f x0 x1 : diff' theRest
    diff' _ = []
zipWithNext' _ [] = []
