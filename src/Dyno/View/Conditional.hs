{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.View.Conditional
       ( Conditional(..), toSwitch, toSwitch'
       ) where

import GHC.Generics ( Generic1 )

import qualified Data.Map as M
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
--import Data.Vector ( Vector )

import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.DMatrix ( DMatrix )
import qualified Casadi.CMatrix as C

import Dyno.Vectorize ( Vectorize(..), devectorize, vlength )

newtype Switch f a = Switch a deriving Show

-- todo(greg): short circuit or not?
--
class Conditional a where
  conditional :: (Enum b, Bounded b, Ord b, Show b) => Switch b a -> [(b, a)] -> a
  conditional' :: Vectorize f => Switch f a -> f a -> a

instance Conditional SX where
  conditional = cmatConditional (realToFrac nan)
  conditional' = cmatConditional' (realToFrac nan)
instance Conditional MX where
  conditional = cmatConditional (realToFrac nan)
  conditional' = cmatConditional' (realToFrac nan)
instance Conditional DMatrix where
  conditional = cmatConditional (realToFrac nan)
  conditional' = cmatConditional' (realToFrac nan)

instance Conditional Double where
  conditional = evaluateConditional nan
  conditional' = evaluateConditional' nan
instance Conditional Float where
  conditional = evaluateConditional (realToFrac nan)
  conditional' = evaluateConditional' (realToFrac nan)

nan :: Double
nan = 0

{-# INLINABLE toSwitch #-}
toSwitch ::  forall a b
             . (Enum b, Bounded b, Eq b, Show b, Fractional a)
             => b -> Switch b a
toSwitch key = lookupKey 0 orderedKeys
  where
    orderedKeys :: [b]
    orderedKeys = orderKeys

    lookupKey :: Int -> [b] -> Switch b a
    lookupKey k (x:xs)
      | key == x = Switch (0.0 + (fromIntegral k))
      | otherwise = lookupKey (k+1) xs
    lookupKey _ [] =
      error $
      "toConditionalKey: the \"impossible\" happened! " ++
      "The enum " ++ show key ++ " was not in the set of all enums."

{-# INLINABLE toSwitch' #-}
toSwitch' ::  forall f a
              . (Vectorize f, Fractional a)
              => (f Int -> Int) -> Switch f a
toSwitch' get = Switch index
  where
    index = 0.0 + fromIntegral (get f)

    f :: f Int
    f = devectorize $ V.fromList $ take (vlength (Proxy :: Proxy f)) [0..]

cmatConditional :: forall a b
                   . (Enum b, Ord b, Bounded b, Show b, C.CMatrix a)
                   => a -> Switch b a -> [(b, a)] -> a
cmatConditional def (Switch x) unorderedKeyVals = C.conditional' x (V.fromList orderedVals) def False
  where
    -- The values in their proper order.
    orderedVals :: [a]
    orderedVals = orderVals unorderedKeyVals

cmatConditional' :: forall f a
                    . (Vectorize f, C.CMatrix a)
                    => a -> Switch f a -> f a -> a
cmatConditional' def (Switch x) orderedVals = C.conditional x (vectorize orderedVals) def

orderKeys :: (Enum a, Bounded a) => [a]
orderKeys
  | length keys > 99 = error "conditional: orderKeys got over 99 enums, that is going to be too many"
  | otherwise = keys
  where
    keys = enumFrom minBound

orderVals :: forall a b . (Enum b, Bounded b, Ord b, Show b) => [(b, a)] -> [a]
orderVals unorderedKeyVals = orderedVals
  where
    switchMap :: M.Map b a
    switchMap = M.fromListWithKey err unorderedKeyVals
      where
        err k _ _ = error $ "conditional: orderVals got two entries for " ++ show k

    -- The options in their proper order.
    -- Will show up to casadi as [0,1,2..]
    orderedKeys :: [b]
    orderedKeys = orderKeys

    -- The options in their proper order.
    -- Will show up to casadi as [0,1,2..]
    orderedVals :: [a]
    orderedVals = map lookup' orderedKeys
      where
        lookup' k = case M.lookup k switchMap of
          Just r -> r
          Nothing -> error $ "conditional: missing an entry for " ++ show k

{-# INLINABLE evaluateConditional #-}
evaluateConditional ::
  forall b a
  . (Enum b, Ord b, Bounded b, Show b, RealFrac a)
  => a -> Switch b a -> [(b, a)] -> a
evaluateConditional def (Switch k) pairs = lookupWithDefault (floor k) (orderVals pairs)
  where
    lookupWithDefault :: Int -> [a] -> a
    lookupWithDefault 0 (x:_) = x
    lookupWithDefault j (_:xs) = lookupWithDefault (j - 1) xs
    lookupWithDefault _ [] = def

{-# INLINABLE evaluateConditional' #-}
evaluateConditional' ::
  forall f a
  . (Vectorize f, RealFrac a)
  => a -> Switch f a -> f a -> a
evaluateConditional' def (Switch k) pairs = case vectorize pairs V.!? round k of
  Nothing -> def
  Just r -> r



data Cases a = Cases {_case1 :: a, _case2 :: a} deriving (Functor, Generic1)
instance Vectorize Cases

data Foo = FooA | FooB deriving (Enum, Bounded, Eq, Ord, Show)

-- | a test
-- >>> _test (toSwitch FooA) (Cases 42 43) :: (Switch Foo Double, Double)
-- (Switch 0.0,42.0)
--
-- >>> _test (toSwitch FooB) (Cases 42 43) :: (Switch Foo Double, Double)
-- (Switch 1.0,43.0)
--
-- >>> _test (Switch (-1)) (Cases 42 43) :: (Switch Foo Double, Double)
-- (Switch (-1.0),-100.0)
--
-- >>> _test (toSwitch FooA) (Cases 42 43) :: (Switch Foo DMatrix, DMatrix)
-- (Switch 0,42)
--
-- >>> _test (toSwitch FooB) (Cases 42 43) :: (Switch Foo DMatrix, DMatrix)
-- (Switch 1,43)
--
-- >>> _test (Switch (-1)) (Cases 42 43) :: (Switch Foo DMatrix, DMatrix)
-- (Switch -1,-100)
_test :: Conditional a => Switch Foo a -> Cases a -> (Switch Foo a, a)
_test whichFoo (Cases x y) = (whichFoo, r)
  where
    r = conditional whichFoo
        [ (FooA, x)
        , (FooB, y)
        ]

-- | another test
-- >>> _test' (toSwitch' _case1) (Cases 42 43) :: (Switch Cases Double, Double)
-- (Switch 0.0,42.0)
--
-- >>> _test' (toSwitch' _case2) (Cases 42 43) :: (Switch Cases Double, Double)
-- (Switch 1.0,43.0)
--
-- >>> _test' (Switch (-1)) (Cases 42 43) :: (Switch Cases Double, Double)
-- (Switch (-1.0),-100.0)
--
-- >>> _test' (toSwitch' _case1) (Cases 42 43) :: (Switch Cases DMatrix, DMatrix)
-- (Switch 0,42)
--
-- >>> _test' (toSwitch' _case2) (Cases 42 43) :: (Switch Cases DMatrix, DMatrix)
-- (Switch 1,43)
--
-- >>> _test' (Switch (-1)) (Cases 42 43) :: (Switch Cases DMatrix, DMatrix)
-- (Switch -1,-100)
_test' :: Conditional a => Switch Cases a -> Cases a -> (Switch Cases a, a)
_test' whichCase (Cases x y) = (whichCase, conditional' whichCase (Cases x y))
