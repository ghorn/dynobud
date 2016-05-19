{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Dyno.View.Conditional
       ( Conditional(..), Conditional'(..), Switch(..), toSwitch, fromSwitch
       , functionConditional
       ) where

import GHC.Generics ( Generic, Generic1 )

import Accessors ( Lookup(..) )
import Control.Lens ( Lens' )
import Data.Aeson ( ToJSON, FromJSON )
import Data.Serialize ( Serialize )
import qualified Data.Vector as V
import Linear ( V2(..), V3(..) )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.DM ( DM )
import qualified Casadi.CMatrix as C
import Casadi.Core.Classes.Function as C

import Dyno.Vectorize ( Vectorize )
import Dyno.View.Fun ( Fun(..), checkFunDimensionsWith )
import Dyno.View.HList ( (:*:) )
import Dyno.View.M ( M, vcat, vsplit )
import Dyno.View.Scheme ( Scheme(..) )
import Dyno.View.Unsafe ( mkM', unM )
import Dyno.View.View ( View, S, J, JV )

newtype Switch f a = UnsafeSwitch a deriving (Functor, Generic, Generic1, Show)
instance Vectorize (Switch f)
instance Serialize a => Serialize (Switch f a)
instance ToJSON a => ToJSON (Switch f a)
instance FromJSON a => FromJSON (Switch f a)

instance (Bounded f, Enum f, Eq f, Show f, Lookup f, RealFrac a) => Lookup (Switch f a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . swLens)
      where
        swLens :: Lens' (Switch f a) f
        swLens g x = fmap toSwitch (g (fromSwitch' x))

        fromSwitch' x = case fromSwitch x of
          Right r -> r
          Left err -> error $ "error: toAccessorTree (Switch f a): " ++ err

-- | Switches over Vectorize
class Conditional a where
  conditional :: (Enum b, Bounded b, Show b, Vectorize f)
                 => Bool -> Switch b a -> (b -> f a) -> f a

instance Conditional (S SX) where
  conditional = cmConditional

instance Conditional (S MX) where
  conditional = cmConditional

instance Conditional (S DM) where
  conditional = cmConditional

instance Conditional Double where
  conditional = \_ -> evaluateConditionalNative
instance Conditional Float where
  conditional = \_ -> evaluateConditionalNative

-- | Switches over View
class Conditional' a where
  conditional' :: (Enum b, Bounded b, Show b, View f0, View f1)
                  => Bool -> Switch b (S a) -> (b -> M f0 f1 a) -> M f0 f1 a

instance Conditional' SX where
  conditional' = cmConditional'
instance Conditional' MX where
  conditional' = cmConditional'
instance Conditional' DM where
  conditional' = cmConditional'


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
      | key == x = UnsafeSwitch (fromIntegral k)
      | otherwise = lookupKey (k+1) xs
    lookupKey _ [] =
      error $
      "toSwitch: the \"impossible\" happened! " ++
      "The enum " ++ show key ++ " was not in the set of all enums."


{-# INLINABLE fromSwitch #-}
fromSwitch ::  forall a b
               . (Enum b, Bounded b, RealFrac a)
               => Switch b a -> Either String b
fromSwitch (UnsafeSwitch index) = lookupKey (round index)
  where
    lookupKey intKey
      | intKey < 0 = Left $ "fromSwitch: got negative index: " ++ show intKey
      | otherwise = lookupKey' intKey orderKeys

    lookupKey' :: Int -> [b] -> Either String b
    lookupKey' 0 (x:_) = Right x
    lookupKey' k (_:xs) = lookupKey' (k-1) xs
    lookupKey' _ [] =
      Left $
      "fromSwitch: " ++
      "The index " ++ show (round index :: Int) ++ " didn't map to an enum."


cmConditional ::
  forall f b a
  . (Enum b, Bounded b, Show b, Vectorize f, C.CMatrix a)
  => Bool -> Switch b (S a) -> (b -> f (S a)) -> f (S a)
cmConditional shortCircuit sw handleAnyCase =
  vsplit $ cmConditional' shortCircuit sw (vcat . handleAnyCase)


cmConditional' ::
  forall f0 f1 b a
  . (Enum b, Bounded b, Show b, View f0, View f1, C.CMatrix a)
  => Bool -> Switch b (S a) -> (b -> M f0 f1 a) -> M f0 f1 a
cmConditional' shortCircuit (UnsafeSwitch sw) handleAnyCase =
  case mkM' output of
    Right r -> r
    Left err -> error $ "cmatConditional: error splitting the output:\n" ++ err
  where
    -- last output is the default case
    allButLastOutputs :: [M f0 f1 a]
    lastOutput :: M f0 f1 a
    (allButLastOutputs, lastOutput) = case reverse (map handleAnyCase orderKeys) of
      (lastOutput':reversedOutputs) -> (reverse reversedOutputs, lastOutput')
      [] -> error "conditional needs at least one argument"

    output :: a
    output = C.conditional' (unM sw) (V.fromList (map unM allButLastOutputs)) (unM lastOutput) shortCircuit

{-# NOINLINE functionConditional #-}
functionConditional ::
  forall f g b
  . (Enum b, Bounded b, Show b, Scheme f, Scheme g)
  => (b -> Fun f g) -> Fun (J (JV (Switch b)) :*: f) g
functionConditional handleAnyCase = unsafePerformIO $ do
  let functions = map handleAnyCase orderKeys :: [Fun f g]

      -- last output is the default case
      allButLastFunctions :: [Fun f g]
      lastFunction :: Fun f g
      (allButLastFunctions, lastFunction) = case reverse functions of
        (lastFunction':reversedFunctions) -> (reverse reversedFunctions, lastFunction')
        [] -> error "conditional needs at least one argument"

  switchFun <- C.function_conditional__0 "function_conditional"
    (V.fromList (fmap unFun allButLastFunctions)) (unFun lastFunction)

  checkFunDimensionsWith "functionConditional" (Fun switchFun)

orderKeys :: (Enum a, Bounded a) => [a]
orderKeys
  | length keys > 99 = error "conditional: orderKeys got over 99 enums, that is going to be too many"
  | otherwise = keys
  where
    keys = enumFrom minBound

{-# INLINABLE evaluateConditionalNative #-}
evaluateConditionalNative ::
  forall f a b
  . (Enum b, Bounded b, RealFrac a)
  => Switch b a -> (b -> f a) -> f a
evaluateConditionalNative sw handleAnyCase = handleAnyCase enum
  where
    enum = case fromSwitch sw of
      Right r -> r
      Left err -> error $ "error: evaluateConditionalNative: " ++ err


data Foo = FooA | FooB | FooC deriving (Enum, Bounded, Eq, Ord, Show)
-- | a test
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo Double, V2 Double)
-- (UnsafeSwitch 0.0,V2 1.0 2.0)
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo Double, V2 Double)
-- (UnsafeSwitch 1.0,V2 2.0 4.0)
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo Double, V2 Double)
-- (UnsafeSwitch 2.0,V2 3.0 6.0)
--
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo (S DM), V2 (S DM))
-- (UnsafeSwitch 0,V2 1 2)
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo (S DM), V2 (S DM))
-- (UnsafeSwitch 1,V2 2 4)
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo (S DM), V2 (S DM))
-- (UnsafeSwitch 2,V2 3 6)
--
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo (S SX), V2 (S SX))
-- (UnsafeSwitch 0,V2 1 2)
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo (S SX), V2 (S SX))
-- (UnsafeSwitch 1,V2 2 4)
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo (S SX), V2 (S SX))
-- (UnsafeSwitch 2,V2 3 6)
--
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo (S MX), V2 (S MX))
-- (UnsafeSwitch 0,V2 vertsplit(switch(0){0}){0} vertsplit(switch(0){0}){1})
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo (S MX), V2 (S MX))
-- (UnsafeSwitch 1,V2 vertsplit(switch(1){0}){0} vertsplit(switch(1){0}){1})
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo (S MX), V2 (S MX))
-- (UnsafeSwitch 2,V2 vertsplit(switch(2){0}){0} vertsplit(switch(2){0}){1})
--
_test :: forall a
         . (Conditional a, Num a)
         => Switch Foo a -> V3 a -> (Switch Foo a, V2 a)
_test sw (V3 x y z) = (sw, conditional True sw f)
  where
    f :: Foo -> V2 a
    f FooA = V2 x (2*x)
    f FooB = V2 y (2*y)
    f FooC = V2 z (2*z)
