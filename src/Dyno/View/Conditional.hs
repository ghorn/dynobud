{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-cse #-} -- unsafePerformIO
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dyno.View.Conditional
       ( Conditional(..), Switch(..), toSwitch, fromSwitch
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

import Dyno.Vectorize ( Vectorize )
import Dyno.View.Fun ( Fun, toMXFun, toSXFun, callMX, callSX )
import Dyno.View.View ( J, JV, S )
import Dyno.View.Unsafe ( mkM', unM )
import Dyno.View.M ( vcat, vsplit )

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


class Conditional a where
  conditional :: (Enum b, Bounded b, Show b, Vectorize f, Vectorize g)
                 => Bool -> g a -> Switch b a -> f a -> (b -> f a -> g a) -> g a

instance Conditional (S SX) where
  conditional = sxConditional
  {-# NOINLINE conditional #-}
instance Conditional (S MX) where
  conditional = mxConditional
  {-# NOINLINE conditional #-}
instance Conditional (S DM) where
  conditional = dmConditional

instance Conditional Double where
  conditional = \_ _ -> evaluateConditionalNative
instance Conditional Float where
  conditional = \_ _ -> evaluateConditionalNative

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


{-# NOINLINE mxConditional #-}
mxConditional ::
  forall f g b
  . (Enum b, Bounded b, Show b, Vectorize f, Vectorize g)
  => Bool -> g (S MX) -> Switch b (S MX) -> f (S MX) -> (b -> f (S MX) -> g (S MX)) -> g (S MX)
mxConditional shortCircuit def (UnsafeSwitch sw) input handleAnyCase = unsafePerformIO $ do
  let toFunction :: b -> IO (Fun (J (JV f)) (J (JV g)))
      toFunction key = toMXFun ("conditional_" ++ show key) (vcat . handleAnyCase key . vsplit)

  functions <- mapM toFunction orderKeys :: IO [Fun (J (JV f)) (J (JV g))]
  let catInput = vcat input

      outputs :: [J (JV g) MX]
      outputs = map (\f -> callMX f catInput) functions

      output :: MX
      output = C.conditional' (unM sw) (V.fromList (map unM outputs)) (unM (vcat def)) shortCircuit

  case mkM' output of
    Right r -> return (vsplit r)
    Left err -> error $ "cmatConditional: error splitting the output:\n" ++ err

{-# NOINLINE sxConditional #-}
sxConditional ::
  forall f g b
  . (Enum b, Bounded b, Show b, Vectorize f, Vectorize g)
  => Bool -> g (S SX) -> Switch b (S SX) -> f (S SX) -> (b -> f (S SX) -> g (S SX)) -> g (S SX)
sxConditional shortCircuit def (UnsafeSwitch sw) input handleAnyCase = unsafePerformIO $ do
  let toFunction :: b -> IO (Fun (J (JV f)) (J (JV g)))
      toFunction key = toSXFun ("conditional_" ++ show key) (vcat . handleAnyCase key . vsplit)

  functions <- mapM toFunction orderKeys :: IO [Fun (J (JV f)) (J (JV g))]
  let catInput = vcat input

      outputs :: [J (JV g) SX]
      outputs = map (\f -> callSX f catInput) functions

      output :: SX
      output = C.conditional' (unM sw) (V.fromList (map unM outputs)) (unM (vcat def)) shortCircuit

  case mkM' output of
    Right r -> return (vsplit r)
    Left err -> error $ "cmatConditional: error splitting the output:\n" ++ err

dmConditional ::
  forall f g b
  . (Enum b, Bounded b, Vectorize f, Vectorize g)
  => Bool -> g (S DM) -> Switch b (S DM)
  -> f (S DM) -> (b -> f (S DM) -> g (S DM)) -> g (S DM)
dmConditional shortCircuit def (UnsafeSwitch sw) input handleAnyCase =
  case mkM' output of
    Right r -> vsplit r
    Left err -> error $ "cmatConditional: error splitting the output:\n" ++ err
  where
    outputs :: [J (JV g) DM]
    outputs = map (\key -> vcat (handleAnyCase key input)) orderKeys

    output :: DM
    output = C.conditional' (unM sw) (V.fromList (map unM outputs)) (unM (vcat def)) shortCircuit

orderKeys :: (Enum a, Bounded a) => [a]
orderKeys
  | length keys > 99 = error "conditional: orderKeys got over 99 enums, that is going to be too many"
  | otherwise = keys
  where
    keys = enumFrom minBound

{-# INLINABLE evaluateConditionalNative #-}
evaluateConditionalNative ::
  forall f g a b
  . (Enum b, Bounded b, RealFrac a)
  => Switch b a -> f a -> (b -> f a -> g a) -> g a
evaluateConditionalNative sw input handleAnyCase = handleAnyCase enum input
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
_test foo input = (foo, conditional True def foo input f)
  where
    def = V2 (-1) (-2)
    f :: Foo -> V3 a -> V2 a
    f sw (V3 x y z) = case sw of
      FooA -> V2 x (2*x)
      FooB -> V2 y (2*y)
      FooC -> V2 z (2*z)
