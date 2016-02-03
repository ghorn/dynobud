{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-cse #-} -- unsafePerformIO
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Dyno.View.Conditional
       ( Conditional(..), Switch, toSwitch
       ) where

import GHC.Generics ( Generic, Generic1 )

import Accessors ( Lookup )
import Data.Aeson ( ToJSON, FromJSON )
import Data.Serialize ( Serialize )
import qualified Data.Vector as V
import Linear ( V2(..), V3(..) )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.DMatrix ( DMatrix )
import qualified Casadi.CMatrix as C

import Dyno.Vectorize ( Vectorize )
import Dyno.View.Fun ( MXFun, SXFun, toMXFun, toSXFun, call, callSX )
import Dyno.View.View ( J, JV, S )
import Dyno.View.Unsafe ( mkM', unM )
import Dyno.View.M ( vcat, vsplit )

newtype Switch f a = Switch a deriving (Functor, Generic, Generic1)
instance Vectorize (Switch f)
instance Lookup a => Lookup (Switch f a)
instance Serialize a => Serialize (Switch f a)
instance ToJSON a => ToJSON (Switch f a)
instance FromJSON a => FromJSON (Switch f a)

class Conditional a where
  conditional :: (Enum b, Bounded b, Ord b, Show b, Vectorize f, Vectorize g)
                 => Bool -> g a -> Switch b a -> (b -> f a -> g a) -> f a -> g a

instance Conditional (S SX) where
  conditional = sxConditional
  {-# NOINLINE conditional #-}
instance Conditional (S MX) where
  conditional = mxConditional
  {-# NOINLINE conditional #-}
instance Conditional (S DMatrix) where
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
      | key == x = Switch (fromIntegral k)
      | otherwise = lookupKey (k+1) xs
    lookupKey _ [] =
      error $
      "toSwitch: the \"impossible\" happened! " ++
      "The enum " ++ show key ++ " was not in the set of all enums."


{-# NOINLINE mxConditional #-}
mxConditional ::
  forall f g b
  . (Enum b, Eq b, Bounded b, Show b, Vectorize f, Vectorize g)
  => Bool -> g (S MX) -> Switch b (S MX) -> (b -> f (S MX) -> g (S MX)) -> f (S MX) -> g (S MX)
mxConditional shortCircuit def (Switch sw) handleAnyCase input = unsafePerformIO $ do
  let toFunction :: b -> IO (MXFun (J (JV f)) (J (JV g)))
      toFunction key = toMXFun ("conditional_" ++ show key) (vcat . handleAnyCase key . vsplit)

  functions <- mapM toFunction orderKeys :: IO [MXFun (J (JV f)) (J (JV g))]
  let catInput = vcat input

      outputs :: [J (JV g) MX]
      outputs = map (\f -> call f catInput) functions

      output :: MX
      output = C.conditional' (unM sw) (V.fromList (map unM outputs)) (unM (vcat def)) shortCircuit

  case mkM' output of
    Right r -> return (vsplit r)
    Left err -> error $ "cmatConditional: error splitting the output:\n" ++ err

{-# NOINLINE sxConditional #-}
sxConditional ::
  forall f g b
  . (Enum b, Eq b, Bounded b, Show b, Vectorize f, Vectorize g)
  => Bool -> g (S SX) -> Switch b (S SX) -> (b -> f (S SX) -> g (S SX)) -> f (S SX) -> g (S SX)
sxConditional shortCircuit def (Switch sw) handleAnyCase input = unsafePerformIO $ do
  let toFunction :: b -> IO (SXFun (J (JV f)) (J (JV g)))
      toFunction key = toSXFun ("conditional_" ++ show key) (vcat . handleAnyCase key . vsplit)

  functions <- mapM toFunction orderKeys :: IO [SXFun (J (JV f)) (J (JV g))]
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
  . (Enum b, Eq b, Bounded b, Show b, Vectorize f, Vectorize g)
  => Bool -> g (S DMatrix) -> Switch b (S DMatrix)
  -> (b -> f (S DMatrix) -> g (S DMatrix)) -> f (S DMatrix) -> g (S DMatrix)
dmConditional shortCircuit def (Switch sw) handleAnyCase input =
  case mkM' output of
    Right r -> vsplit r
    Left err -> error $ "cmatConditional: error splitting the output:\n" ++ err
  where
    outputs :: [J (JV g) DMatrix]
    outputs = map (\key -> vcat (handleAnyCase key input)) orderKeys

    output :: DMatrix
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
  . (Enum b, Eq b, Bounded b, Show b, RealFrac a)
  => Switch b a -> (b -> f a -> g a) -> f a -> g a
evaluateConditionalNative (Switch key) handleAnyCase = handleAnyCase (fromSwitch (round key))
  where
    fromSwitch intKey
      | intKey < 0 = error $ "evaluateConditionalNative: fromSwitch: key " ++ show intKey ++ " is negative"
      | otherwise = lookupKey intKey orderKeys
      where
        lookupKey :: Int -> [b] -> b
        lookupKey 0 (x:_) = x
        lookupKey k (_:xs) = lookupKey (k-1) xs
        lookupKey _ [] =
          error $
          "evaluateConditionalNative: fromSwitch: " ++
          "The key " ++ show intKey ++ " was not in the set of all enums."


data Foo = FooA | FooB | FooC deriving (Enum, Bounded, Eq, Ord, Show)
-- | a test
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo Double, V2 Double)
-- (Switch 0.0,V2 1.0 2.0)
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo Double, V2 Double)
-- (Switch 1.0,V2 2.0 4.0)
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo Double, V2 Double)
-- (Switch 2.0,V2 3.0 6.0)
--
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo (S DMatrix), V2 (S DMatrix))
-- (Switch 0,V2 1 2)
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo (S DMatrix), V2 (S DMatrix))
-- (Switch 1,V2 2 4)
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo (S DMatrix), V2 (S DMatrix))
-- (Switch 2,V2 3 6)
--
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo (S SX), V2 (S SX))
-- (Switch 0,V2 1 2)
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo (S SX), V2 (S SX))
-- (Switch 1,V2 2 4)
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo (S SX), V2 (S SX))
-- (Switch 2,V2 3 6)
--
-- >>> _test (toSwitch FooA) (V3 1 2 3) :: (Switch Foo (S MX), V2 (S MX))
-- (Switch 0,V2 vertsplit(conditional(0){0}){0} vertsplit(conditional(0){0}){1})
--
-- >>> _test (toSwitch FooB) (V3 1 2 3) :: (Switch Foo (S MX), V2 (S MX))
-- (Switch 1,V2 vertsplit(conditional(1){0}){0} vertsplit(conditional(1){0}){1})
--
-- >>> _test (toSwitch FooC) (V3 1 2 3) :: (Switch Foo (S MX), V2 (S MX))
-- (Switch 2,V2 vertsplit(conditional(2){0}){0} vertsplit(conditional(2){0}){1})
--
_test :: forall a
         . (Conditional a, Num a)
         => Switch Foo a -> V3 a -> (Switch Foo a, V2 a)
_test foo input = (foo, conditional True def foo f input)
  where
    def = V2 (-1) (-2)
    f :: Foo -> V3 a -> V2 a
    f sw (V3 x y z) = case sw of
      FooA -> V2 x (2*x)
      FooB -> V2 y (2*y)
      FooC -> V2 z (2*z)
