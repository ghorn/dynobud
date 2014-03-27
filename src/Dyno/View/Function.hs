{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dyno.View.Function
       ( MXFun
       , SXFun
       , (:*:)(..)
       , toMXFun
       , toExpandedMXFun
       , toSXFun
       , callMXFun
       , callSXFun
       , evalMXFun
       , expandMXFun
       ) where

import Data.Proxy
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dyno.Casadi.MX ( MX, expand )
import Dyno.Casadi.SX ( SX )
import Dyno.Casadi.DMatrix ( DMatrix )
import Dyno.Casadi.MXFunction ( MXFunction, mxFunction, callMX, evalDMatrix )
import Dyno.Casadi.SXFunction ( SXFunction, sxFunction )
import Dyno.Casadi.Option
import Dyno.Casadi.SharedObject

import qualified Casadi.Wrappers.Classes.MXFunction as M
import qualified Casadi.Wrappers.Classes.SharedObject as C

import Dyno.View.Symbolic ( Symbolic(..) )
import Dyno.View.View
import Dyno.View.Viewable ( Viewable )

newtype MXFun (f :: * -> *) (g :: * -> *) = MXFun MXFunction deriving Show
newtype SXFun (f :: * -> *) (g :: * -> *) = SXFun SXFunction deriving Show

infixr 6 :*:
data (:*:) f g a = (:*:) (f a) (g a)

class FunArgs f a where -- | f -> a where
  vectorize :: f a -> Vector a
  devectorize :: Vector a -> f a
  numElems :: Proxy (f a) -> Int

instance (View f, Viewable a) => FunArgs (J f) a where
  vectorize = V.singleton . unJ
  devectorize = mkJ . V.head
  numElems = const 1
  
--instance (View f, Viewable a) => FunArgs f a where
--  vectorize = vectorize . cat
--  devectorize = split . devectorize
--  numElems = const 1

instance (FunArgs f a, FunArgs g a) => FunArgs ((:*:) f g) a where
  vectorize (x :*: y) = vectorize x V.++ vectorize y
  devectorize xy = (devectorize x) :*: (devectorize y)
    where
      x :: Vector a
      y :: Vector a
      (x,y)
        | V.length x' /= nx = error "splitting HList in casadi Fun got length mismatch"
        | V.length y' /= ny = error "splitting HList in casadi Fun got length mismatch"
        | otherwise = (x',y')
      (x',y')= V.splitAt nx xy
      nx = numElems (Proxy :: Proxy (f a))
      ny = numElems (Proxy :: Proxy (g a))
  numElems = const $ numElems (Proxy :: Proxy (f a)) + numElems (Proxy :: Proxy (g a))


class FunArgs f a => SymInputs f a where -- | f -> a where
  sym' :: Int -> Proxy (f a) -> IO (f a, Int)

instance (View f, Symbolic a) => SymInputs (J f) a where
  sym' k = const $ do
    x <- sym ("x" ++ show k)
    return (x,k+1)
--instance (View f, Symbolic a) => SymInputs f a where
--  sym' k0 = const $ do
--    (x,k1) <- sym' k0 (Proxy :: Proxy (J f a))
--    return (split x, k1)

instance (SymInputs f a, SymInputs g a) => SymInputs ((:*:) f g) a where
  sym' k0 = const $ do
    (x,k1) <- sym' k0 (Proxy :: Proxy (f a))
    (y,k2) <- sym' k1 (Proxy :: Proxy (g a))
    return (x :*: y, k2)

-- | make an MXFunction
toMXFun :: forall f g . (SymInputs f MX, FunArgs g MX) => String -> (f MX -> g MX) -> IO (MXFun f g)
toMXFun name fun = do
  (inputs,_) <- sym' 0 (Proxy :: Proxy (f MX))
  mxf <- mxFunction (vectorize inputs) (vectorize (fun inputs))
  setOption mxf "name" name
  soInit mxf
  return (MXFun mxf)

-- | make an MXFunction
toExpandedMXFun :: forall f g . (SymInputs f MX, FunArgs g MX) => String -> (f MX -> g MX) -> IO (MXFun f g)
toExpandedMXFun name fun = do
  (inputs,_) <- sym' 0 (Proxy :: Proxy (f MX))
  let outputs = expand $ vectorize (fun inputs)
  mxf <- mxFunction (vectorize inputs) outputs
  setOption mxf "name" name
  soInit mxf
  return (MXFun mxf)

-- | make an SXFunction
toSXFun :: forall f g . (SymInputs f SX, FunArgs g SX) => String -> (f SX -> g SX) -> IO (SXFun f g)
toSXFun name f = do
  (inputs,_) <- sym' 0 (Proxy :: Proxy (f SX))
  sxf <- sxFunction (vectorize inputs) (vectorize (f inputs))
  setOption sxf "name" name
  soInit sxf
  return (SXFun sxf)

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMXFun :: (FunArgs f MX, FunArgs g MX) => MXFun f g -> f MX -> g MX
callMXFun (MXFun mxf) = devectorize . callMX mxf . vectorize

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callSXFun :: (FunArgs f MX, FunArgs g MX) => SXFun f g -> f MX -> g MX
callSXFun (SXFun mxf) = devectorize . callMX mxf . vectorize

-- | evaluate an MXFunction with 1 input and 1 output
evalMXFun :: (FunArgs f DMatrix, FunArgs g DMatrix) => MXFun f g -> f DMatrix -> IO (g DMatrix)
evalMXFun (MXFun mxf) = fmap devectorize . evalDMatrix mxf . vectorize

-- | expand an MXFunction with 1 input and 1 output
expandMXFun :: MXFun f g -> IO (SXFun f g)
expandMXFun (MXFun mxf) = do
  sxf <- M.mxFunction_expand' mxf
  C.sharedObject_init' sxf
  return (SXFun sxf)
