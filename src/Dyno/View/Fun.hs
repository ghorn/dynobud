{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dyno.View.Fun
       ( MXFun
       , SXFun
       , Fun
       , (:*:)(..)
       , toMXFun
       , toSXFun
       , toFunJac
       , toFunJac'
       , callFun
       , callMXFun
       , callSXFun
       , callSXFunSX
       , evalMXFun
       , expandMXFun
       ) where

import Data.Proxy
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Data.Vector ( Vector )

import Dyno.Casadi.MX ( symV )
import Dyno.Casadi.Function ( Function, callMX, callSX, evalDMatrix, jacobian )
import Dyno.Casadi.MXFunction ( MXFunction, mxFunction )
import Dyno.Casadi.SXFunction ( SXFunction, sxFunction )
import Dyno.Casadi.Option
import Dyno.Casadi.SharedObject

import qualified Casadi.Wrappers.Classes.MXFunction as M
import qualified Casadi.Wrappers.Classes.SharedObject as C

import Dyno.View.Symbolic ( Symbolic(..) )
import Dyno.View.View
import Dyno.View.Viewable -- ( Viewable )

newtype MXFun (f :: * -> *) (g :: * -> *) = MXFun MXFunction deriving Show
newtype SXFun (f :: * -> *) (g :: * -> *) = SXFun SXFunction deriving Show
newtype Fun (f :: * -> *) (g :: * -> *) = Fun Function deriving Show

infixr 6 :*:
data (:*:) f g a = (:*:) (f a) (g a)

class FunArgs f a where -- | f -> a where
  vectorize :: f a -> Vector a
  devectorize :: Vector a -> f a
  numElems :: Proxy (f a) -> Int
  sizeList ::Int ->  Proxy (f a) -> Seq.Seq Int

instance View f => FunArgs (J f) a where
  vectorize = V.singleton . unsafeUnJ
  devectorize = UnsafeJ . V.head
  numElems = const 1
  sizeList = const . Seq.singleton . (n +)
    where
      n = size (Proxy :: Proxy f)

--instance View f => FunArgs f a where
--  vectorize = V.singleton . unsafeUnJ
--  devectorize = UnsafeJ . V.head
--  numElems = const 1
--  sizeList = const (Seq.singleton (size (Proxy :: Proxy f)))

instance (FunArgs f a, FunArgs g a) => FunArgs (f :*: g) a where
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
  sizeList k0 pxy = xs Seq.>< ys
    where
      xs = sizeList k0 px
      ys = sizeList k1 py
      k1 = case Seq.viewr xs of
        Seq.EmptyR -> k0
        _ Seq.:> k1' -> k1'

      reproxy :: Proxy ((x :*: y) p) -> (Proxy (x p), Proxy (y p))
      reproxy = const (Proxy,Proxy)
      (px, py) = reproxy pxy


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

-- | make an SXFunction
toSXFun :: forall f g . (SymInputs f SX, FunArgs g SX) => String -> (f SX -> g SX) -> IO (SXFun f g)
toSXFun name f = do
  (inputs,_) <- sym' 0 (Proxy :: Proxy (f SX))
  sxf <- sxFunction (vectorize inputs) (vectorize (f inputs))
  setOption sxf "name" name
  soInit sxf
  return (SXFun sxf)

-- | expand an MXFunction
expandMXFun :: MXFun f g -> IO (SXFun f g)
expandMXFun (MXFun mxf) = do
  sxf <- M.mxFunction_expand' mxf
  C.sharedObject_init' sxf
  return (SXFun sxf)

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMXFun :: (FunArgs f MX, FunArgs g MX) => MXFun f g -> f MX -> g MX
callMXFun (MXFun mxf) = devectorize . callMX mxf . vectorize

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callFun :: (FunArgs f MX, FunArgs g MX) => Fun f g -> f MX -> g MX
callFun (Fun mxf) = devectorize . callMX mxf . vectorize

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSXFun :: (FunArgs f MX, FunArgs g MX) => SXFun f g -> f MX -> g MX
callSXFun (SXFun mxf) = devectorize . callMX mxf . vectorize

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callSXFunSX :: (FunArgs f SX, FunArgs g SX) => SXFun f g -> f SX -> g SX
callSXFunSX (SXFun sxf) = devectorize . callSX sxf . vectorize

-- | evaluate an MXFunction
evalMXFun :: (FunArgs f DMatrix, FunArgs g DMatrix) => MXFun f g -> f DMatrix -> IO (g DMatrix)
evalMXFun (MXFun mxf) = fmap devectorize . evalDMatrix mxf . vectorize



toFunJac ::
  forall x y f g . (SymInputs x MX, SymInputs y MX, FunArgs f MX, FunArgs g MX, FunArgs f (J x MX))
  => String -> ((x MX, y MX) -> (f MX, g MX)) -> IO ((x MX, y MX) -> (Vector (Vector MX), f MX, g MX))
toFunJac name f0 = do
  (diffInputs',_) <- sym' 0 (Proxy :: Proxy (x MX))
  let nsyms = F.sum $ fmap vsize1 (vectorize diffInputs')
  diffInputsCat <- symV "dx" nsyms
  let inputSizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (x MX))))
      diffInputs = vvertsplit diffInputsCat inputSizes

  (inputs,_) <- sym' 0 (Proxy :: Proxy (y MX))
  let (diffOutputs, outputs) = f0 (devectorize diffInputs, inputs)
      diffOutputsCat = vveccat (vectorize diffOutputs)

      allInputs = V.cons diffInputsCat (vectorize inputs)
      allOutputs = V.cons diffOutputsCat (vectorize outputs)

  mxf <- mxFunction allInputs allOutputs
  setOption mxf "name" name
  soInit mxf
  let compact = False
      symmetric = False
  mxfJac <- jacobian mxf 0 0 compact symmetric
  soInit mxfJac

  let callMe :: (x MX, y MX) -> (Vector (Vector MX), f MX, g MX)
      callMe (x',y')
        | 2 + ng /= V.length vouts =
          error $ "toFunJac: bad number of outputs :("
        | ng /= V.length g = error "toFunJac: g: bad split"
        | otherwise = (rows, devectorize fs, devectorize g)
        where
          --retJac :: f (J x MX)
          --retJac = devectorize retJac'
          --retJac' :: Vector (J x MX)
          --retJac' = fmap devectorize rows
          rows :: Vector (Vector MX)
          rows = fmap (flip vhorzsplit horzsizes) $ vvertsplit jac vertsizes
          vertsizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (f MX))))
          horzsizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (x MX))))

          fs = vvertsplit f vertsizes
          x = vveccat (vectorize x')

          jac = vouts V.! 0
          f = vouts V.! 1
          g = V.drop 2 vouts

          ng = numElems (Proxy :: Proxy (g MX))
          vouts = callMX mxfJac $ V.cons x (vectorize y')

  return callMe



toFunJac' ::
  forall x y f . (SymInputs x MX, SymInputs y MX, FunArgs f MX, FunArgs f (J x MX))
  => String -> ((x MX, y MX) -> f MX) -> IO ((x MX, y MX) -> Vector (Vector MX))
toFunJac' name f0 = do
  (diffInputs',_) <- sym' 0 (Proxy :: Proxy (x MX))
  let nsyms = F.sum $ fmap vsize1 (vectorize diffInputs')
  diffInputsCat <- symV "dx" nsyms
  let inputSizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (x MX))))
      diffInputs = vvertsplit diffInputsCat inputSizes

  (inputs,_) <- sym' 0 (Proxy :: Proxy (y MX))
  let diffOutputs = f0 (devectorize diffInputs, inputs)
      diffOutputsCat = vveccat (vectorize diffOutputs)

      allInputs = V.cons diffInputsCat (vectorize inputs)
      allOutputs = V.singleton diffOutputsCat

  mxf <- mxFunction allInputs allOutputs
  setOption mxf "name" name
  soInit mxf
  let compact = False
      symmetric = False
  mxfJac <- jacobian mxf 0 0 compact symmetric
  soInit mxfJac

  let callMe :: (x MX, y MX) -> (Vector (Vector MX)) -- , f MX)
      callMe (x',y')
        | 2 /= V.length vouts =
          error $ "toFunJac': bad number of outputs :("
        | otherwise = (rows) -- , devectorize fs)
        where
          --retJac :: f (J x MX)
          --retJac = devectorize retJac'
          --retJac' :: Vector (J x MX)
          --retJac' = fmap devectorize rows
          rows :: Vector (Vector MX)
          rows = fmap (flip vhorzsplit horzsizes) $ vvertsplit jac vertsizes
          vertsizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (f MX))))
          horzsizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (x MX))))

          --fs = vvertsplit f vertsizes
          x = vveccat (vectorize x')

          jac = vouts V.! 0
          --f = vouts V.! 1

          vouts = callMX mxfJac $ V.cons x (vectorize y')

  return callMe
