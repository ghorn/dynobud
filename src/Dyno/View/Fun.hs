{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Dyno.View.Fun
       ( MXFun
       , SXFun
       , Fun
       , toMXFun
       , toSXFun
       , callFun
       , callMXFun
       , callSXFun
       , callSXFunSX
       , evalMXFun
       , evalSXFun
       , expandMXFun
       , toFunJac
       ) where

import Control.Monad ( zipWithM )
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector ( Vector )

import Dyno.Casadi.MX ( symM )
import Dyno.Casadi.SX ( ssymM )
import Dyno.Casadi.Function ( Function, callMX, callSX, evalDMatrix, jacobian )
import Dyno.Casadi.MXFunction ( MXFunction, mxFunction )
import Dyno.Casadi.SXFunction ( SXFunction, sxFunction )
import Dyno.Casadi.Option
import Dyno.Casadi.SharedObject

import qualified Casadi.Core.Classes.MXFunction as M
import qualified Casadi.Core.Classes.SharedObject as C
import qualified Casadi.Core.Classes.OptionsFunctionality as C

import Dyno.View.CasadiMat
import Dyno.View.Scheme
import Dyno.View.FunJac

newtype MXFun (f :: * -> *) (g :: * -> *) = MXFun MXFunction deriving Show
newtype SXFun (f :: * -> *) (g :: * -> *) = SXFun SXFunction deriving Show
newtype Fun (f :: * -> *) (g :: * -> *) = Fun Function deriving Show

mkSym :: forall a f .
         (Scheme f, CasadiMat a)
         => (String -> Int -> Int -> IO a)
         -> String -> Proxy f -> IO (f a)
mkSym mk name _ = do
  let sizes :: [(Int,Int)]
      sizes = sizeList (Proxy :: Proxy f)
      f :: (Int, Int) -> Int -> IO a
      f (nrow,ncol) k = mk (name ++ show k) nrow ncol
  ms <- zipWithM f sizes [(0::Int)..]
  return $ fromVector (V.fromList ms)

mkFun :: forall f g fun fun' a
         . (Scheme f, Scheme g, C.SharedObjectClass fun, C.OptionsFunctionalityClass fun)
         => (Vector a -> Vector a -> IO fun)
         -> (String -> Proxy f -> IO (f a))
         -> (fun -> fun' f g)
         -> String
         -> (f a -> g a)
         -> IO (fun' f g)
mkFun mkfun mksym con name userf = do
  inputs <- mksym "x" (Proxy :: Proxy f)
  fun <- mkfun (toVector inputs) (toVector (userf inputs))
  setOption fun "name" name
  soInit fun
  return (con fun)


-- | make an MXFunction
toMXFun :: forall f g . (Scheme f, Scheme g) => String -> (f MX -> g MX) -> IO (MXFun f g)
toMXFun name fun = mkFun mxFunction (mkSym symM) MXFun name fun

-- | make an MXFunction
toSXFun :: forall f g . (Scheme f, Scheme g) => String -> (f SX -> g SX) -> IO (SXFun f g)
toSXFun name fun = mkFun sxFunction (mkSym ssymM) SXFun name fun

-- | expand an MXFunction
expandMXFun :: MXFun f g -> IO (SXFun f g)
expandMXFun (MXFun mxf) = do
  sxf <- M.mxFunction_expand__0 mxf
  C.sharedObject_init__0 sxf
  return (SXFun sxf)

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMXFun :: (Scheme f, Scheme g) => MXFun f g -> f MX -> g MX
callMXFun (MXFun mxf) = fromVector . callMX mxf . toVector

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callFun :: (Scheme f, Scheme g) => Fun f g -> f MX -> g MX
callFun (Fun mxf) = fromVector . callMX mxf . toVector

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSXFun :: (Scheme f, Scheme g) => SXFun f g -> f MX -> g MX
callSXFun (SXFun mxf) = fromVector . callMX mxf . toVector

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callSXFunSX :: (Scheme f, Scheme g) => SXFun f g -> f SX -> g SX
callSXFunSX (SXFun sxf) = fromVector . callSX sxf . toVector

-- | evaluate an MXFunction
evalMXFun :: (Scheme f, Scheme g) => MXFun f g -> f DMatrix -> IO (g DMatrix)
evalMXFun (MXFun mxf) = fmap fromVector . evalDMatrix mxf . toVector

-- | evaluate an SXFunction
evalSXFun :: (Scheme f, Scheme g) => SXFun f g -> f DMatrix -> IO (g DMatrix)
evalSXFun (SXFun sxf) = fmap fromVector . evalDMatrix sxf . toVector

-- | make a function which also contains a jacobian
toFunJac :: Fun (JacIn xj x) (JacOut fj f) -> IO (Fun (JacIn xj x) (Jac xj fj f))
toFunJac (Fun fun) = do
  maybeName <- getOption fun "name"
  let name = case maybeName of Nothing -> "no_name"
                               Just n -> n
  let compact = False
      symmetric = False
  funJac <- jacobian fun 0 0 compact symmetric
  setOption funJac "name" (name ++ "_dynobudJac")
  soInit funJac

  return (Fun funJac)


--toFunJac' ::
--  forall x y f . (SymInputs x MX, SymInputs y MX, FunArgs f MX, FunArgs f (J x MX))
--  => String -> ((x MX, y MX) -> f MX) -> IO ((x MX, y MX) -> Vector (Vector MX))
--toFunJac' name f0 = do
--  (diffInputs',_) <- sym' 0 (Proxy :: Proxy (x MX))
--  let nsyms = F.sum $ fmap vsize1 (vectorize diffInputs')
--  diffInputsCat <- symV "dx" nsyms
--  let inputSizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (x MX))))
--      diffInputs = vvertsplit diffInputsCat inputSizes
--
--  (inputs,_) <- sym' 0 (Proxy :: Proxy (y MX))
--  let diffOutputs = f0 (devectorize diffInputs, inputs)
--      diffOutputsCat = vveccat (vectorize diffOutputs)
--
--      allInputs = V.cons diffInputsCat (vectorize inputs)
--      allOutputs = V.singleton diffOutputsCat
--
--  mxf <- mxFunction allInputs allOutputs
--  setOption mxf "name" name
--  soInit mxf
--  let compact = False
--      symmetric = False
--  mxfJac <- jacobian mxf 0 0 compact symmetric
--  soInit mxfJac
--
--  let callMe :: (x MX, y MX) -> Vector (Vector MX) -- , f MX)
--      callMe (x',y')
--        | 2 /= V.length vouts =
--          error "toFunJac': bad number of outputs :("
--        | otherwise = rows -- , devectorize fs)
--        where
--          --retJac :: f (J x MX)
--          --retJac = devectorize retJac'
--          --retJac' :: Vector (J x MX)
--          --retJac' = fmap devectorize rows
--          rows :: Vector (Vector MX)
--          rows = fmap (`vhorzsplit` horzsizes) $ vvertsplit jac vertsizes
--          vertsizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (f MX))))
--          horzsizes = V.fromList ((0:) $ F.toList (sizeList 0 (Proxy :: Proxy (x MX))))
--
--          --fs = vvertsplit f vertsizes
--          x = vveccat (vectorize x')
--
--          jac = vouts V.! 0
--          --f = vouts V.! 1
--
--          vouts = callMX mxfJac $ V.cons x (vectorize y')
-- 
--  return callMe
