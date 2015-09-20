{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Dyno.View.Fun
       ( FunClass(..)
       , AlwaysInline(..)
       , NeverInline(..)
       , MXFun
       , SXFun
       , Fun(..)
       , toMXFun
       , toMXFun'
       , toSXFun
       , toSXFun'
       , eval
       , call
       , callSX
       , expandMXFun
       , toFunJac
       ) where

import Control.Monad ( zipWithM )
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector ( Vector )

import Casadi.MX ( symM )
import Casadi.SX ( ssymM )
import Casadi.Function ( AlwaysInline(..), NeverInline(..) )
import qualified Casadi.Function as C
import qualified Casadi.MXFunction as C
import qualified Casadi.SXFunction as C
import Casadi.Option
import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.DMatrix ( DMatrix )
import Casadi.CMatrix ( CMatrix )

import qualified Casadi.Core.Classes.Function as F
import qualified Casadi.Core.Classes.MXFunction as M
import qualified Casadi.Core.Classes.SharedObject as C
import qualified Casadi.Core.Classes.OptionsFunctionality as C

import Dyno.View.Viewable ( Viewable )
import Dyno.View.Scheme
import Dyno.View.FunJac

newtype MXFun (f :: * -> *) (g :: * -> *) = MXFun C.MXFunction
newtype SXFun (f :: * -> *) (g :: * -> *) = SXFun C.SXFunction
newtype Fun (f :: * -> *) (g :: * -> *) = Fun { unFun :: C.Function }

instance Show (MXFun f g) where
  showsPrec k (MXFun f) = showsPrec k f
instance Show (SXFun f g) where
  showsPrec k (SXFun f) = showsPrec k f
instance Show (Fun f g) where
  showsPrec k (Fun f) = showsPrec k f

class FunClass fun where
  fromFun :: Fun f g -> IO (fun f g)
  toFun :: fun f g -> Fun f g

instance FunClass Fun where
  fromFun = return
  toFun = id

instance FunClass SXFun where
  fromFun (Fun f) = do
    sxf <- C.sxFunctionFromFunction f
    return (SXFun sxf)
  toFun (SXFun f) = Fun (F.castFunction f)


instance FunClass MXFun where
  fromFun (Fun f) = do
    mxf <- C.mxFunctionFromFunction f
    return (MXFun mxf)
  toFun (MXFun f) = Fun (F.castFunction f)

-- | call a Function on numeric inputs, getting numeric outputs
eval :: (FunClass fun, Scheme f, Scheme g) => fun f g -> f DMatrix -> IO (g DMatrix)
eval f' = fmap fromVector . C.evalDMatrix f . toVector
  where
    Fun f = toFun f'

-- | call a function on MX inputs, yielding MX outputs
call :: (FunClass fun, Scheme f, Scheme g) => fun f g -> f MX -> g MX
call f x = call' f x (AlwaysInline False) (NeverInline False)

-- | call a function on MX inputs, yielding MX outputs
call' :: (FunClass fun, Scheme f, Scheme g)
        => fun f g -> f MX -> AlwaysInline -> NeverInline -> g MX
call' f' x ai ni = fromVector $ C.callMX f (toVector x) ai ni
  where
    Fun f = toFun f'

---- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: (Scheme f, Scheme g) => SXFun f g -> f SX -> g SX
callSX (SXFun sxf) x =
  fromVector $
  C.callSX sxf (toVector x) (AlwaysInline False) (NeverInline False)

mkSym :: forall a f .
         (Scheme f, CMatrix a, Viewable a)
         => (String -> Int -> Int -> IO a)
         -> String -> Proxy f -> IO (f a)
mkSym mk name _ = do
  let sizes :: [(Int,Int)]
      sizes = sizeList (Proxy :: Proxy f)
      f :: (Int, Int) -> Int -> IO a
      f (nrow,ncol) k = mk (name ++ show k) nrow ncol
  ms <- zipWithM f sizes [(0::Int)..]
  return $ fromVector (V.fromList ms)

mkFun ::
  forall f g fun fun' a
  . ( Scheme f, Scheme g, Viewable a
    , C.SharedObjectClass fun, C.OptionsFunctionalityClass fun
    )
  => (String -> Vector a -> Vector a -> M.Map String Opt -> IO fun)
  -> (String -> Proxy f -> IO (f a))
  -> (fun -> fun' f g)
  -> String
  -> (f a -> g a)
  -> M.Map String Opt
  -> IO (fun' f g)
mkFun mkfun mksym con name userf opts = do
  inputs <- mksym "x" (Proxy :: Proxy f)
  fun <- mkfun name (toVector inputs) (toVector (userf inputs)) opts
  return (con fun)

-- | make an MXFunction with name
toMXFun :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f MX -> g MX)
           -> IO (MXFun f g)
toMXFun n f = toMXFun' n f M.empty

-- | make an MXFunction with name and options
toMXFun' :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f MX -> g MX) -> M.Map String Opt
           -> IO (MXFun f g)
toMXFun' = mkFun C.mxFunction (mkSym symM) MXFun

-- | make an SXFunction with name
toSXFun :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f SX -> g SX)
           -> IO (SXFun f g)
toSXFun n f = toSXFun' n f M.empty

-- | make an SXFunction with name and options
toSXFun' :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f SX -> g SX) -> M.Map String Opt
           -> IO (SXFun f g)
toSXFun' = mkFun C.sxFunction (mkSym ssymM) SXFun

-- | expand an MXFunction
expandMXFun :: MXFun f g -> IO (SXFun f g)
expandMXFun (MXFun mxf) = do
  sxf <- M.mxFunction_expand__0 mxf
  C.sharedObject_init__0 sxf
  return (SXFun sxf)

-- | make a function which also contains a jacobian
toFunJac ::
  FunClass fun =>
  fun (JacIn xj x) (JacOut fj f) -> IO (fun (JacIn xj x) (Jac xj fj f))
toFunJac fun0 = do
  let Fun fun = toFun fun0
      compact = False
      symmetric = False
  funJac <- C.jacobian fun 0 0 compact symmetric
  fromFun (Fun funJac)


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
