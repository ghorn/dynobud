{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Dyno.View.Fun
       ( AlwaysInline(..)
       , NeverInline(..)
       , Fun(..)
       , toMXFun
       , toMXFun'
       , toSXFun
       , toSXFun'
       , callDM
       , callMX, callMX'
       , callSX
       , expandMXFun
       , toFunJac, toFunHess
       , checkFunDimensions
       , checkFunDimensionsWith
       ) where

import Control.Monad ( (>=>), zipWithM )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )
import Data.Proxy
import qualified Data.Vector as V
import Text.Printf ( printf )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.MX ( MX, symM )
import Casadi.SX ( SX, ssymM )
import Casadi.Function ( AlwaysInline(..), NeverInline(..) )
import qualified Casadi.Function as C
import Casadi.GenericType ( GType )
import Casadi.DM ( DM )
import Casadi.CMatrix ( CMatrix )
import qualified Casadi.Core.Classes.Function as F
import qualified Casadi.Core.Classes.Sparsity as C

import Dyno.View.FunJac
import Dyno.View.Scheme
import Dyno.View.View ( View )

newtype Fun (f :: * -> *) (g :: * -> *) = Fun { unFun :: C.Function }

instance Show (Fun f g) where
  showsPrec k (Fun f) = showsPrec k f

-- | call a Function on numeric inputs, getting numeric outputs
callDM :: (Scheme f, Scheme g) => Fun f g -> f DM -> IO (g DM)
callDM (Fun f) = fmap fromVector . C.callDM f . toVector

-- | call a function on MX inputs, yielding MX outputs
callMX :: (Scheme f, Scheme g) => Fun f g -> f MX -> g MX
callMX f x = callMX' f x (AlwaysInline False) (NeverInline False)

-- | call a function on MX inputs, yielding MX outputs
callMX' :: (Scheme f, Scheme g)
           => Fun f g -> f MX -> AlwaysInline -> NeverInline -> g MX
callMX' (Fun f) x ai ni = fromVector $ C.callMX f (toVector x) ai ni

-- TODO(greg): remove this
-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: (Scheme f, Scheme g) => Fun f g -> f SX -> g SX
callSX (Fun sxf) x =
  fromVector $ C.callSX sxf (toVector x) (AlwaysInline False) (NeverInline False)

mkSym :: forall a f .
         (Scheme f, CMatrix a)
         => (String -> Int -> Int -> IO a)
         -> String -> Proxy f -> IO (f a)
mkSym mk name _ = do
  let sizes :: [(Int,Int)]
      sizes = sizeList (Proxy :: Proxy f)
      f :: (Int, Int) -> Int -> IO a
      f (nrow,ncol) k = mk (name ++ show k) nrow ncol
  ms <- zipWithM f sizes [(0::Int)..]
  return $ fromVector (V.fromList ms)

-- | make an MXFunction with name
toMXFun :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f MX -> g MX)
           -> IO (Fun f g)
toMXFun n f = toMXFun' n f M.empty

-- | make an MXFunction with name and options
toMXFun' :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f MX -> g MX) -> M.Map String GType
           -> IO (Fun f g)
toMXFun' name userf opts = do
  inputs <- mkSym symM "x" (Proxy :: Proxy f)
  fun <- C.mxFunction name (toVector inputs) (toVector (userf inputs)) opts
  checkFunDimensionsWith ("toMXFun' (" ++ name ++ ")") (Fun fun)

-- | make an SXFunction with name
toSXFun :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f SX -> g SX)
           -> IO (Fun f g)
toSXFun n f = toSXFun' n f M.empty

-- | make an SXFunction with name and options
toSXFun' :: forall f g
           . (Scheme f, Scheme g)
           => String -> (f SX -> g SX) -> M.Map String GType
           -> IO (Fun f g)
toSXFun' name userf opts = do
  inputs <- mkSym ssymM "x" (Proxy :: Proxy f)
  fun <- C.sxFunction name (toVector inputs) (toVector (userf inputs)) opts
  checkFunDimensionsWith ("toSXFun' (" ++ name ++ ")") (Fun fun)


-- | expand an MXFunction
expandMXFun :: (Scheme f, Scheme g) => Fun f g -> IO (Fun f g)
expandMXFun (Fun mxf) = do
  sxf <- F.function_expand__2 mxf
  checkFunDimensionsWith "expandMXFun" (Fun sxf)

-- partial version of checkFunDimensions which throws an error
checkFunDimensionsWith ::
  forall f g
  . (Scheme f, Scheme g)
  => String -> Fun f g -> IO (Fun f g)
checkFunDimensionsWith name fun = do
  case checkFunDimensions fun of
   Left msg -> error $ name ++ " error:\n" ++ msg
   Right _ -> return fun

-- if dimensions are good, return Nothing, otherwise return error message
checkFunDimensions ::
  forall f g
  . (Scheme f, Scheme g)
  => Fun f g -> Either String String
checkFunDimensions (Fun f) = unsafePerformIO $ do
  nInRuntime <- F.function_n_in f
  nOutRuntime <- F.function_n_out f
  let nInType  = numFields (Proxy :: Proxy f)
      nOutType = numFields (Proxy :: Proxy g)
      ioLenErr name nIOType nIORuntime
        | nIOType == nIORuntime = Nothing
        | otherwise =
            Just $ printf "num %s incorrect: type: %d, runtime: %d"
            name nIOType nIORuntime

  case catMaybes [ ioLenErr "inputs" nInType nInRuntime
                 , ioLenErr "outputs" nOutType nOutRuntime
                 ] of
   errs@(_:_) -> return $ Left $ unlines
                 ("checkFunDimensions got ill-dimensioned function:":errs)
   [] -> do
     let getSize sp = do
           s1 <- C.sparsity_size1 sp
           s2 <- C.sparsity_size2 sp
           return (s1, s2)
     sInsRuntime <- mapM (F.function_sparsity_in__1 f >=> getSize) (take nInRuntime [0..])
     sOutsRuntime <- mapM (F.function_sparsity_out__1 f >=> getSize) (take nOutRuntime [0..])
     let sInsType  = sizeList (Proxy :: Proxy f)
         sOutsType = sizeList (Proxy :: Proxy g)
         ioSizeErr name k sType sRuntime
           | sType == sRuntime = Nothing
--           | sType == (1,0) && sRuntime == (0,1) = Nothing
--           | sType == (1,0) && sRuntime == (0,1) = Nothing
           | sType == (1,0) && sRuntime == (0,0) = Nothing
           | otherwise =
               Just $ printf "%s %d dimension mismatch! type: %s, runtime: %s"
               name (k :: Int) (show sType) (show sRuntime)
         sizeErrs =
           (zipWith3 (ioSizeErr "input")  [0..] sInsType  sInsRuntime) ++
           (zipWith3 (ioSizeErr "output") [0..] sOutsType sOutsRuntime)
     return $ case catMaybes sizeErrs of
      [] -> Right $
            unlines
            [ "checkFunDimensions got well-dimensioned function"
            , printf "%d inputs, %d outputs" nInType nOutType
            , "input sizes:  " ++ show sInsType
            , "output sizes: " ++ show sOutsType
            ]
      errs -> Left $ unlines
              ("checkFunDimensions got ill-dimensioned function:":errs)

-- | make a function which also contains a jacobian
toFunJac ::
  (View xj, View fj, Scheme x, Scheme f) =>
  Fun (JacIn xj x) (JacOut fj f)
  -> IO (Fun (JacIn xj x) (Jac xj fj f))
toFunJac (Fun fun) = do
  let compact = False
      symmetric = False
  funJac <- C.jacobian fun 0 0 compact symmetric
  checkFunDimensionsWith "toFunJac" (Fun funJac)

-- | make a function which also contains a jacobian
toFunHess ::
  forall xj x f
  . (View xj, Scheme x, Scheme f)
  => Fun (JacIn xj x) (HessOut f)
  -> IO (Fun (JacIn xj x) (Hess xj f))
toFunHess (Fun fun) = do
  funHess <- C.hessian fun 0 0
  checkFunDimensionsWith "toFunHess" (Fun funHess)
