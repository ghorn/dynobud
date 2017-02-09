{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Dyno.View.Fun
       ( Fun(..)
       , toFun
       , callSym, callDM
       , expandFun
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

import qualified Casadi.Core.Classes.Function as F
import qualified Casadi.Core.Classes.Sparsity as C
import           Casadi.DM ( DM )
import qualified Casadi.Function as C
import           Casadi.GenericType ( GType )
import qualified Casadi.Matrix as CM

import Dyno.View.Scheme ( Scheme(..) )

newtype Fun (f :: * -> *) (g :: * -> *) = Fun { unFun :: C.Function }

instance Show (Fun f g) where
  showsPrec k (Fun f) = showsPrec k f

-- | call a Function on symbolic inputs, getting symbolic outputs
callSym :: (Scheme f, Scheme g, CM.SMatrix a) => Fun f g -> f a -> g a
callSym (Fun f) = fromVector . CM.callSym f . toVector

-- | call a Function on numeric inputs, getting numeric outputs
callDM :: (Scheme f, Scheme g) => Fun f g -> f DM -> IO (g DM)
callDM (Fun f) = (fmap fromVector) . C.callDM f . toVector

-- | make a Function with name and options
toFun :: forall f g a
         . (Scheme f, Scheme g, CM.SMatrix a)
      => String -> (f a -> g a) -> M.Map String GType
      -> IO (Fun f g)
toFun name userf opts = do
  let xsizes :: [(Int, Int)]
      xsizes = sizeList (Proxy :: Proxy f)

      mkSym :: (Int, Int) -> Int -> IO a
      mkSym (nrow, ncol) k = CM.sym ("x" ++ show k) nrow ncol

  inputs <- (fromVector . V.fromList) <$> zipWithM mkSym xsizes [(0::Int)..] :: IO (f a)

  fun <- CM.toFunction name (toVector inputs) (toVector (userf inputs)) opts
  checkFunDimensionsWith ("toFun' (" ++ name ++ ")") (Fun fun)

-- | expand a Function
expandFun :: (Scheme f, Scheme g) => Fun f g -> IO (Fun f g)
expandFun (Fun mxf) = do
  sxf <- F.function_expand__2 mxf
  checkFunDimensionsWith "expandFun" (Fun sxf)

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
