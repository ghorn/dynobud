{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
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
       , checkFunDimensions
       , checkFunDimensionsWith
       ) where

import Control.Monad ( (>=>), zipWithM )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Text.Printf ( printf )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.MX ( MX, symM )
import Casadi.SX ( SX, ssymM )
import Casadi.Function ( AlwaysInline(..), NeverInline(..) )
import qualified Casadi.Function as C
import qualified Casadi.MXFunction as C
import qualified Casadi.SXFunction as C
import Casadi.Option
import Casadi.DMatrix ( DMatrix )
import Casadi.CMatrix ( CMatrix )
import Casadi.Viewable ( Viewable )
import qualified Casadi.Core.Classes.Function as F
import qualified Casadi.Core.Classes.MXFunction as M
import qualified Casadi.Core.Classes.Sparsity as C
import qualified Casadi.Core.Classes.OptionsFunctionality as C

import Dyno.View.FunJac
import Dyno.View.Scheme
import Dyno.View.View ( View )

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

mkToFun ::
  forall f g fun fun' a
  . ( Scheme f, Scheme g, Viewable a, FunClass fun'
    , C.OptionsFunctionalityClass fun
    )
  => String
  -> (String -> Vector a -> Vector a -> M.Map String Opt -> IO fun)
  -> (String -> Proxy f -> IO (f a))
  -> (fun -> fun' f g)
  -> String
  -> (f a -> g a)
  -> M.Map String Opt
  -> IO (fun' f g)
mkToFun errName mkfun mksym con name userf opts = do
  inputs <- mksym "x" (Proxy :: Proxy f)
  fun <- mkfun name (toVector inputs) (toVector (userf inputs)) opts
  checkFunDimensionsWith (errName ++ " (" ++ name ++ ")") (con fun)

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
toMXFun' = mkToFun "toMXFun" C.mxFunction (mkSym symM) MXFun

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
toSXFun' = mkToFun "toSXFun" C.sxFunction (mkSym ssymM) SXFun

-- | expand an MXFunction
expandMXFun :: (Scheme f, Scheme g) => MXFun f g -> IO (SXFun f g)
expandMXFun (MXFun mxf) = do
  sxf <- M.mxFunction_expand__0 mxf
  checkFunDimensionsWith "expandMXFun" (SXFun sxf)

-- partial version of checkFunDimensions which throws an error
checkFunDimensionsWith ::
  forall fun f g
  . (FunClass fun, Scheme f, Scheme g)
  => String -> fun f g -> IO (fun f g)
checkFunDimensionsWith name fun = do
  case checkFunDimensions fun of
   Left msg -> error $ name ++ " error:\n" ++ msg
   Right _ -> return fun

-- if dimensions are good, return Nothing, otherwise return error message
checkFunDimensions ::
  forall fun f g
  . (FunClass fun, Scheme f, Scheme g)
  => fun f g -> Either String String
checkFunDimensions f' = unsafePerformIO $ do
  let f :: F.Function
      Fun f = toFun f'
  nInRuntime <- F.function_nIn f
  nOutRuntime <- F.function_nOut f
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
     sInsRuntime <- mapM (F.function_inputSparsity__2 f >=> getSize)
                    (take nInRuntime [0..])
     sOutsRuntime <- mapM (F.function_outputSparsity__2 f >=> getSize)
                     (take nOutRuntime [0..])
     let sInsType  = sizeList (Proxy :: Proxy f)
         sOutsType = sizeList (Proxy :: Proxy g)
         ioSizeErr name k sType sRuntime
           | sType == sRuntime = Nothing
           | sType == (1,0) && sRuntime == (0,1) = Nothing
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
  (FunClass fun, View xj, View fj, Scheme x, Scheme f) =>
  fun (JacIn xj x) (JacOut fj f) -> IO (fun (JacIn xj x) (Jac xj fj f))
toFunJac fun0 = do
  let Fun fun = toFun fun0
      compact = False
      symmetric = False
  funJac <- C.jacobian fun 0 0 compact symmetric
  fromFun (Fun funJac) >>= checkFunDimensionsWith "toFunJac"
