{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Dyno.View.CustomFunction
       ( CustomFun(..)
       , DerivGen(..)
       , toCustomFun
       ) where

import Control.Monad ( zipWithM )
import Data.Proxy
import qualified Data.Vector as V
import Data.Vector ( Vector )
import qualified Data.Foldable as F

import Casadi.Sparsity ( Sparsity, dense )
import Casadi.Option ( Opt(..), setOption )
import Casadi.SharedObject ( soInit )

import qualified Dyno.TypeVecs as TV
import Dyno.TypeVecs ( Vec, Dim, reifyDim )
import Dyno.View.CasadiMat ( DMatrix, sparsity )
import Dyno.View.Scheme ( Scheme(..) )
import Dyno.View.Fun ( Fun(..) )
import Casadi.Callback ( makeCustomEvaluate, makeDerivativeGenerator )
import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.CustomFunction as C
import qualified Casadi.Core.Classes.IOInterfaceFunction as C
import Casadi.Core.Classes.DerivativeGenerator ( DerivativeGenerator )


data CustomFun f g =
  CustomFun
  { cfFun :: f DMatrix -> IO (g DMatrix)
  , cfSparsityIn :: Maybe (f Sparsity)
  , cfSparsityOut :: Maybe (g Sparsity)
  , cfDerivGenerator :: Maybe (DerivGen f g)
  , cfOptions :: [(String, Opt)]
  }

data DerivGen f g =
  DerivGen
  { dgGetSeeds :: forall nfwd nadj
                  . (Dim nfwd, Dim nadj)
                  => f DMatrix -> Vec nfwd (f DMatrix) -> Vec nadj (g DMatrix)
                  -> IO (g DMatrix, Vec nfwd (g DMatrix), Vec nadj (f DMatrix))
  , dgOptions :: [(String, Opt)]
  , dgFwdSparsity :: Maybe (f Sparsity)
  , dgAdjSparsity :: Maybe (g Sparsity)
  }

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf k vs
  | length vs0 == k = vs0 : groupsOf k vs1
  | otherwise = error "groupsOf not divisible"
  where
    (vs0, vs1) = splitAt k vs

toDerivGen :: forall f g . (Scheme f, Scheme g) => DerivGen f g -> IO DerivativeGenerator
toDerivGen dg = makeDerivativeGenerator $ \originalFun nfwd nadj -> do
  let f fun = do
        putStrLn "custom evaluate"
        numIn <- C.ioInterfaceFunction_getNumInputs fun
        inputs <- mapM (C.ioInterfaceFunction_getInput__2 fun) (take numIn [0..])

        let nf = numFields (Proxy :: Proxy f)
            ng = numFields (Proxy :: Proxy g)

        let f' :: forall nfwd nadj
                  . (Dim nfwd, Dim nadj)
                  => Proxy nfwd -> Proxy nadj -> IO (Vector DMatrix)
            f' _ _ = do
              let (inputs0', inputs12') = splitAt nf inputs
                  (inputs1', inputs2') = splitAt (nfwd*nf) inputs12'

                  inputs0 :: f DMatrix
                  inputs0 = fromVector (V.fromList inputs0')

                  inputs1 :: Vec nfwd (f DMatrix)
                  inputs1 = TV.mkVec' (map (fromVector . V.fromList) (groupsOf nf inputs1'))

                  inputs2 :: Vec nadj (g DMatrix)
                  inputs2 = TV.mkVec' (map (fromVector . V.fromList) (groupsOf ng inputs2'))
              (out0, out1, out2) <- dgGetSeeds dg inputs0 inputs1 inputs2
              let out0' = toVector out0
                  out1' = V.concat $ F.toList (fmap toVector out1)
                  out2' = V.concat $ F.toList (fmap toVector out2)
              return (V.concat [out0', out1', out2'])

        outs <- reifyDim nfwd $ \pnfwd ->
          reifyDim nadj $ \pnadj -> f' pnfwd pnadj
        _ <- zipWithM (C.ioInterfaceFunction_setOutput__2 fun) (V.toList outs) [0..]
        return ()

  ce <- makeCustomEvaluate f


  numIn <- C.ioInterfaceFunction_getNumInputs originalFun
  numOut <- C.ioInterfaceFunction_getNumOutputs originalFun
  spIns0 <- mapM (fmap sparsity . (C.ioInterfaceFunction_getInput__2 originalFun)) (take numIn [0..])
  spOuts0 <- mapM (fmap sparsity . (C.ioInterfaceFunction_getOutput__2 originalFun)) (take numOut [0..])

  let spFwd = case dgFwdSparsity dg of
--        Just sp -> toVector sp
        _ -> V.fromList $ map (uncurry dense) $ sizeList (Proxy :: Proxy f)
      spAdj = case dgAdjSparsity dg of
--        Just sp -> toVector sp
        _ -> V.fromList $ map (uncurry dense) $ sizeList (Proxy :: Proxy g)

      -- TODO: this is only right when everything's dense because it depends on jac sparsity!!!
      spIns = V.concat [ V.fromList spIns0
                       , V.concat (replicate nfwd spFwd)
                       , V.concat (replicate nadj spAdj)
                       ]
      spOuts = V.concat [ V.fromList spOuts0
                        , V.concat (replicate nfwd spAdj)
                        , V.concat (replicate nadj spFwd)
                        ]
  cf <- C.customFunction__1 ce spIns spOuts
  mapM_ (\(n,Opt o) -> setOption cf n o) (dgOptions dg)
  
  soInit cf
  
  return (C.castFunction cf)

toCustomFun ::
  forall f g
  . (Scheme f, Scheme g)
  => CustomFun f g
  -> IO (Fun f g)
toCustomFun customFun = do
  ce <- makeCustomEvaluate $ \fun -> do
        numIn <- C.ioInterfaceFunction_getNumInputs fun
        inputs <- mapM (C.ioInterfaceFunction_getInput__2 fun) (take numIn [0..])
        outputs <- cfFun customFun $ fromVector (V.fromList inputs)
        _ <- zipWithM (C.ioInterfaceFunction_setOutput__2 fun) (V.toList (toVector outputs)) [0..]
        return ()

  let spIn :: Vector Sparsity
      spIn = case cfSparsityIn customFun of
        Just spIn' -> toVector spIn'
        Nothing -> V.fromList $ map (uncurry dense) $ sizeList (Proxy :: Proxy f)
      spOut :: Vector Sparsity
      spOut = case cfSparsityOut customFun of
        Just spOut' -> toVector spOut'
        Nothing -> V.fromList $ map (uncurry dense) $ sizeList (Proxy :: Proxy g)

  cf <- C.customFunction__1 ce spIn spOut
  
  mapM_ (\(n,Opt o) -> setOption cf n o) (cfOptions customFun)
  case cfDerivGenerator customFun of
    Nothing -> return ()
    Just dg -> do
      dgen <- toDerivGen dg
      setOption cf "derivative_generator" dgen
  soInit cf
  
  return (Fun (C.castFunction cf))
