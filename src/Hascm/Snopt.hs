{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Hascm.Snopt where

import qualified Data.Vector as V
import Data.Maybe ( fromMaybe )

import Control.Monad ( unless, when )
import qualified Data.HashSet as HS
import Foreign.ForeignPtr ( newForeignPtr_ )
import Foreign.Storable ( peek )
import qualified Data.Vector.Storable as VS

import Hascm.Vectorize
import Hascm.Casadi.SX
import Hascm.Casadi.DMatrix
import Hascm.Casadi.SXMatrix
import Hascm.Casadi.SXFunction
import Hascm.Nlp

import Snopt.SnoptA
--import Snopt.Bindings ( U_fp )

inf :: Double
inf = read "1e50"

toBnds :: (Maybe Double, Maybe Double) -> (Double, Double)
toBnds (lb,ub) = (fromMaybe (-inf) lb, fromMaybe inf ub)

--makeFG :: [Expr Double] -> [Expr Double] -> IO
--          (V.Vector Double -> IO (VS.Vector Double),
--           V.Vector Double -> IO (VS.Vector Double),
--           V.Vector (Int,Int),
--           SXMatrix, SXMatrix)
--makeFG inputs outputs = do
--  alg <- constructAlgorithm (V.fromList inputs) (V.fromList outputs)
--  (sxIns, sxOuts) <- toSXMats alg
--  putStrLn "makeFG: inputs:"
--  repr sxIns
----  desc sxIns
--  putStrLn "makeFG: f outputs:"
--  repr sxOuts
----  desc sxOuts
--
--
data SnoptIn a = SnoptIn { snoptInX :: a
                         , snoptInP :: a
                         } deriving (Functor, Generic1, Show)
data SnoptOut a = SnoptOut { snoptOutFG :: a
                           , snoptOutFGJacob :: a
                           } deriving (Functor, Generic1, Show)
instance Vectorize SnoptIn
instance Vectorize SnoptOut

toSnoptSymbolics :: (Vectorize x, Vectorize p, Vectorize g) => Nlp x p g ->
                    IO (SnoptIn (V.Vector Double) -> IO (SnoptOut (V.Vector Double)), V.Vector (Int,Int,SX))
toSnoptSymbolics nlp = do
  -- run the function to make SX
  (NlpInputs x' p', NlpFun f' g') <- funToSX (nlpFG nlp)
  let fg' = V.singleton f' V.++ vectorize g'

  -- create SXMatrices
  x <- svector (vectorize x')
  p <- svector (vectorize p')
  fg <- svector fg'

  -- jacobian
  fgJacob <- sjacobian fg x

  fgSparse <- ssparse fgJacob

  -- create an SXFunction
  let snoptIn = SnoptIn x p
      snoptOut  = SnoptOut fg fgJacob
  snoptFun <- toSXFunction snoptIn snoptOut

  let callFun :: SnoptIn (V.Vector Double) -> IO (SnoptOut (V.Vector Double))
      callFun xp = fmap (fmap ddata) $ evalSXFun snoptFun (fmap dvector xp)

  return (callFun, fgSparse)


solveNlpSnopt :: forall x p g . (Vectorize x, Vectorize p, Vectorize g) =>
            Nlp x p g -> Maybe (x Double -> IO Bool) -> x Double -> p Double ->
            Maybe (Multipliers x g Double) ->
            IO (Either String (NlpOut x g Double))
solveNlpSnopt nlp callback x0 p lambda0 = do
  (snoptFun, jacobSparsity) <- toSnoptSymbolics nlp

  let fgbnds = V.map toBnds $ V.singleton (Nothing, Nothing) V.++ (vectorize $ nlpBG nlp)
      xbnds = V.map toBnds $ vectorize $ nlpBX nlp
      (flow, fupp) = V.unzip fgbnds

      nx = V.length (vectorize x0)
      (xlow, xupp) = V.unzip xbnds
      xInit = vectorize x0

      f0init = replicate nf 0
      nf = V.length fgbnds

  let ijxA :: [((Int,Int),Double)]
      ijxA = []

      ijG :: V.Vector (Int,Int)
      ijG = V.map (\(x,y,_) -> (x+1,y+1)) jacobSparsity

      (ijA,aval) = unzip ijxA
      (iAfun,jAvar) = unzip ijA
      (iGfun,jGvar) = unzip $ V.toList ijG

      na = length ijA
      ng = V.length ijG
  let ijG' = HS.fromList (V.toList ijG)
      ijA' = HS.fromList ijA
      inter = HS.toList $ HS.intersection ijG' ijA'
  unless (null inter) $ error $
    "ijG and ijA overlap, shared elements: " ++ show inter

  let --userfg :: U_fp
      userfg _status n' x' needF nF' f' needG lenG' g' _ _ _ _ _ _ = do
        xp <- newForeignPtr_ x'
        fp <- newForeignPtr_ f'
        gp <- newForeignPtr_ g'

        n <- fmap fromIntegral $ peek n'
        nF <- fmap fromIntegral $ peek nF'
        lenG <- fmap fromIntegral $ peek lenG'
        --statuss <- peek status
        --putStrLn $ "status: " ++ show statuss

        when (n /= V.length (vectorize x0)) $ error "x length mismatch lol"
        when (nF /= V.length fgbnds) $ error "f length mismatch lol"
        when (lenG /= ng) $ error "lenG mismatch lol"

        let xvec = V.fromList $ VS.toList $ VS.unsafeFromForeignPtr0 xp nx
        SnoptOut fg jacob <- snoptFun (SnoptIn xvec (vectorize p))

        when (nF /= V.length fg) $ error "fg length mismatch lol"
        when (lenG /= V.length jacob) $ error "jacob mismatch lol"

        case callback of
          Nothing -> return ()
          Just cb -> cb (devectorize xvec) >> return () -- should halt of callback returns False

        fvec <- VS.unsafeThaw $ VS.unsafeFromForeignPtr0 fp nf
        gvec <- VS.unsafeThaw $ VS.unsafeFromForeignPtr0 gp ng
        needF' <- peek needF
        needG' <- peek needG

        unless (needF' `elem` [0,1]) $ error "needF isn't 1 or 0"
        unless (needG' `elem` [0,1]) $ error "needG isn't 1 or 0"
        when (needF' `elem` [0,1]) $ do
--          putStrLn $ "callF: " ++ show fvec'
          VS.copy fvec (VS.fromList $ V.toList fg)
        when (needG' `elem` [0,1]) $ do
--          putStrLn $ "callG: " ++ show gvec'
          VS.copy gvec (VS.fromList $ V.toList jacob)

  let --runSnopt :: IO (Either String SnInteger)
      --runSnopt = runSnoptA 500 10000 20000 nx nf na ng userfg $ do
      runSnopt = runSnoptA 500 10000000 20000000 nx nf na ng userfg $ do
        sninit

        --snseti "Verify level" 3
        --snseti "Major print level" 111111

        setIAfun $ VS.fromList $ map fromIntegral iAfun
        setXlow $ VS.fromList $ V.toList xlow
        setXupp $ VS.fromList $ V.toList xupp
        setX $ VS.fromList $ V.toList xInit

        setFlow $ VS.fromList $ V.toList flow
        setFupp $ VS.fromList $ V.toList fupp
        setF $ VS.fromList f0init

        setObjRow 1
        setObjAdd 0

        setIAfun $ VS.fromList $ map fromIntegral iAfun
        setJAvar $ VS.fromList $ map fromIntegral jAvar
        setA $ VS.fromList aval

        setIGfun $ VS.fromList $ map fromIntegral iGfun
        setJGvar $ VS.fromList $ map fromIntegral jGvar

        -- if lagrange multipliers are available, use them
        case lambda0 of
          Nothing -> return ()
          Just (Multipliers lamX lamG) -> do
            setXmul $ VS.fromList $ V.toList (vectorize lamX)
            setFmul $ VS.fromList $ 0 : V.toList (vectorize lamG)

        snopta ""-- "toy1"

        xopt <- getX
        fopt <- getF
        lambdaX' <- getXmul
        lambdaF <- getFmul

        let lambdaOpt' =
              Multipliers { lambdaX = devectorize $ V.fromList $ VS.toList lambdaX'
                          , lambdaG = devectorize $ V.fromList $ tail $ VS.toList lambdaF
                          }

        return NlpOut { fOpt = VS.head fopt
                      , xOpt = devectorize $ V.fromList $ VS.toList xopt
                      , gOpt = devectorize $ V.fromList $ tail $ VS.toList xopt
                      , lambdaOpt = lambdaOpt'
                      }

  runSnopt
