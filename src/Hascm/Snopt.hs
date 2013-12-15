{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Hascm.Snopt ( solveNlpSnopt ) where

import qualified Data.Vector as V
import Data.Maybe ( fromMaybe )

import Control.Monad ( unless, when )
import qualified Data.HashSet as HS
import Foreign.C.Types ( CInt )
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

data SnoptIn a = SnoptIn a a
               deriving (Functor, Generic1, Show)
data SnoptOut a = SnoptOut a a
                deriving (Functor, Generic1, Show)
data SnoptG a = SnoptG a deriving (Functor, Generic1, Show)
instance Vectorize SnoptIn
instance Vectorize SnoptOut

toSnoptSymbolics :: (Vectorize x, Vectorize p, Vectorize g) => Nlp x p g ->
                    IO (SnoptIn (V.Vector Double) -> IO (SnoptOut (V.Vector Double)), V.Vector (Int,Int,SX))
toSnoptSymbolics nlp = do
  -- run the function to make SX
  (NlpInputs x' p', NlpFun obj' constraints') <- funToSX (nlpFG nlp)

  -- create SXMatrices
  let x = svector (vectorize x')
      p = svector (vectorize p')
      obj = svector (V.singleton obj')
      constraints = svector (vectorize constraints')
      f = svector $ V.singleton obj' V.++ vectorize constraints'

      objGrad = strans (sgradient obj x)
      constraintJacobian = sjacobian constraints x
      g = svertcat (V.fromList [objGrad, constraintJacobian])
      gSparse = ssparse g

  -- create an SXFunction
  let snoptIn = SnoptIn x p
  fgFun <- toSXFunction snoptIn (SnoptOut f g)

  let callFG :: SnoptIn (V.Vector Double) -> IO (SnoptOut (V.Vector Double))
      callFG xp = fmap (fmap ddata) $ evalSXFun fgFun (fmap dvector xp)

  return (callFG, gSparse)


solveNlpSnopt :: forall x p g . (Vectorize x, Vectorize p, Vectorize g) =>
            Nlp x p g -> Maybe (x Double -> IO Bool) ->
            Maybe (Multipliers x g Double) ->
            IO (Either String (NlpOut x g Double))
solveNlpSnopt nlp callback lambda0 = do
  (snoptFun, jacobSparsity) <- toSnoptSymbolics nlp

  let fbnds = V.map toBnds $ V.singleton (Nothing, Nothing) V.++ bg
      bx = vectorize (nlpBX nlp)
      bg = vectorize $ nlpBG nlp
      xbnds = V.map toBnds bx
      (flow, fupp) = V.unzip fbnds

      x0 = vectorize $ nlpX0 nlp
      p = nlpP nlp
      nx = V.length x0
      (xlow, xupp) = V.unzip xbnds

      nf = V.length fbnds
  SnoptOut f0init _ <- snoptFun (SnoptIn x0 (vectorize p))

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

        when (n /= nx) $
          error $ "x0 length mismatch lol" ++ show (n , nx)
        when (nF /= V.length fbnds) $
          error $ "fbnds length mismatch lol" ++ show (nF, V.length fbnds)
        when (ng /= lenG) $ error $ "lenG mismatch lol" ++ show (ng, lenG)

        let xvec = V.fromList $ VS.toList $ VS.unsafeFromForeignPtr0 xp nx
        SnoptOut f jacob <- snoptFun (SnoptIn xvec (vectorize p))

        when (nF /= V.length f) $ error $ "f length mismatch lol" ++ show (nF, V.length f)
        when (ng /= V.length jacob) $ error $ "jacob length mismatch lol" ++ show (ng, V.length jacob)

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
          VS.copy fvec (VS.fromList $ V.toList f)
        when (needG' `elem` [0,1]) $ do
--          putStrLn $ "callG: " ++ show gvec'
          VS.copy gvec (VS.fromList $ V.toList jacob)

  let --runSnopt :: IO (Either String SnInteger)
      --runSnopt = runSnoptA 500 10000 20000 nx nf na ng userfg $ do
      runSnopt = runSnoptA 500 10000000 20000000 nx nf na ng userfg $ do
        setIsummary 6
        setIprint 9 -- 0
        sninit

        --snseti "Verify level" 3
        --snseti "Major print level" 111111

        setIAfun $ VS.fromList $ map fromIntegral iAfun
        setXlow $ VS.fromList $ V.toList xlow
        setXupp $ VS.fromList $ V.toList xupp
        setX $ VS.fromList $ V.toList x0

        setFlow $ VS.fromList $ V.toList flow
        setFupp $ VS.fromList $ V.toList fupp
        setF $ VS.fromList $ V.toList f0init

        setObjRow 1
        setObjAdd 0

        setIAfun $ VS.fromList $ map fromIntegral iAfun
        setJAvar $ VS.fromList $ map fromIntegral jAvar
        setA $ VS.fromList aval

        setIGfun $ VS.fromList $ map fromIntegral iGfun
        setJGvar $ VS.fromList $ map fromIntegral jGvar

        -- if lagrange multipliers are available, use them
        -- the snopt manual specifies that when constraints are initially active,
        -- x should == xlb or xub, so the initial state here might be tweaked by 1e-9
        -- to set that exactly
        case lambda0 of
          Nothing -> return ()
          Just (Multipliers lamX lamG) -> do
            let lamX' = vectorize lamX
                lamG' = vectorize lamG
            setXmul $ VS.fromList $ V.toList lamX'
            setFmul $ VS.fromList $ 0 : V.toList lamG'
            let variableState :: Double -> (Maybe Double, Maybe Double) -> Double -> (CInt, Double)
                variableState x (Nothing, Nothing) _ = (0, x)
                variableState x (Just lbx, _) lambdax
                  | lambdax < -1e-9 && (lbx - x >  -1e-9) = (4, lbx)
                variableState x (_, Just ubx) lambdax
                  | lambdax > 1e-9 && (x - ubx >  -1e-9) = (5, ubx)
                variableState x _ _ = (0, x)

                (xstate, x0') = V.unzip $ V.zipWith3 variableState x0 bx lamX'
                (fstate, f0') = V.unzip $ V.zipWith3 variableState
                                (f0init)
                                (V.singleton (Nothing, Nothing) V.++ bg)
                                (V.singleton 0 V.++ lamG')
            setX $ VS.fromList $ V.toList x0'
            setF $ VS.fromList $ V.toList f0'
            setXstate $ VS.fromList $ V.toList xstate
            setFstate $ VS.fromList $ V.toList fstate

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
