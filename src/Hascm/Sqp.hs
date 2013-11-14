{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}

module Hascm.Sqp ( solveSqp, SqpIn(..), SqpOut(..), SqpOut'(..) ) where

import Control.Monad ( when )
import Foreign.C.Types ( CInt )
import Data.Maybe ( catMaybes )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Text.Printf


--import Casadi.Wrappers.Enums
--import Casadi.Wrappers.Classes.QPStructure
--import Casadi.Wrappers.Classes.CRSSparsity

import CPLEX
import CPLEX.Param

import Hascm.Vectorize
import Hascm.Casadi.DMatrix
import Hascm.Casadi.SXMatrix
import Hascm.Casadi.SXFunction
import Hascm.Casadi.SX
--import Hascm.Casadi.QP
--import Hascm.Casadi.IOSchemes


import Hascm.Nlp

data SqpIn a = SqpIn { sqpInX :: a
                     , sqpInLambdaX :: a
                     , sqpInLambdaG :: a
                     } deriving (Functor, Generic1, Show)
data SqpOut a = SqpOut { sqpOutF :: a
                       , sqpOutG :: a
                       , sqpOutGradF :: a
                       , sqpOutGradL :: a
                       , sqpOutJacG :: a
                       , sqpOutHessL :: a
                       } deriving (Functor, Generic1, Show)
data SqpOut' a = SqpOut' { sqpOutF' :: a
                         , sqpOutG' :: a
                         , sqpOutGradF' :: a
                         , sqpOutJacG' :: a
                         } deriving (Functor, Generic1, Show)
instance Vectorize SqpIn
instance Vectorize SqpOut
instance Vectorize SqpOut'

toSqpSymbolics ::
  (Vectorize x, Vectorize g) =>
  Nlp x g -> IO (SXFunction SqpIn SqpOut,
                 SXFunction Id SqpOut')
toSqpSymbolics sqp = do
  -- run the function to make SX
  (x', NlpFun f' g') <- funToSX (nlpFG sqp)

  let nG = V.length (vectorize g')
      nX = V.length (vectorize x')

  -- create SXMatrices
  x <- svector (vectorize x')
  lambdaX <- ssymV "lambdaX" nX
  lambdaG <- ssymV "lambdaG" nG
  f <- svector (V.singleton f')
  g <- svector (vectorize g')

  -- gradient f
  gradF <- sgradient f x

  -- lagrangian of hessian
  lambdaGg <- strans lambdaG >>= (`mm` g)
  lambdaXx <- strans lambdaX >>= (`mm` x)
  lagrangian <- f `ssub` lambdaGg >>= (`ssub` lambdaXx)

  -- jacobians / hessian
  jacobG <- sjacobian g x
  gradL <- sgradient lagrangian x
  hessL <- shessian lagrangian x
  
  -- create an SXFunction
  let sqpIn = SqpIn x lambdaX lambdaG
      sqpOut   = SqpOut   f g gradF gradL jacobG hessL
      sqpOut'  = SqpOut'  f g gradF jacobG
  out <- toSXFunction sqpIn sqpOut
  out' <- toSXFunction (Id x) sqpOut'
  
  return (out, out')

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: CInt
cpx_OFF =  0


toSense :: V.Vector (Maybe Double, Maybe Double) -> V.Vector Sense
toSense = V.map toSense'
  where
    toSense' (Nothing, Nothing) = error "toSense: (Nothing, Nothing)"
    toSense' (Nothing,  Just x) = L x
    toSense' ( Just x, Nothing) = G x
    toSense' ( Just x,  Just y) = R x y

toRc :: Functor f => f (Int,Int,a) -> f (Row,Col,a)
toRc = fmap toRc'
  where
    toRc' (x,y,z) = (Row x, Col y, z)

solveSqp :: (Vectorize x, Vectorize g) => Nlp x g -> x Double -> IO (SqpIn DMatrix, SqpOut DMatrix, Kkt Double)
solveSqp nlp x0_ = do
  (sqp ,sqp') <- toSqpSymbolics nlp

  let x0 = dvector (vectorize x0_)
  
  SqpOut' _ g0 gradF0 jacG0 <- evalSXFun sqp' (Id x0)
  SqpOut _ _ _ _ _ hessL0 <- evalSXFun sqp (SqpIn x0 x0 g0)
  let gradF0' = ddata $ ddensify gradF0
      amat = toRc $ V.toList $ dsparse jacG0
      qmat = toRc $ V.toList $ dsparse hessL0

  withEnv $ \env -> withLp env "sqp_baby" $ \lp -> do
    setIntParam env CPX_PARAM_SCRIND cpx_OFF
    setIntParam env CPX_PARAM_DATACHECK cpx_ON

    let objsen = CPX_MIN
        obj = gradF0'
        rhs = toSense $ vectorize (nlpBG nlp)
        xbnds = vectorize (nlpBX nlp) ------- WRONG, need to be delta bounds
    putStrLn "=========================== COPY LP ================================"
    putStrLn $ "objsen: " ++ show objsen
    putStrLn $ "obj: " ++ show obj
    putStrLn $ "rhs: " ++ show rhs
    putStrLn $ "amat: " ++ show amat
    putStrLn $ "xbnds: " ++ show xbnds
    statusLp <- copyLp env lp objsen obj rhs amat xbnds

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    ------------------------
    statusQuad <- copyQuad env lp qmat
    case statusQuad of
      Nothing -> return ()
      Just msg -> error $ "CPXcopyquad error: " ++ msg

    ------------------------
    statusOpt <- qpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg
      
    statusSol <- getSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> runSqpIter 0 (vectorize (nlpBX nlp)) (vectorize (nlpBG nlp)) env lp sqp (vectorize x0_)


-----------------------------------------------------------------
        
toDeltaXBnds :: V.Vector (Maybe Double, Maybe Double) -> V.Vector Double -> V.Vector (Col, Bound)
toDeltaXBnds xbnds xk = V.fromList $ f 0 (V.toList xk) (V.toList xbnds)
  where
    f :: Int -> [Double] -> [(Maybe Double, Maybe Double)] -> [(Col, Bound)]
    f k (_:xs) ((Nothing, Nothing):bds) = f (k+1) xs bds
    f k (x:xs) ((Just lb, Nothing):bds) = (Col k, L' (lb - x)) : f (k+1) xs bds
    f k (x:xs) ((Nothing, Just ub):bds) = (Col k, U' (ub - x)) : f (k+1) xs bds
    f k (x:xs) ((Just lb, Just ub):bds) = (Col k, L' (lb - x)) : (Col k, U' (ub - x)) : f (k+1) xs bds
    f _ [] [] = []
    f _ nx ny = error $ "toDeltaXBnds length mismatch: " ++ show (nx, ny)

toRhs :: V.Vector (Maybe Double, Maybe Double) -> V.Vector (Int, Int, Double) -> V.Vector (Row, Double)
toRhs bg g0 = V.map f g0
  where
    f :: (Int, Int, Double) -> (Row, Double)
    f (k,0,val) = case bg V.!? k of
      Just (Just lbg, _) -> (Row k, lbg - val)
      Just (Nothing, Just ubg) -> (Row k, ubg - val)
      Just (Nothing, Nothing) -> error "why would you have g with no bounds"
      Nothing -> error "toRhs lookup fail"
    f (k,nz,val) = error $ "toRhs: got non-0 column" ++ show (k,nz,val)

toObj :: V.Vector (Int, Int, Double) -> V.Vector (Col, Double)
toObj = V.map f
  where
    f :: (Int, Int, Double) -> (Col, Double)
    f (0,k,val) = (Col k, val)
    f (nz,k,val) = error $ "toObj: got non-0 row" ++ show (nz,k,val)

data Kkt a = Kkt { kktStationarity :: a
                 , kktXPrimal :: a
                 , kktXDual :: a
                 , kktXComplimentarity :: a
                 , kktGPrimal :: a
                 , kktGDual :: a
                 , kktGComplimentarity :: a
                 } deriving (Functor, Show)

feasibility ::
  V.Vector Double -> V.Vector Double -> V.Vector (Maybe Double, Maybe Double) -> V.Vector (Double, Double)
feasibility = V.zipWith3 pdf
  where
    pdf :: Double -> Double -> (Maybe Double, Maybe Double) -> (Double, Double)
    pdf _ _ (Nothing, Nothing) = (0, 0)
    pdf mu x (Just lb, Nothing)
      | x < lb = (lb - x, mu)
      | otherwise = (0, 0)
    pdf mu x (Nothing, Just ub)
      | ub < x = (x - ub, -mu)
      | otherwise = (0, 0)
    pdf mu x (Just lb, Just ub)
      | x < lb = (lb - x, mu)
      | ub < x = (x - ub, -mu)
      | otherwise = (0, 0)

toKkt
  :: V.Vector (Maybe Double, Maybe Double)
     -> V.Vector Double
     -> V.Vector Double
     -> V.Vector (Maybe Double, Maybe Double)
     -> V.Vector Double
     -> SqpOut DMatrix
     -> Kkt (V.Vector Double)
toKkt xbnd lambdaX x0 gbnd lambdaG (SqpOut _ g0 _ gradL _ _) =
  Kkt { kktStationarity = ddata $ ddensify gradL
      , kktXPrimal = xPrimal
      , kktXDual   = xDual
      , kktXComplimentarity   = V.zipWith (*) xPrimal xDual
      , kktGPrimal = gPrimal
      , kktGDual   = gDual
      , kktGComplimentarity   = V.zipWith (*) gPrimal gDual
      }
  where
    (xPrimal, xDual) = V.unzip $ feasibility lambdaX x0 xbnd
    (gPrimal, gDual) = V.unzip $ feasibility lambdaG (ddata $ ddensify g0) gbnd

sqpConverged :: Kkt Double -> Bool
sqpConverged kktInf
  | kktStationarity kktInf > tol = False
  | kktXPrimal kktInf > tol = False
  | kktGPrimal kktInf > tol = False
  | kktXDual kktInf > tol = False
  | kktGDual kktInf > tol = False
  | kktXComplimentarity kktInf > tol = False
  | kktGComplimentarity kktInf > tol = False
  | otherwise = True
  where
    tol = 1e-8

summarizeQp :: CpxSolution -> String
summarizeQp sol =
  init $ unlines
  [ "x      : " ++ show (solX sol)
  , "pi'    : " ++ show (solPi sol)
  , "slack  : " ++ show (solSlack sol)
  , "dj     : " ++ show (solDj sol)
  , "solstat: " ++ show (solStat sol)
  , "objval : " ++ show (solObj sol)
  ]

debug :: String -> IO ()
--debug = putStrLn
debug _ = return ()

data IterPrint a = IterPrint [(String, a -> String)]

ipKkt :: IterPrint (Int, Kkt Double)
ipKkt = IterPrint
        [ (printf "%6.6s" "iter ", \(k, _) -> printf "%5d" k)
        , (printf "%8.8s" "|∇L|  ", \(_, Kkt {kktStationarity = x}) -> printf "%8.2E" x)
        , (printf "%8.8s" "|xp|  ", \(_, Kkt {kktXPrimal = x})     -> printf "%8.2E" x)
        , (printf "%8.8s" "|xd|  ", \(_, Kkt {kktXDual = x})       -> printf "%8.2E" x)
        , (printf "%8.8s" "|gp|  ", \(_, Kkt {kktGPrimal = x})     -> printf "%8.2E" x)
        , (printf "%8.8s" "|gd|  ", \(_, Kkt {kktGDual = x})       -> printf "%8.2E" x)
        ]

printHeader :: IterPrint a -> String
printHeader (IterPrint xs) = field xs
  where
    field ((name, _):xs') = name ++ " " ++ field xs'
    field [] = []

printLine :: IterPrint a -> a -> String
printLine (IterPrint xs) x = field xs
  where
    field ((_, f):xs') = f x  ++ " " ++ field xs'
    field [] = []

runSqpIter :: Int -> V.Vector (Maybe Double, Maybe Double) -> V.Vector (Maybe Double, Maybe Double) -> CpxEnv -> CpxLp -> SXFunction SqpIn SqpOut -> V.Vector Double -> IO (SqpIn DMatrix, SqpOut DMatrix, Kkt Double)
runSqpIter iter bx bg env lp sqp xk = do
  statusOpt <- qpopt env lp
  case statusOpt of
    Nothing -> return ()
    Just msg -> error $ "CPXqpopt error: " ++ msg

  statusSol <- getSolution env lp
  sol <- case statusSol of
    Left msg -> error $ "CPXsolution error: " ++ msg
    Right sol' -> return sol'

  let dxSol = V.fromList $ VS.toList (solX sol)
      lambdaGSol = V.fromList $ VS.toList (solPi sol)
      lambdaXSol = V.fromList $ VS.toList (solDj sol)
      xkp1 = V.zipWith (+) xk dxSol
      deltaxBnds = toDeltaXBnds bx xkp1
  
  let xkp1' = dvector xkp1
      lambdaX' = dvector lambdaXSol
      lambdaG' = dvector lambdaGSol
      sqpIn = (SqpIn xkp1' lambdaX' lambdaG')
  sqpOut@(SqpOut f0 g0 gradF0 _ jacG0 hessL0) <- evalSXFun sqp sqpIn

  let kkt = toKkt bx lambdaXSol xkp1 bg lambdaGSol sqpOut
      kktInf = fmap (V.maximum . V.map abs) kkt

  when (iter `mod` 10 == 0) $ putStrLn $ printHeader ipKkt
  putStrLn $ printLine ipKkt (iter, kktInf)

  -- check for convergence
  if | sqpConverged kktInf -> do
     putStrLn "============================= sqp converged ====================================="
     putStrLn $ summarizeQp sol
     return (sqpIn, sqpOut, kktInf)

     | iter == 500 -> do
     putStrLn "=========================== out of iterations =================================="
     putStrLn $ summarizeQp sol
     return (sqpIn, sqpOut, kktInf)

     | xk == xkp1 -> do
     putStrLn "========================= not converged, but x unchanged ======================="
     putStrLn $ summarizeQp sol
     return (sqpIn, sqpOut, kktInf)

     | otherwise -> do
      debug "\n\n------------------------ STARTING A NEW ITERATION ------------------------------"
      debug $ summarizeQp sol
      debug $ "x0: " ++ show xkp1
      debug $ "sqpIn: " ++ show sqpIn
      debug $ "sqpOut: " ++ show sqpOut

      -- change obj
      let gradF0' = dsparse (dtrans gradF0)
      let obj = toObj gradF0'
      debug $ "new obj: " ++ show obj
      cobj <- changeObj env lp obj
      case cobj of
        Nothing -> return ()
        Just msg -> error $ "changeObj error: " ++ msg
    
      -- change rhs
      let g0' = dsparse g0
          rhs = toRhs bg g0'
      debug $ "new rhs: " ++ show rhs
      crhs <- changeRhs env lp rhs
      case crhs of
        Nothing -> return ()
        Just msg -> error $ "changeRhs error: " ++ msg
    
      -- change bounds
      debug $ "new deltaxBds: " ++ show deltaxBnds
      cb <- changeBds env lp deltaxBnds
      case cb of
        Nothing -> return ()
        Just msg -> error $ "changeBds error: " ++ msg
    
      let amat = toRc $ dsparse jacG0
    
      debug $ "new coefs: " ++ show amat
      ccl <- changeCoefList env lp amat
      case ccl of
        Nothing -> return ()
        Just msg -> error $ "changeCoefList error: " ++ msg
    
      let qmat = toRc $ V.toList $ dsparse hessL0
      debug $ "new qp coefs: " ++ show qmat
      cqps <- mapM (\(r,c,v) -> changeQpCoef env lp r c v) qmat
      case catMaybes cqps of
        [] -> return ()
        msgs -> error $ "changeQpCoef errors: " ++ show msgs
    
      runSqpIter (iter + 1) bx bg env lp sqp xkp1
