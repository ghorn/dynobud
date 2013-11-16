{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}

module Hascm.Sqp.Sqp ( solveSqp, SqpIn(..), SqpOut(..), SqpIn'(..), SqpOut'(..) ) where

import Control.Monad ( when )
import Foreign.C.Types ( CInt )
import Data.Maybe ( catMaybes )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Text.Printf

import CPLEX
import CPLEX.Param

import Hascm.Sqp.LineSearch

import Hascm.Vectorize
import Hascm.Casadi.DMatrix
import Hascm.Casadi.SXMatrix
import Hascm.Casadi.SXFunction
import Hascm.Casadi.SX

import Hascm.Nlp

debug :: String -> IO ()
--debug = putStrLn
debug _ = return ()

data SqpIn a = SqpIn { sqpInX :: a
                     , sqpInP :: a
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
data SqpIn' a = SqpIn' { sqpInX' :: a
                       , sqpInP' :: a
                       } deriving (Functor, Generic1, Show)
data SqpOut' a = SqpOut' { sqpOutF' :: a
                         , sqpOutG' :: a
                         } deriving (Functor, Generic1, Show)
instance Vectorize SqpIn
instance Vectorize SqpOut
instance Vectorize SqpIn'
instance Vectorize SqpOut'

toSqpSymbolics :: (Vectorize x, Vectorize p, Vectorize g) => Nlp x p g ->
                  IO (SXFunction SqpIn SqpOut, SXFunction SqpIn' SqpOut')
toSqpSymbolics sqp = do
  -- run the function to make SX
  (NlpInputs x' p', NlpFun f' g') <- funToSX (nlpFG sqp)

  let nG = V.length (vectorize g')
      nX = V.length (vectorize x')

  -- create SXMatrices
  x <- svector (vectorize x')
  p <- svector (vectorize p')
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
  let sqpIn  = SqpIn  x p lambdaX lambdaG
      sqpIn' = SqpIn' x p
      sqpOut  = SqpOut  f g gradF gradL jacobG hessL
      sqpOut' = SqpOut' f g
  debug $ show sqpOut
  sqpFun  <- toSXFunction sqpIn  sqpOut
  sqpFun' <- toSXFunction sqpIn' sqpOut'
  return (sqpFun, sqpFun')


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

toDeltaXBnd0 :: Double -> (Maybe Double, Maybe Double) -> (Maybe Double, Maybe Double)
toDeltaXBnd0 x0' (Just lb, Nothing) = (Just (lb - x0'), Nothing)
toDeltaXBnd0 x0' (Nothing, Just ub) = (Nothing, Just (ub - x0'))
toDeltaXBnd0 x0' (Just lb, Just ub) = (Just (lb - x0'), Just (ub - x0'))
toDeltaXBnd0 _ (Nothing, Nothing) = (Nothing, Nothing)

solveSqp :: (Vectorize x, Vectorize p, Vectorize g) =>
            Nlp x p g -> LineSearch IO Double -> x Double -> p Double -> IO (SqpIn DMatrix, SqpOut DMatrix, Kkt Double)
solveSqp nlp lineSearch x0_ p0_ = do
  (sqp, sqp') <- toSqpSymbolics nlp

  let x0 = dvector (vectorize x0_)
      p0 = dvector (vectorize p0_)
      lambdaX0 = vectorize $ fmap (const 0) (nlpBX nlp)
      lambdaG0 = vectorize $ fmap (const 0) (nlpBG nlp)

  SqpOut _ _ gradF0 _ jacG0 hessL0 <- evalSXFun sqp (SqpIn x0 p0 (dvector lambdaX0) (dvector lambdaG0))

  withEnv $ \env -> withLp env "sqp_baby" $ \lp -> do
    setIntParam env CPX_PARAM_SCRIND cpx_OFF
    setIntParam env CPX_PARAM_DATACHECK cpx_ON

    let objsen = CPX_MIN
        obj = ddata $ ddensify gradF0
        amat = toRc $ V.toList $ dsparse jacG0
        qmat = toRc $ V.toList $ dsparse hessL0
        rhs = toSense $ vectorize (nlpBG nlp)
        xbnds = vectorize (nlpBX nlp)
        deltaXBnds = V.zipWith toDeltaXBnd0 (vectorize x0_) xbnds

    debug "=========================== COPY LP ================================"
    debug $ "objsen: " ++ show objsen
    debug $ "obj: " ++ show obj
    debug $ "rhs: " ++ show rhs
    debug $ "amat: " ++ show amat
    debug $ "xbnds: " ++ show xbnds
    debug $ "deltaXBnds: " ++ show deltaXBnds
    statusLp <- copyLp env lp objsen obj rhs amat deltaXBnds

    -- copy lp and quad
    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    statusQuad <- copyQuad env lp qmat
    case statusQuad of
      Nothing -> return ()
      Just msg -> error $ "CPXcopyquad error: " ++ msg

    -- if all goes well, start sqp iterations
    runSqpIter 0 lineSearch (vectorize (nlpBX nlp)) (vectorize (nlpBG nlp))
      env lp sqp sqp' (vectorize p0_) (vectorize x0_) lambdaX0 lambdaG0


runSqpIter ::
  Int -> LineSearch IO Double -> V.Vector (Maybe Double, Maybe Double) -> V.Vector (Maybe Double, Maybe Double)
  -> CpxEnv -> CpxLp -> SXFunction SqpIn SqpOut -> SXFunction SqpIn' SqpOut'
  -> V.Vector Double -> V.Vector Double
  -> V.Vector Double -> V.Vector Double
  -> IO (SqpIn DMatrix, SqpOut DMatrix, Kkt Double)
runSqpIter iter lineSearch bx bg env lp sqp sqp' p0 xk lambdaXk lambdaGk = do
  -- test the current point
  let sqpIn :: SqpIn DMatrix
      sqpIn = fmap dvector $ SqpIn xk p0 lambdaXk lambdaGk
  sqpOut@(SqpOut _ gk gradFk gradLk jacGk hessLk) <- evalSXFun sqp sqpIn

  let kkt = toKkt bx lambdaXk xk bg lambdaGk (ddata $ ddensify gk) (ddata $ ddensify gradLk)
      kktInf = fmap normInf kkt

  -- print the header and iterations and everything
  when (iter `mod` 10 == 0) $ putStrLn $ printHeader ipKkt
  putStrLn $ printLine ipKkt (iter, kktInf)

  -- check for convergence
  if | sqpConverged kktInf -> do
     putStrLn "============================= sqp converged ====================================="
     return (sqpIn, sqpOut, kktInf)

     | iter == 500 -> do
     putStrLn "=========================== out of iterations =================================="
     return (sqpIn, sqpOut, kktInf)

--     | xk == xkp1 -> do
--     putStrLn "========================= not converged, but x unchanged ======================="
--     return (sqpIn, sqpOut, kktInf)

     | otherwise -> do
      debug "\n\n------------------------ STARTING A NEW ITERATION ------------------------------"
      debug $ "xk: " ++ show xk
      debug $ "sqpIn: " ++ show sqpIn
      debug $ "sqpOut: " ++ show sqpOut

      let deltaxBnds = toDeltaXBnds bx xk
      updateQp env lp gradFk jacGk hessLk gk deltaxBnds bg

      statusOpt <- qpopt env lp
      case statusOpt of
        Nothing -> return ()
        Just msg -> error $ "CPXqpopt error: " ++ msg

      statusSol <- getSolution env lp
      sol <- case statusSol of
        Left msg -> error $ "CPXsolution error: " ++ msg
        Right sol' -> return sol'
      debug $ summarizeQp sol

      let pk = V.fromList $ VS.toList (solX sol)
          lambdaGkp1Hat = V.fromList $ VS.toList (solPi sol)
          lambdaXkp1Hat = V.fromList $ VS.toList (solDj sol)
          sigma = 100
          projGrad = V.sum (V.zipWith (*) (ddata (ddensify gradFk)) pk) - sigma*(norm1 (kktXPrimal kkt) + norm1 (kktGPrimal kkt))
          meritFun x = do
            SqpOut' f0' g0 <- fmap (fmap (ddata . ddensify)) $ evalSXFun sqp' (fmap dvector (SqpIn' x p0))
            let f0 = V.head f0'
                xViol = V.sum $ V.map abs $ fst $ V.unzip $ feasibility  x  x bx
                gViol = V.sum $ V.map abs $ fst $ V.unzip $ feasibility g0 g0 bg

            return $ f0 + sigma*(xViol + gViol)

      lineSearchRet <- lineSearch meritFun xk projGrad pk
      case lineSearchRet of
        Left err -> error $ "line search fail: " ++ err
        Right (xkp1, t) -> do
          let lambdaXkp1 = V.zipWith (+) lambdaXk $ V.map (*t) (V.zipWith (-) lambdaXkp1Hat lambdaXk)
              lambdaGkp1 = V.zipWith (+) lambdaGk $ V.map (*t) (V.zipWith (-) lambdaGkp1Hat lambdaGk)

          runSqpIter (iter + 1) lineSearch bx bg env lp sqp sqp' p0 xkp1 lambdaXkp1 lambdaGkp1

updateQp
  :: CpxEnv -> CpxLp -> DMatrix -> DMatrix -> DMatrix -> DMatrix -> V.Vector (Col, Bound)
     -> V.Vector (Maybe Double, Maybe Double)
     -> IO ()
updateQp env lp gradF0 jacG0 hessL0 g0 deltaxBnds bg = do
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
     -> V.Vector Double
     -> V.Vector Double
     -> Kkt (V.Vector Double)
toKkt xbnd lambdaX x0 gbnd lambdaG g0 gradL =
  Kkt { kktStationarity = gradL
      , kktXPrimal = xPrimal
      , kktXDual   = xDual
      , kktXComplimentarity   = V.zipWith (*) xPrimal xDual
      , kktGPrimal = gPrimal
      , kktGDual   = gDual
      , kktGComplimentarity   = V.zipWith (*) gPrimal gDual
      }
  where
    (xPrimal, xDual) = V.unzip $ feasibility lambdaX x0 xbnd
    (gPrimal, gDual) = V.unzip $ feasibility lambdaG g0 gbnd

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

data IterPrint a = IterPrint [(String, a -> String)]

strip :: String -> String
strip = reverse . strip' . reverse . strip'
  where
    strip' (' ':xs) = strip' xs
    strip' xs = xs

printe :: Int -> Int -> Int -> Double -> String
printe k j e v = printf "%*s" k $ strip $ toExp $ printf "%*.*E" k j v
  where
    toExp ('E':xs) = 'E' : printf "%+0*d" (e+1) (read xs :: Int)
    toExp (x:xs) = x : toExp xs
    toExp [] = []

ipKkt :: IterPrint (Int, Kkt Double)
ipKkt = IterPrint
        [ (printf "%6.6s" "iter ", \(k, _) -> printf "%5d" k)
        , (printf "%9.9s" "|∇L|  ", \(_, Kkt {kktStationarity = x}) -> printe 9 2 2 x)
        , (printf "%9.9s" "|xp|  ", \(_, Kkt {kktXPrimal = x})     -> printe 9 2 2 x)
        , (printf "%9.9s" "|xd|  ", \(_, Kkt {kktXDual = x})       -> printe 9 2 2 x)
        , (printf "%9.9s" "|gp|  ", \(_, Kkt {kktGPrimal = x})     -> printe 9 2 2 x)
        , (printf "%9.9s" "|gd|  ", \(_, Kkt {kktGDual = x})       -> printe 9 2 2 x)
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

norm1 :: Num a => V.Vector a -> a
norm1 x = V.sum (V.map abs x)

normInf :: (Ord a, Num a) => V.Vector a -> a
normInf x
  | V.null x = 0
  | otherwise = V.maximum (V.map abs x)
