{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds, KindSignatures #-}

module Hascm.OcpMonad
--       ( OcpMonad
--       , (===)
--       , (<==)
----       , leq3
--       , minimize
--       , designVar
--       , buildNlp
--       , buildNlp'
--       , reifyNlp
--       )
       where

import Control.Lens ( Lens', over )
import Control.Monad ( when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState )
import qualified Control.Monad.State as State
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
--import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Data.Sequence ( (|>) )
import qualified Data.Vector as V
import Data.Proxy
import Hascm.Ocp

import Hascm.Vectorize
import Hascm.TypeVecs ( Vec )
import qualified Hascm.TypeVecs as TV

import Hascm.Interface.LogsAndErrors
import Hascm.Interface.Types hiding ( NlpState(..) )

--import Hascm.AlgorithmV
import Dvda.Expr
--import Dvda.Algorithm.Construct ( Algorithm(..), AlgOp(..) )
--import Dvda.Algorithm.FunGraph ( Node(..) )

--import Dvda.Algorithm

withEllipse :: Int -> String -> String
withEllipse n blah
  | length blah <= n = blah
  | otherwise = take n blah ++ "..."

newtype OcpMonad a =
  OcpMonad
  { runOcp :: ErrorT ErrorMessage (WriterT [LogMessage] (State OcpState)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState OcpState
             , MonadWriter [LogMessage]
             )

newtype PCMonad a =
  PCMonad
  { runPc :: ErrorT ErrorMessage (WriterT [LogMessage] (State PCState)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState PCState
             , MonadWriter [LogMessage]
             )

newtype BCMonad a =
  BCMonad
  { runBc :: ErrorT ErrorMessage (WriterT [LogMessage] (State BCState)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState BCState
             , MonadWriter [LogMessage]
             )

newtype DaeMonad a =
  DaeMonad
  { runDae :: ErrorT ErrorMessage (WriterT [LogMessage] (State DaeState)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState DaeState
             , MonadWriter [LogMessage]
             )

emptySymbolicDae :: DaeState
emptySymbolicDae = DaeState S.empty S.empty S.empty S.empty S.empty HS.empty S.empty

buildDae' :: DaeState -> DaeMonad a -> (Either ErrorMessage a, [LogMessage], DaeState)
buildDae' nlp0 builder = (result, logs, state)
  where
    ((result,logs),state) =
      flip runState nlp0 . runWriterT . runErrorT . runDae $ builder

buildDae :: DaeMonad a -> (Either ErrorMessage a, [LogMessage], DaeState)
buildDae = buildDae' emptySymbolicDae


newDaeVariable ::
  (MonadState DaeState m, MonadError ErrorMessage m, MonadWriter [LogMessage] m)
  => String -> Lens' DaeState (S.Seq Sym) -> String -> m (Expr a)
newDaeVariable description lens name = do
  debug $ "adding " ++ description ++ " \""++name++"\""
  state0 <- State.get
  let map0 = daeNameSet state0
      sym' = Sym name
  when (HS.member name map0) $ err $ name ++ " already in name set"
  let state1 = state0 { daeNameSet =  HS.insert name map0 }
      state2 = over lens (|> sym') state1
  State.put state2
  return (ESym sym')


diffState :: String -> DaeMonad (Expr Double, Expr Double)
diffState name = do
  x <- newDaeVariable "differential state" daeX name
  xdot <- newDaeVariable "differential state derivative" daeXDot ("ddt( " ++ name ++ " )")
  return (x, xdot)

algVar :: String -> DaeMonad (Expr Double)
algVar = newDaeVariable "algebraic variable" daeZ

control :: String -> DaeMonad (Expr Double)
control = newDaeVariable "control" daeU

parameter :: String -> DaeMonad (Expr Double)
parameter = newDaeVariable "parameter" daeP


infix 4 ===
class EqMonad m where
  (===) :: Expr Double -> Expr Double -> m ()

instance EqMonad DaeMonad where
  (===) lhs rhs = do
    debug $ "adding equality constraint: " ++
      withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { daeConstraints = daeConstraints state0 |> (lhs, rhs) }

instance EqMonad OcpMonad where
  (===) lhs rhs = do
    debug $ "adding equality constraint: " ++
      withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { ocpConstraints = ocpConstraints state0 |> (Eq2 lhs rhs) }


emptySymbolicOcp :: OcpState
emptySymbolicOcp = OcpState S.empty S.empty S.empty S.empty S.empty HS.empty S.empty ObjectiveUnset HomotopyParamUnset

buildOcp' :: OcpState -> OcpMonad a -> (Either ErrorMessage a, [LogMessage], OcpState)
buildOcp' nlp0 builder = (result, logs, state)
  where
    ((result,logs),state) =
      flip runState nlp0 . runWriterT . runErrorT . runOcp $ builder

buildOcp :: OcpMonad a -> (Either ErrorMessage a, [LogMessage], OcpState)
buildOcp = buildOcp' emptySymbolicOcp


infix 4 <==
class LeqMonad m where
  (<==) :: Expr Double -> Expr Double -> m ()

instance LeqMonad OcpMonad where
  (<==) lhs rhs = do
    debug $ "adding inequality constraint: " ++
      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { ocpConstraints = ocpConstraints state0 |> (Ineq2 lhs rhs) }

instance LeqMonad PCMonad where
  (<==) lhs rhs = do
    debug $ "adding inequality constraint: " ++
      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { pcConstraints = pcConstraints state0 |> (Ineq2 lhs rhs) }

instance EqMonad PCMonad where
  (===) lhs rhs = do
    debug $ "adding inequality constraint: " ++
      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { pcConstraints = pcConstraints state0 |> (Eq2 lhs rhs) }

instance EqMonad BCMonad where
  (===) lhs rhs = do
    debug $ "adding inequality constraint: " ++
      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { bcConstraints = bcConstraints state0 |> (Eq2 lhs rhs) }

instance LeqMonad BCMonad where
  (<==) lhs rhs = do
    debug $ "adding inequality constraint: " ++
      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { bcConstraints = bcConstraints state0 |> (Ineq2 lhs rhs) }

--leq3 :: Expr Double -> Expr Double -> Expr Double -> OcpMonad ()
--leq3 lhs mid rhs = do
--  debug $ "adding inequality constraint bounds: " ++
--    withEllipse 30 (show lhs) ++ " <= " ++
--    withEllipse 30 (show mid) ++ " <= " ++
--    withEllipse 30 (show rhs)
--  state0 <- State.get
--  State.put $ state0 { nlpConstraints = nlpConstraints state0 |> (Ineq3 lhs mid rhs) }

minimize :: Expr Double -> OcpMonad ()
minimize obj = do
  debug $ "setting objective function: " ++ withEllipse 30 (show obj)
  state0 <- State.get
  case ocpObj state0 of
    Objective x -> err $ init $ unlines
                   [ "you set the objective function twice"
                   , "    old val: " ++ show x
                   , "    new val: " ++ show obj
                   ]
    ObjectiveUnset -> State.put $ state0 { ocpObj = Objective obj }


--constr :: (Eq a, Num a) => Constraint (Expr a) -> (Expr a, (Maybe Double, Maybe Double))
--constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
--constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))
--
--
--toG :: (Eq a, Num a, Dim ng) => S.Seq (Constraint (Expr a)) -> Vec ng (Expr a, (Maybe Double, Maybe Double))
--toG nlpConstraints' = TV.mkSeq $ fmap constr (nlpConstraints')
--
--convertAlgorithm :: Floating a => Algorithm Double -> Algorithm a
--convertAlgorithm alg = alg { algOps = newAlgOps }
--  where
--    newAlgOps = map convert (algOps alg)
--
--    convert :: Floating a => AlgOp Double -> AlgOp a
--    convert (InputOp k x) = InputOp k x
--    convert (OutputOp k x) = OutputOp k x
--    convert (NormalOp k x) = NormalOp k (convertG x)
--
--    convertG :: Floating a => GExpr Double Node -> GExpr a Node
--    convertG (GSym x) = GSym x
--    convertG (GConst c) = GConst (fromRational (toRational c))
--    convertG (GNum x) = GNum x
--    convertG (GFractional x) = GFractional x
--    convertG (GFloating x) = GFloating x
--
--buildNlp :: forall nx ng .
--            (Dim nx, Dim ng) =>
--            OcpMonad () -> IO (Nlp (Vec nx) None (Vec ng), [LogMessage])
--buildNlp nlp = do
--  let (_,logs,state) = build nlp
--  obj <- case nlpObj state of
--    Objective obj' -> return obj'
--    ObjectiveUnset -> error "solveNlp: objective unset"
--
--  let inputs :: [Expr Double]
--      inputs = map ESym (F.toList (nlpX state))
--
--      g :: Vec ng (Expr Double)
--      gbnd :: Vec ng (Maybe Double, Maybe Double)
--      (g, gbnd) = TV.tvunzip $ toG (nlpConstraints state)
--
--      xbnd :: Vec nx (Maybe Double, Maybe Double)
--      xbnd = fill (Nothing, Nothing)
--      nlpFun = NlpFun obj g
--  alg' <- constructAlgorithm (V.fromList inputs) (vectorize nlpFun)
--  let alg :: forall b . Floating b => Algorithm b
--      alg = convertAlgorithm alg'
--      fg :: forall b . Floating b => NlpInputs (Vec nx) None b -> NlpFun (Vec ng) b
--      fg (NlpInputs x _) = devectorize $ runAlgorithm alg (vectorize x)
--  --mapM_ print (algOps alg)
--  --print inputs
--
--      nlp' = Nlp { nlpFG = fg
--                 , nlpBX = xbnd
--                 , nlpBG = gbnd
--                 , nlpX0 = fmap (const 0) xbnd
--                 , nlpP = None
--                 }
--  return (nlp', logs)
--
--
--
--toG' :: (Eq a, Num a) => S.Seq (Constraint (Expr a)) -> V.Vector (Expr a, (Maybe Double, Maybe Double))
--toG' nlpConstraints' = V.fromList $ F.toList $ fmap constr (nlpConstraints')
--
--
--buildNlp' :: OcpMonad a -> IO (Nlp V.Vector V.Vector V.Vector, [LogMessage])
--buildNlp' nlp = do
--  let (_,logs,state) = build nlp
--  obj <- case nlpObj state of
--    Objective obj' -> return obj'
--    ObjectiveUnset -> error "solveNlp: objective unset"
--
--  let inputs :: [Expr Double]
--      inputs = map ESym (F.toList (nlpX state))
--      nx = length inputs
--
--      parameters :: [Expr Double]
--      parameters = []
--      np = 0
--
--      g :: V.Vector (Expr Double)
--      gbnd :: V.Vector (Maybe Double, Maybe Double)
--      (g, gbnd) = V.unzip $ toG' (nlpConstraints state)
--      ng = V.length g
--
--      xbnd :: V.Vector (Maybe Double, Maybe Double)
--      xbnd = V.replicate nx (Nothing, Nothing)
--  alg' <- constructAlgorithm (V.fromList inputs V.++ V.fromList parameters) (V.singleton obj V.++ g)
--  let alg :: forall b . Floating b => Algorithm b
--      alg = convertAlgorithm alg'
--      fg :: forall b . Floating b => NlpInputs V.Vector V.Vector b -> NlpFun V.Vector b
--      fg (NlpInputs x p)
--        | V.length x /= nx = error $ "static nlp: V.length x /= nx " ++ show (V.length x, nx)
--        | V.length p /= np = error $ "static nlp: V.length p /= np " ++ show (V.length p, np)
--        | V.length g' /= ng = error $ "static nlp: V.length g /= ng " ++ show (V.length g', ng)
--        | otherwise = NlpFun (V.head vout) g'
--        where
--          g' = V.tail vout
--          vout = runAlgorithm alg (x V.++ p)
--      nlp' = Nlp { nlpFG = fg
--                 , nlpBX = xbnd
--                 , nlpBG = gbnd
--                 , nlpX0 = fmap (const 0) xbnd
--                 , nlpP = V.empty
--                 }
--  return (nlp', logs)
--
--
--
--reifyNlp ::
--  forall r .
--  Nlp V.Vector V.Vector V.Vector ->
--  (forall nx np ng . (Dim nx, Dim np, Dim ng) => Nlp (Vec nx) (Vec np) (Vec ng) -> r) ->
--  r
--reifyNlp nlp0 f =
--  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
--  TV.reifyDim np $ \(Proxy :: Proxy np) ->
--  TV.reifyDim ng $ \(Proxy :: Proxy ng) ->
--  f (Nlp
--     ((\(NlpInputs x' p') ->
--        fout devectorize (fg (NlpInputs (vectorize x') (vectorize p')))) :: forall a . Floating a => NlpInputs (Vec nx) (Vec np) a -> NlpFun (Vec ng) a)
--     (TV.mkVec bx :: Vec nx (Maybe Double, Maybe Double))
--     (TV.mkVec bg :: Vec ng (Maybe Double, Maybe Double))
--     (TV.mkVec x0 :: Vec nx Double)
--     (TV.mkVec p :: Vec np Double)
--    )
--  where
--    fout :: (f a -> g a) -> NlpFun f a -> NlpFun g a
--    fout f' (NlpFun obj g) = NlpFun obj (f' g)
--
--    nx = V.length bx
--    ng = V.length bg
--    np = V.length p
--
--    bx = nlpBX nlp0
--    bg = nlpBG nlp0
--    x0 = nlpX0 nlp0
--    p = nlpP nlp0
--
--    fg :: forall a . Floating a => NlpInputs V.Vector V.Vector a -> NlpFun V.Vector a
--    fg = nlpFG nlp0

newVariable ::
  (MonadState OcpState m, MonadError ErrorMessage m, MonadWriter [LogMessage] m)
  => String -> Lens' OcpState (S.Seq Sym) -> String -> m (Expr a)
newVariable description lens name = do
  debug $ "adding " ++ description ++ " \""++name++"\""
  state0 <- State.get
  let map0 = ocpSymSet state0
      sym' = Sym name
  when (HS.member sym' map0) $ err $ name ++ " already in symbol map"
  let state1 = state0 { ocpSymSet =  HS.insert sym' map0 }
      state2 = over lens (|> sym') state1
  State.put state2
  return (ESym sym')


diffState' :: String -> OcpMonad (Expr Double, Expr Double)
diffState' name = do
  x <- newVariable "differential state" ocpX name
  xdot <- newVariable "differential state derivative" ocpXDot ("ddt( " ++ name ++ " )")
  return (x, xdot)

algVar' :: String -> OcpMonad (Expr Double)
algVar' = newVariable "algebraic variable" ocpZ

control' :: String -> OcpMonad (Expr Double)
control' = newVariable "control" ocpU

parameter' :: String -> OcpMonad (Expr Double)
parameter' = newVariable "parameter" ocpP

myDae :: DaeMonad ()
myDae = do
  (p,p') <- diffState "p"
  (v,v') <- diffState "v"
  u <- control "u"

  let k = 4
      b = 0.3

  p' === v
  v' === (u - k*p - b*v)


pathConstraints :: (String -> BCMonad (Expr Double)) -> BCMonad ()
pathConstraints get = do
  p <- get "x"
  v <- get "v"
  u <- get "u"
  
  v**v + u**2 <== 4
  p*v <== p/v + 200


boundaryConditions :: (String -> BCMonad (Expr Double)) -> (String -> BCMonad (Expr Double)) -> BCMonad ()
boundaryConditions get0 getF = do
  p0 <- get0 "x"
  v0 <- get0 "v"
  
  pF <- getF "p"
  vF <- getF "v"

  p0 === 0
  v0 === 0

  p0 + 4 <== pF
  v0 === vF

mayer :: (Floating b, Monad m) => (String -> m b) -> m b
mayer get = do
  p <- get "p"
  v <- get "v"

  return (p**2 + v**2)
  
lagrange :: (Floating b, Monad m) => (String -> m b) -> m b
lagrange get = do
  p <- get "p"
  v <- get "v"
  u <- get "u"

  return (p**2 + v**2 + u**2)
  
  
-- | This function will call ocpDae, ocpBc, ocpPc on all-zero input vectors to get output dimensions,
-- so make sure this doesn't throw an exception on divide-by-zero. The outputs are never evaluated
-- so lazy divide-by-zero is totally fine.
reifyOcp ::
  forall a ret .
  Num a =>
  OcpPhase V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector a
  -> (forall x z u p r c h . (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize c, Vectorize h)
      => OcpPhase x z u p r c h a -> ret)
  -> ret
reifyOcp ocp f =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim nr $ \(Proxy :: Proxy nr) ->
  TV.reifyDim nc $ \(Proxy :: Proxy nc) ->
  TV.reifyDim nh $ \(Proxy :: Proxy nh) ->
  f (OcpPhase
     { ocpMayer = (\x t -> ocpMayer ocp (vectorize x) t) :: Vec nx a -> a -> a
     , ocpLagrange = (\x z u p t -> ocpLagrange ocp (vectorize x) (vectorize z) (vectorize u) (vectorize p) t) :: Vec nx a -> Vec nz a -> Vec nu a -> Vec np a -> a -> a
     , ocpDae = (\x' x z u p t -> devectorize (ocpDae ocp (vectorize x') (vectorize x) (vectorize z) (vectorize u) (vectorize p) t)) :: Dae (Vec nx) (Vec nz) (Vec nu) (Vec np) (Vec nr) a
     , ocpBc = (\x0 xf -> devectorize (ocpBc ocp (vectorize x0) (vectorize xf))) :: Vec nx a -> Vec nx a -> Vec nc a
     , ocpPathC = (\x z u p t -> devectorize (ocpPathC ocp (vectorize x) (vectorize z) (vectorize u) (vectorize p) t)) :: Vec nx a -> Vec nz a -> Vec nu a -> Vec np a -> a -> Vec nh a
     , ocpPathCBnds = devectorize (ocpPathCBnds ocp) :: Vec nh (Maybe Double, Maybe Double)
     , ocpXbnd = TV.mkVec (ocpXbnd ocp) :: Vec nx (Maybe Double, Maybe Double)
     , ocpZbnd = TV.mkVec (ocpZbnd ocp) :: Vec nz (Maybe Double, Maybe Double)
     , ocpUbnd = TV.mkVec (ocpUbnd ocp) :: Vec nu (Maybe Double, Maybe Double)
     , ocpPbnd = TV.mkVec (ocpPbnd ocp) :: Vec np (Maybe Double, Maybe Double)
     , ocpTbnd = ocpTbnd ocp
     }
    )
  where
    nx = V.length (ocpXbnd ocp)
    nz = V.length (ocpZbnd ocp)
    nu = V.length (ocpUbnd ocp)
    np = V.length (ocpPbnd ocp)
    nh = V.length (ocpPathCBnds ocp)
    (nr, nc) = ( V.length (ocpDae ocp x' x z u p t)
               , V.length (ocpBc ocp x x)
               )
      where
        x' = V.replicate nx 0
        x = x'
        z = V.replicate nz 0
        u = V.replicate nu 0
        p = V.replicate np 0
        t = 0
