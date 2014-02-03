{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}


module Hascm.OcpMonad
       ( OcpMonad
       , EqMonad(..)
       , LeqMonad(..)
       , DaeMonad
       , BCMonad
       , diffState
       , algVar
       , control
       , parameter
       , output
       , lagrangeTerm
       , buildOcpPhase
       , reifyOcp
       )
       where

import Control.Arrow( (***) )
import Control.Lens ( Lens', over )
import Control.Monad ( when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState )
import qualified Control.Monad.State as State
import Control.Monad.Writer ( WriterT, Writer, MonadWriter, runWriterT, runWriter )
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Sequence ( (|>) )
import qualified Data.Vector as V
import Data.Proxy
import System.IO.Unsafe

import Dvda.Algorithm
import Dvda.Expr

import Hascm.Ocp
import Hascm.AlgorithmV ( convertAlgorithm )
import Hascm.Vectorize
import Hascm.TypeVecs ( Vec )
import qualified Hascm.TypeVecs as TV
import Hascm.DirectCollocation.Dynamic ( CollTrajMeta(..), NameTree(..) )

import Hascm.Interface.LogsAndErrors
import Hascm.Interface.Types hiding ( NlpState(..) )

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

newtype BCMonad b a =
  BCMonad
--  { runBc :: ErrorT ErrorMessage (WriterT [LogMessage] (State (S.Seq (Constraint (Expr Double))))) a
  { runBc :: ErrorT ErrorMessage (WriterT [LogMessage] (State (S.Seq (b,b)))) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState (S.Seq (b,b))
--             , MonadState (S.Seq (Constraint (Expr Double)))
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
emptySymbolicDae = DaeState S.empty S.empty S.empty S.empty S.empty M.empty HS.empty S.empty

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

output :: String -> Expr Double -> DaeMonad ()
output name expr = do
  debug $ "adding output \""++name++"\": " ++ withEllipse 30 (show expr)
  state0 <- State.get
  let nameSet0 = daeNameSet state0
      outputs0 = _daeO state0
  when (HS.member name nameSet0) $ err $ name ++ " already in name set"
  when (M.member name outputs0) $ impossible $ name ++ " already in output map"
  let state1 = state0 { daeNameSet =  HS.insert name nameSet0
                      , _daeO = M.insert name expr outputs0
                      }
  State.put state1

infix 4 ===
class EqMonad m a | m -> a where
  (===) :: a -> a -> m ()

instance EqMonad DaeMonad (Expr Double) where
  (===) lhs rhs = do
    debug $ "adding equality constraint: " ++
      withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { daeConstraints = daeConstraints state0 |> (lhs, rhs) }

instance EqMonad OcpMonad (Expr Double) where
  (===) lhs rhs = do
    debug $ "adding equality constraint: " ++
      withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { ocpPathConstraints = ocpPathConstraints state0 |> Eq2 lhs rhs }


infix 4 <==
class LeqMonad m where
  (<==) :: Expr Double -> Expr Double -> m ()

instance LeqMonad OcpMonad where
  (<==) lhs rhs = do
    debug $ "adding inequality constraint: " ++
      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { ocpPathConstraints = ocpPathConstraints state0 |> Ineq2 lhs rhs }

instance EqMonad (BCMonad a) a where
  (===) lhs rhs = do
    debug "adding inequality constraint: "
      -- ++ withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    (state0 :: S.Seq (a,a)) <- State.get
    State.put $ state0  |> (lhs, rhs)
--    State.put $ state0  |> (Eq2 lhs rhs)

--instance LeqMonad BCMonad where
--  (<==) lhs rhs = do
--    debug $ "adding inequality constraint: " ++
--      withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
--    state0 <- State.get
--    State.put $ state0 |> (Ineq2 lhs rhs)

--leq3 :: Expr Double -> Expr Double -> Expr Double -> OcpMonad ()
--leq3 lhs mid rhs = do
--  debug $ "adding inequality constraint bounds: " ++
--    withEllipse 30 (show lhs) ++ " <= " ++
--    withEllipse 30 (show mid) ++ " <= " ++
--    withEllipse 30 (show rhs)
--  state0 <- State.get
--  State.put $ state0 { nlpConstraints = nlpConstraints state0 |> (Ineq3 lhs mid rhs) }


--constr :: (Eq a, Num a) => Constraint (Expr a) -> (Expr a, (Maybe Double, Maybe Double))
--constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
--constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))
--
--
--toG :: (Eq a, Num a, Dim ng) => S.Seq (Constraint (Expr a)) -> Vec ng (Expr a, (Maybe Double, Maybe Double))
--toG nlpConstraints' = TV.mkSeq $ fmap constr (nlpConstraints')
--
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

lagrangeTerm :: Expr Double -> OcpMonad ()
lagrangeTerm obj = do
  debug $ "setting lagrange term: " ++ withEllipse 30 (show obj)
  state0 <- State.get
  case ocpLagrangeObj state0 of
    Objective x -> err $ init $ unlines
                   [ "you set the lagrange objective function twice"
                   , "    old val: " ++ withEllipse 30 (show x)
                   , "    new val: " ++ withEllipse 30 (show obj)
                   ]
    ObjectiveUnset -> State.put $ state0 { ocpLagrangeObj = Objective obj }



emptySymbolicOcp :: OcpState
emptySymbolicOcp = OcpState S.empty ObjectiveUnset HomotopyParamUnset

constr :: (Eq a, Num a) => Constraint (Expr a) -> (Expr a, (Maybe Double, Maybe Double))
constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))

buildOcpPhase ::
  DaeMonad ()
  -> (forall a m . (Floating a, Monad m) => (String -> m a) -> a -> m a)
  -> (forall a . Floating a => (String -> BCMonad a a) -> (String -> BCMonad a a) -> BCMonad a ())
  -> ((String -> OcpMonad (Expr Double)) -> OcpMonad ())
  -> (Maybe Double, Maybe Double)
  -> (OcpPhase V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector, Int -> Int -> CollTrajMeta)
buildOcpPhase daeMonad mayerMonad bcMonad ocpMonad tbnds =
  (OcpPhase { ocpMayer = mayerFun
            , ocpLagrange = lagrangeFun
            , ocpDae = daeFun
            , ocpBc = bcFun
            , ocpPathC = pathConstraintFun
            , ocpPathCBnds = V.fromList pathConstraintBnds
            , ocpXbnd = V.replicate (length xnames) (Nothing, Nothing)
            , ocpZbnd = V.replicate (length znames) (Nothing, Nothing)
            , ocpUbnd = V.replicate (length unames) (Nothing, Nothing)
            , ocpPbnd = V.replicate (length pnames) (Nothing, Nothing)
            , ocpTbnd = tbnds
            },
   \n deg -> CollTrajMeta { ctmX = NameTreeNode ("", "") (zip (map show xnames) (map NameTreeLeaf [0..]))
                          , ctmZ = NameTreeNode ("", "") (zip (map show znames) (map NameTreeLeaf [0..]))
                          , ctmU = NameTreeNode ("", "") (zip (map show unames) (map NameTreeLeaf [0..]))
                          , ctmP = NameTreeNode ("", "") (zip (map show pnames) (map NameTreeLeaf [0..]))
                          , ctmN = n
                          , ctmDeg = deg
                          }
  )
  where
    dae :: DaeState
    dae = case buildDae daeMonad of
      (Left errmsg, _, _) -> error $ "toOcpPhase: buildDae failure: " ++ show errmsg
      (_, _, daeState) -> daeState

    xdotnames, xnames, znames, unames, pnames :: [Sym]
    xdotnames = F.toList $ _daeXDot dae
    xnames = F.toList $ _daeX dae
    znames = F.toList $ _daeZ dae
    unames = F.toList $ _daeU dae
    pnames = F.toList $ _daeP dae
    daeResidual :: [Expr Double]
    daeResidual = map (uncurry (-)) (F.toList (daeConstraints dae))

    outputNames :: [String]
    outputExprs :: [Expr Double]
    (outputNames, outputExprs) = unzip $ M.toList $ _daeO dae

    onames = map Sym outputNames

    lagrangeFun :: forall a. Floating a =>
                   V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a -> a -> a
    lagrangeFun x z u p o _ = case runAlgorithm alg (V.concat [x,z,u,p,o]) of
      Right ret -> V.head ret
      Left errmsg -> error $ "toOcpPhase: lagrangeFun: " ++ errmsg ++
        "\ninputs: " ++ show (xnames ++ znames ++ unames ++ pnames) ++ show onames ++
        "\nnumeric inputs x: " ++ show (V.length x) ++
        "\nnumeric inputs z: " ++ show (V.length z) ++
        "\nnumeric inputs u: " ++ show (V.length u) ++
        "\nnumeric inputs p: " ++ show (V.length p) ++
        "\nnumeric inputs o: " ++ show (V.length o)
      where
        obj = case ocpLagrangeObj ocp of
          ObjectiveUnset -> 0
          Objective obj' -> obj'

        algInputs = map ESym (xnames ++ znames ++ unames ++ pnames ++ onames)
        alg :: Algorithm a
        alg =
          convertAlgorithm $ unsafePerformIO $
          constructAlgorithm (V.fromList algInputs) (V.singleton obj)
        {-# NOINLINE alg #-}
    pathConstraints :: [Expr Double]
    pathConstraintBnds :: [(Maybe Double, Maybe Double)]
    (pathConstraints, pathConstraintBnds) = unzip $ map constr (F.toList (ocpPathConstraints ocp))

    pathConstraintFun ::
      forall a . Floating a =>
      V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a -> a -> V.Vector a
    pathConstraintFun x z u p o _ = case runAlgorithm alg (V.concat [x,z,u,p,o]) of
      Right ret -> ret
      Left errmsg -> error $ "toOcpPhase: pathConstraintFun: " ++ errmsg
      where
        algInputs = map ESym (xnames ++ znames ++ unames ++ pnames ++ onames)
        alg :: Algorithm a
        alg =
          convertAlgorithm $ unsafePerformIO $
          constructAlgorithm (V.fromList algInputs) (V.fromList pathConstraints)
        {-# NOINLINE alg #-}


    ocp :: OcpState
    ocp =
      case flip runState emptySymbolicOcp $ runWriterT $ runErrorT (runOcp (ocpMonad lookupThingy)) of
        ((Left errmsg, logs),_) ->
          error $ unlines $ ("" : map show logs) ++ ["","ocp monad failure: " ++ show errmsg]
        ((Right _, _), ocpState) -> ocpState
      where
        varmap :: M.Map Sym (Expr Double)
        varmap = M.fromList $ concatMap (\names -> zip names (map ESym names))
                 [ xnames
                 , znames
                 , unames
                 , pnames
                 , onames
                 ]

        lookupThingy :: String -> OcpMonad (Expr Double)
        lookupThingy name = do
          debug $ "ocp monad: looking up \"" ++ name ++ "\""
          case M.lookup (Sym name) varmap of
            Nothing -> err $ "ocp monad: nothing named \"" ++ name ++ "\""
            Just expr -> do
              debug $ "ocp monad: found \"" ++ name ++ "\": " ++ show expr
              return expr

    daeFun ::
      forall a . Floating a =>
      V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a -> a ->
      (V.Vector a, V.Vector a)
    daeFun x' x z u p _ = case runAlgorithm alg (V.concat [x',x,z,u,p]) of
      Right ret -> V.splitAt (length daeResidual) ret
      Left errmsg -> error $ "toOcpPhase: daeFun: " ++ errmsg
      where
        algInputs = map ESym (xdotnames ++ xnames ++ znames ++ unames ++ pnames)
        algOutputs = daeResidual ++ outputExprs
        alg :: Algorithm a
        alg =
          convertAlgorithm $ unsafePerformIO $
          constructAlgorithm (V.fromList algInputs) (V.fromList algOutputs)
        {-# NOINLINE alg #-}


    mayerFun :: forall a. Floating a => V.Vector a -> a -> a
    mayerFun x t =
      case runWriter (runErrorT (mayerMonad lookupState t)) of
        (Left errmsg, logs) ->
          error $ unlines $ ("" : map show logs) ++ ["","mayer monad failure: " ++ show errmsg]
        (Right ret, _) -> ret
      where
        xmap :: M.Map Sym a
        xmap = M.fromList $ zip xnames (V.toList x)

        lookupState :: String -> ErrorT ErrorMessage (Writer [LogMessage]) a
        lookupState name = do
          debug $ "mayer monad: looking up \"" ++ name ++ "\""
          case M.lookup (Sym name) xmap of
            Nothing -> err $ "mayer monad: no state named \"" ++ name ++ "\""
            Just expr -> do
              debug $ "mayer monad: found \"" ++ name ++ "\""
              return expr


    bcFun :: forall a . Floating a => V.Vector a -> V.Vector a -> V.Vector a
    bcFun x0 xF =
      case flip runState S.empty $ runWriterT (runErrorT (runBc $ bcMonad lookupState0 lookupStateF)) of
        ((Left errmsg, logs),_) ->
          error $ unlines $ ("" : map show logs) ++ ["","boundary condition monad failure: " ++ show errmsg]
        ((Right _,_), ret) -> V.fromList $ map (uncurry (-)) $ F.toList ret
      where
        x0map :: M.Map Sym a
        x0map = M.fromList $ zip xnames (V.toList x0)

        xFmap :: M.Map Sym a
        xFmap = M.fromList $ zip xnames (V.toList xF)

        lookupState0 :: String -> BCMonad a a
        lookupState0 name = do
          debug $ "boundary condition monad: looking up initial \"" ++ name ++ "\""
          case M.lookup (Sym name) x0map of
            Nothing -> err $ "boundary condition monad: no state named \"" ++ name ++ "\""
            Just expr -> do
              debug $ "boundary condition monad: found \"" ++ name ++ "\""
              return expr

        lookupStateF :: String -> BCMonad a a
        lookupStateF name = do
          debug $ "boundary condition monad: looking up final \"" ++ name ++ "\""
          case M.lookup (Sym name) xFmap of
            Nothing -> err $ "boundary condition monad: no state named \"" ++ name ++ "\""
            Just expr -> do
              debug $ "boundary condition monad: found \"" ++ name ++ "\""
              return expr



-- | This function will call ocpDae, ocpBc, ocpPc on Double type all-zero input vectors to get output dimensions,
-- so make sure this doesn't throw an exception on divide-by-zero. The outputs are never evaluated
-- so lazy divide-by-zero is totally fine.
reifyOcp ::
  forall ret .
  OcpPhase V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector
  -> (forall x z u p r o c h . (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize o, Vectorize c, Vectorize h)
      => OcpPhase x z u p r o c h -> ret)
  -> ret
reifyOcp ocp f =
  TV.reifyDim nx $ \(Proxy :: Proxy nx) ->
  TV.reifyDim nz $ \(Proxy :: Proxy nz) ->
  TV.reifyDim nu $ \(Proxy :: Proxy nu) ->
  TV.reifyDim np $ \(Proxy :: Proxy np) ->
  TV.reifyDim nr $ \(Proxy :: Proxy nr) ->
  TV.reifyDim no $ \(Proxy :: Proxy no) ->
  TV.reifyDim nc $ \(Proxy :: Proxy nc) ->
  TV.reifyDim nh $ \(Proxy :: Proxy nh) ->
  f OcpPhase
     { ocpMayer = (\x t -> ocpMayer ocp (vectorize x) t) :: forall a . Floating a => Vec nx a -> a -> a
     , ocpLagrange = (\x z u p o t -> ocpLagrange ocp (vectorize x) (vectorize z) (vectorize u) (vectorize p) (vectorize o) t) :: forall a . Floating a => Vec nx a -> Vec nz a -> Vec nu a -> Vec np a -> Vec no a -> a -> a
     , ocpDae = (\x' x z u p t -> (devectorize *** devectorize) (ocpDae ocp (vectorize x') (vectorize x) (vectorize z) (vectorize u) (vectorize p) t)) :: forall a . Floating a => Dae (Vec nx) (Vec nz) (Vec nu) (Vec np) (Vec nr) (Vec no) a
     , ocpBc = (\x0 xf -> devectorize (ocpBc ocp (vectorize x0) (vectorize xf))) :: forall a . Floating a => Vec nx a -> Vec nx a -> Vec nc a
     , ocpPathC = (\x z u p o t -> devectorize (ocpPathC ocp (vectorize x) (vectorize z) (vectorize u) (vectorize p) (vectorize o) t)) :: forall a . Floating a => Vec nx a -> Vec nz a -> Vec nu a -> Vec np a -> Vec no a -> a -> Vec nh a
     , ocpPathCBnds = devectorize (ocpPathCBnds ocp) :: Vec nh (Maybe Double, Maybe Double)
     , ocpXbnd = TV.mkVec (ocpXbnd ocp) :: Vec nx (Maybe Double, Maybe Double)
     , ocpZbnd = TV.mkVec (ocpZbnd ocp) :: Vec nz (Maybe Double, Maybe Double)
     , ocpUbnd = TV.mkVec (ocpUbnd ocp) :: Vec nu (Maybe Double, Maybe Double)
     , ocpPbnd = TV.mkVec (ocpPbnd ocp) :: Vec np (Maybe Double, Maybe Double)
     , ocpTbnd = ocpTbnd ocp
     }
  where
    nx = V.length (ocpXbnd ocp)
    nz = V.length (ocpZbnd ocp)
    nu = V.length (ocpUbnd ocp)
    np = V.length (ocpPbnd ocp)
    nh = V.length (ocpPathCBnds ocp)

    nr = V.length daeRes
    no = V.length daeOut
    ((daeRes, daeOut), nc) = (ocpDae ocp x' x z u p t, V.length (ocpBc ocp x x))
      where
        x' = V.replicate nx 0
        x = x'
        z = V.replicate nz 0
        u = V.replicate nu 0
        p = V.replicate np 0
        t = 0 :: Double
