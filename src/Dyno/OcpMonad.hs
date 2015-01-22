{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PackageImports #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language RankNTypes #-}

module Dyno.OcpMonad
       ( OcpMonad
       , EqMonad(..)
       , LeqMonad(..)
       , DaeMonad
       , BCMonad
       , SXElement
       , diffState
       , algVar
       , control
       , parameter
       , output
       , lagrangeTerm
       , solveStaticOcp
       ) where

import Control.Applicative ( Applicative(..) )
import Control.Lens ( Lens', over )
import Control.Monad ( when )
import qualified "mtl" Control.Monad.State as State
import "mtl" Control.Monad.Reader ( MonadIO(..) )
import "mtl" Control.Monad.Writer ( WriterT, Writer, MonadWriter, runWriterT, runWriter )
import "mtl" Control.Monad.State ( StateT, MonadState, runStateT )
import "mtl" Control.Monad.Except ( ExceptT, MonadError, runExceptT )
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Sequence ( (|>) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Proxy ( Proxy(..) )

import Casadi.Option ( setOption )
import Casadi.SXFunction ( sxFunction )
import Casadi.SX ( SX )
import Casadi.Function ( callSX )
import Casadi.SharedObject ( soInit )

import qualified Dyno.View.CasadiMat as CM
import Dyno.SXElement ( SXElement, sxElementSym, sxElementToSX, sxToSXElement )
import Dyno.Ocp ( OcpPhase(..) )
import Dyno.Nlp ( Bounds )
import Dyno.Vectorize ( Vectorize(..), fill )
import Dyno.View.View ( mkJ )
import Dyno.View.JV ( sxSplitJV )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.NlpSolver ( NlpSolverStuff )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..) )
import Dyno.DirectCollocation.Dynamic ( DynCollTraj, CollTrajMeta(..), NameTree(..) )
import Dyno.DirectCollocation ( solveOcp )

import Dyno.Interface.LogsAndErrors
import Dyno.Interface.Types

--withEllipse :: Int -> String -> String
--withEllipse n blah
--  | length blah <= n = blah
--  | otherwise = take n blah ++ "..."

newtype OcpMonad a =
  OcpMonad
  { runOcp :: ExceptT ErrorMessage (WriterT [LogMessage] (StateT OcpState IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorMessage
             , MonadState OcpState
             , MonadWriter [LogMessage]
             , MonadIO
             )

newtype BCMonad a =
  BCMonad
  { runBc :: ExceptT ErrorMessage (WriterT [LogMessage] (StateT (S.Seq (Constraint SXElement)) IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorMessage
             , MonadState (S.Seq (Constraint SXElement))
             , MonadWriter [LogMessage]
             , MonadIO
             )

newtype DaeMonad a =
  DaeMonad
  { runDae :: ExceptT ErrorMessage (WriterT [LogMessage] (StateT DaeState IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ErrorMessage
             , MonadState DaeState
             , MonadWriter [LogMessage]
             , MonadIO
             )

emptySymbolicDae :: DaeState
emptySymbolicDae = DaeState S.empty S.empty S.empty S.empty S.empty M.empty HS.empty S.empty

buildDae :: DaeMonad a -> IO (Either ErrorMessage a, [LogMessage], DaeState)
buildDae = buildDae' emptySymbolicDae
  where
    buildDae' :: DaeState -> DaeMonad a -> IO (Either ErrorMessage a, [LogMessage], DaeState)
    buildDae' nlp0 builder = do
      ((result,logs),state) <- flip runStateT nlp0 . runWriterT . runExceptT . runDae $ builder
      return (result, logs, state)

newDaeVariable ::
  (MonadState DaeState m, MonadError ErrorMessage m, MonadWriter [LogMessage] m, MonadIO m)
  => String -> Lens' DaeState (S.Seq (String, SXElement)) -> String -> m SXElement
newDaeVariable description lens name = do
  debug $ "adding " ++ description ++ " \""++name++"\""
  case name of [] -> err "name cannot be empty"
               ('_':_) -> err $ "name \"" ++ name ++
                          "\" cannot have leading underscore (this is reserved for internal use)"
               _ -> return ()
  state0 <- State.get
  let map0 = daeNameSet state0
  sym <- liftIO (sxElementSym name)
  when (HS.member name map0) $ err $ name ++ " already in name set"
  let state1 = state0 { daeNameSet =  HS.insert name map0 }
      state2 = over lens (|> (name, sym)) state1
  State.put state2
  return sym

svector :: Vector SXElement -> SX
svector = CM.vertcat . fmap sxElementToSX

diffState :: String -> DaeMonad (SXElement, SXElement)
diffState name = do
  x <- newDaeVariable "differential state" daeX name
  xdot <- newDaeVariable "differential state derivative" daeXDot ("ddt( " ++ name ++ " )")
  return (x, xdot)

algVar :: String -> DaeMonad SXElement
algVar = newDaeVariable "algebraic variable" daeZ

control :: String -> DaeMonad SXElement
control = newDaeVariable "control" daeU

parameter :: String -> DaeMonad SXElement
parameter = newDaeVariable "parameter" daeP

output :: String -> SXElement -> DaeMonad ()
output name expr = do
  debug $ "adding output \""++name++"\""
--  debug $ "adding output \""++name++"\": " ++ withEllipse 30 (show expr)
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

instance EqMonad DaeMonad SXElement where
  (===) lhs rhs = do
    debug $ "adding equality constraint: "
--     ++ withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { daeConstraints = daeConstraints state0 |> (lhs, rhs) }

instance EqMonad OcpMonad SXElement where
  (===) lhs rhs = do
    debug $ "adding equality constraint: "
--     ++ withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { ocpPathConstraints = ocpPathConstraints state0 |> Eq2 lhs rhs }


infix 4 <==
class LeqMonad m where
  (<==) :: SXElement -> SXElement -> m ()

instance LeqMonad OcpMonad where
  (<==) lhs rhs = do
    debug $ "adding inequality constraint: "
--     ++ withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 { ocpPathConstraints = ocpPathConstraints state0 |> Ineq2 lhs rhs }

instance EqMonad BCMonad SXElement where
  (===) lhs rhs = do
    debug $ "adding inequality constraint: "
      -- ++ withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 |> Eq2 lhs rhs

instance LeqMonad BCMonad where
  (<==) lhs rhs = do
    debug $ "adding inequality constraint: "
--      ++ withEllipse 30 (show lhs) ++ " <= " ++ withEllipse 30 (show rhs)
    state0 <- State.get
    State.put $ state0 |> Ineq2 lhs rhs


constr :: Constraint SXElement -> (SXElement, Bounds)
constr (Eq2 lhs rhs) = (lhs - rhs, (Just 0, Just 0))
constr (Ineq2 lhs rhs) = (lhs - rhs, (Nothing, Just 0))
constr (Ineq3 x (lhs,rhs)) = (x, (Just lhs, Just rhs))



lagrangeTerm :: SXElement -> OcpMonad ()
lagrangeTerm obj = do
  debug "setting lagrange term"
  --debug $ "setting lagrange term: " ++ withEllipse 30 (show obj)
  state0 <- State.get
  case ocpLagrangeObj state0 of
    Objective _x -> err $ init $ unlines
                   [ "you set the lagrange objective function twice"
--                   , "    old val: " ++ withEllipse 30 (show x)
--                   , "    new val: " ++ withEllipse 30 (show obj)
                   ]
    ObjectiveUnset -> State.put $ state0 { ocpLagrangeObj = Objective obj }



emptySymbolicOcp :: OcpState
emptySymbolicOcp = OcpState S.empty ObjectiveUnset HomotopyParamUnset

reifyOcpPhase ::
  forall ret .
  (SXElement -> DaeMonad ())
  -> (forall a m . (Floating a, Monad m) => a -> (String -> m a) -> (String -> m a) -> m a)
  -> ((String -> BCMonad SXElement) -> (String -> BCMonad SXElement) -> BCMonad ())
  -> (SXElement -> (String -> OcpMonad SXElement) -> OcpMonad ())
  -> (Maybe Double, Maybe Double)
  -> Int -> Int
  -> (forall x z u p r o c h .
      (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize o, Vectorize c, Vectorize h)
      => OcpPhase x z u p r o c h -> CollTrajMeta -> IO ret)
  -> IO ret
reifyOcpPhase daeMonad mayerMonad bcMonad ocpMonad tbnds n deg f = do
  time <- sxElementSym "_t"
  endT <- sxElementSym "T"
  let time' = sxElementToSX time
      endT' = sxElementToSX endT
  dae' <- buildDae (daeMonad time)
  let dae :: DaeState
      dae = case dae' of
        (Left errmsg, _, _) -> error $ "buildOcpPhase: buildDae failure: " ++ show errmsg
        (_, _, daeState) -> daeState

      xdotnames, xnames, znames, unames, pnames :: Vector String
      xdots, xs, zs, us, ps :: Vector SXElement
      (xdotnames,xdots) = V.unzip $ V.fromList $ F.toList $ _daeXDot dae
      (xnames,xs)       = V.unzip $ V.fromList $ F.toList $ _daeX dae
      (znames,zs)       = V.unzip $ V.fromList $ F.toList $ _daeZ dae
      (unames,us)       = V.unzip $ V.fromList $ F.toList $ _daeU dae
      (pnames,ps)       = V.unzip $ V.fromList $ F.toList $ _daeP dae

      xdots' = svector xdots
      xs'    = svector xs
      zs'    = svector zs
      us'    = svector us
      ps'    = svector ps

      daeResidual :: Vector SXElement
      daeResidual = V.map (uncurry (-)) $ V.fromList $ F.toList $ daeConstraints dae

      onames :: Vector String
      osOut :: Vector SXElement
      (onames, osOut) = V.unzip $ V.fromList $ M.toList $ _daeO dae
  os <- V.mapM sxElementSym onames :: IO (Vector SXElement)
  let os' = svector os

      lookupThingy :: String -> OcpMonad SXElement
      lookupThingy name = do
        debug $ "ocp monad: looking up \"" ++ name ++ "\""
        case M.lookup name varmap of
          Nothing -> err $ "ocp monad: nothing named \"" ++ name ++ "\""
          Just expr -> do
            debug $ "ocp monad: found \"" ++ name ++ "\""
            --debug $ "ocp monad: found \"" ++ name ++ "\": " ++ show expr
            return expr
        where
          varmap :: M.Map String SXElement
          varmap = M.fromList $ F.toList $ V.concat
                   [ V.zip xdotnames xdots
                   , V.zip xnames xs
                   , V.zip znames zs
                   , V.zip unames us
                   , V.zip pnames ps
                   , V.zip onames os
                   ]

  ocp' <- flip runStateT emptySymbolicOcp $ runWriterT $ runExceptT (runOcp (ocpMonad time lookupThingy))
  let ocp :: OcpState
      ocp = case ocp' of
        ((Left errmsg, logs),_) ->
           error $ unlines $ ("" : map show logs) ++ ["","ocp monad failure: " ++ show errmsg]
        ((Right _, _), ocpState) -> ocpState

      obj = case ocpLagrangeObj ocp of
        ObjectiveUnset -> 0
        Objective obj' -> obj'

  lagFunSX <- sxFunction (V.fromList [xs',zs',us',ps',os',time',endT']) (V.fromList [svector (V.singleton obj)])
  setOption lagFunSX "name" "lagrange"
  soInit lagFunSX

  let pathConstraints :: [SXElement]
      pathConstraintBnds :: [(Maybe Double, Maybe Double)]
      (pathConstraints, pathConstraintBnds) = unzip $ map constr (F.toList (ocpPathConstraints ocp))

  pathcFunSX <- sxFunction (V.fromList [xs',zs',us',ps',os',time'])
                         (V.singleton (svector (V.fromList pathConstraints)))
  setOption pathcFunSX "name" "pathConstraints"
  soInit pathcFunSX


  daeFunSX <- sxFunction (V.fromList [xdots', xs', zs', us', ps', time'])
                         (V.fromList [svector daeResidual, svector osOut])
  setOption pathcFunSX "name" "daeResidualAndOutputs"
  soInit daeFunSX

  -- run the mayer function
  x0s <- mapM (sxElementSym . (++ "_0")) (F.toList xnames)
  xFs <- mapM (sxElementSym . (++ "_F")) (F.toList xnames)
  let lookupState :: M.Map String SXElement -> String
                     -> ExceptT ErrorMessage (Writer [LogMessage]) SXElement
      lookupState xmap name = do
        debug $ "mayer monad: looking up \"" ++ name ++ "\""
        case M.lookup name xmap of
          Nothing -> err $ "mayer monad: no state named \"" ++ name ++ "\""
          Just expr -> do
            debug $ "mayer monad: found \"" ++ name ++ "\""
            return expr

      xmap0 :: M.Map String SXElement
      xmap0 = M.fromList $ zip (F.toList xnames) x0s

      xmapF :: M.Map String SXElement
      xmapF = M.fromList $ zip (F.toList xnames) xFs

      mayerObj :: SXElement
      mayerObj = case runWriter (runExceptT (mayerMonad endT (lookupState xmap0) (lookupState xmapF))) of
          (Left errmsg, logs) ->
            error $ unlines $ ("" : map show logs) ++ ["","mayer monad failure: " ++ show errmsg]
          (Right ret, _) -> ret
  mayerFunSX <- sxFunction (V.fromList [svector (V.singleton endT), svector (V.fromList x0s), svector (V.fromList xFs)])
                           (V.singleton (svector (V.singleton mayerObj)))
  setOption mayerFunSX "name" "mayer"
  soInit mayerFunSX


  let lookupState0 :: String -> BCMonad SXElement
      lookupState0 name = do
        debug $ "boundary condition monad: looking up initial \"" ++ name ++ "\""
        case M.lookup name xmap0 of
          Nothing -> err $ "boundary condition monad: no state named \"" ++ name ++ "\""
          Just expr -> do
            debug $ "boundary condition monad: found \"" ++ name ++ "\""
            return expr

      lookupStateF :: String -> BCMonad SXElement
      lookupStateF name = do
        debug $ "boundary condition monad: looking up final \"" ++ name ++ "\""
        case M.lookup name xmapF of
          Nothing -> err $ "boundary condition monad: no state named \"" ++ name ++ "\""
          Just expr -> do
            debug $ "boundary condition monad: found \"" ++ name ++ "\""
            return expr
  bcs' <- flip runStateT S.empty $ runWriterT (runExceptT (runBc $ bcMonad lookupState0 lookupStateF))
  let bcs :: Vector SXElement
      bcbnds :: Vector Bounds
      (bcs,bcbnds) = case bcs' of
        ((Left errmsg, logs),_) ->
          error $ unlines $ ("" : map show logs) ++ ["","boundary condition monad failure: " ++ show errmsg]
        ((Right _,_), ret) -> V.unzip $ V.fromList $ map constr $ F.toList ret
  bcFunSX <- sxFunction (V.fromList [svector (V.fromList x0s), svector (V.fromList xFs)])
                        (V.singleton (svector bcs))
  setOption bcFunSX "name" "boundaryConditions"
  soInit bcFunSX

  let meta = CollTrajMeta
             { ctmX = NameTreeNode ("", "") (zip (F.toList xnames) (map NameTreeLeaf [0..]))
             , ctmZ = NameTreeNode ("", "") (zip (F.toList znames) (map NameTreeLeaf [0..]))
             , ctmU = NameTreeNode ("", "") (zip (F.toList unames) (map NameTreeLeaf [0..]))
             , ctmP = NameTreeNode ("", "") (zip (F.toList pnames) (map NameTreeLeaf [0..]))
             , ctmO = NameTreeNode ("", "") (zip (F.toList onames) (map NameTreeLeaf [0..]))
             , ctmN = n
             , ctmDeg = deg
             , ctmNx = V.length xnames
             , ctmNz = V.length znames
             , ctmNu = V.length unames
             , ctmNp = V.length pnames
             , ctmNo = V.length onames
             , ctmNsx = 0
             , ctmQuadRoots = Legendre -- TODO: make this an input
             }
  TV.reifyDim (ctmNx meta) $ \(Proxy :: Proxy nx) ->
    TV.reifyDim (ctmNz meta) $ \(Proxy :: Proxy nz) ->
    TV.reifyDim (ctmNu meta) $ \(Proxy :: Proxy nu) ->
    TV.reifyDim (ctmNp meta) $ \(Proxy :: Proxy np) ->
    TV.reifyDim (V.length daeResidual) $ \(Proxy :: Proxy nr) ->
    TV.reifyDim (V.length onames) $ \(Proxy :: Proxy no) ->
    TV.reifyDim (V.length bcs) $ \(Proxy :: Proxy nc) ->
    TV.reifyDim (length pathConstraints) $ \(Proxy :: Proxy nh) -> do
  --  TV.reifyDim ncov $ \(Proxy :: Proxy ncov) -> do
  --  TV.reifyDim nsh $ \(Proxy :: Proxy nsh) -> do
  --  TV.reifyDim nsc $ \(Proxy :: Proxy nsc) -> do

    let daeFun :: Vec nx SXElement -> Vec nx SXElement -> Vec nz SXElement -> Vec nu SXElement
                  -> Vec np SXElement -> SXElement
                   -> (Vec nr SXElement, Vec no SXElement)
        daeFun x' x z u p t = (devec (rets V.! 0), devec (rets V.! 1))
          where
            rets = callSX daeFunSX (V.fromList [vec x', vec x, vec z, vec u, vec p, sxElementToSX t])

        lagrangeFun :: Vec nx SXElement -> Vec nz SXElement -> Vec nu SXElement -> Vec np SXElement -> Vec no SXElement -> SXElement -> SXElement -> SXElement
        lagrangeFun x z u p o t tf =
          sxToSXElement $ V.head $ callSX lagFunSX $
                 (V.fromList [vec x, vec z, vec u, vec p, vec o, sxElementToSX t, sxElementToSX tf])
          --Left errmsg -> error $ "toOcpPhase: lagrangeFun: " ++ errmsg ++
          --  "\ninputs: " ++ show (xnames ++ znames ++ unames ++ pnames) ++ show onames ++
          --  "\nnumeric inputs x: " ++ show (V.length x) ++
          --  "\nnumeric inputs z: " ++ show (V.length z) ++
          --  "\nnumeric inputs u: " ++ show (V.length u) ++
          --  "\nnumeric inputs p: " ++ show (V.length p) ++
          --  "\nnumeric inputs o: " ++ show (V.length o)

        pathConstraintFun :: Vec nx SXElement -> Vec nz SXElement -> Vec nu SXElement
                             -> Vec np SXElement -> Vec no SXElement -> SXElement -> Vec nh SXElement
        pathConstraintFun x z u p o t =
          devec $ V.head $ callSX pathcFunSX (V.fromList [vec x, vec z, vec u, vec p, vec o, sxElementToSX t])

        mayerFun :: SXElement -> Vec nx SXElement -> Vec nx SXElement
                    -> SXElement
        mayerFun endT'' x0 xF = sxToSXElement $ V.head $ callSX mayerFunSX (V.fromList [sxElementToSX endT'', vec x0, vec xF])

        bcFun :: Vec nx SXElement -> Vec nx SXElement -> Vec nc SXElement
        bcFun x0 xF = devec $ V.head $ callSX bcFunSX (V.fromList [vec x0, vec xF])

        ocpPhase =
          OcpPhase { ocpMayer = mayerFun
                   , ocpLagrange = lagrangeFun
                   , ocpDae = daeFun
                   , ocpBc = bcFun
                   , ocpBcBnds = devectorize bcbnds
                   , ocpPathC = pathConstraintFun
                   , ocpPathCBnds = devectorize (V.fromList pathConstraintBnds)
                   , ocpXbnd = fill (Nothing, Nothing)
                   , ocpZbnd = fill (Nothing, Nothing)
                   , ocpUbnd = fill (Nothing, Nothing)
                   , ocpPbnd = fill (Nothing, Nothing)
                   , ocpTbnd = tbnds
                   , ocpObjScale = Nothing
                   , ocpTScale = Nothing
                   , ocpXScale = Nothing
                   , ocpZScale = Nothing
                   , ocpUScale = Nothing
                   , ocpPScale = Nothing
                   , ocpResidualScale = Nothing
                   , ocpBcScale = Nothing
                   , ocpPathCScale = Nothing
                   }
    f ocpPhase meta

vec :: Vectorize f => f SXElement -> SX
vec = svector . vectorize

devec :: Vectorize f => SX -> f SXElement
devec = sxSplitJV . mkJ

solveStaticOcp ::
  NlpSolverStuff
  -> (SXElement -> DaeMonad ())
  -> (forall a m . (Floating a, Monad m) => a -> (String -> m a) -> (String -> m a) -> m a)
  -> ((String -> BCMonad SXElement) -> (String -> BCMonad SXElement) -> BCMonad ())
  -> (SXElement -> (String -> OcpMonad SXElement) -> OcpMonad ())
  -> (Maybe Double, Maybe Double)
  -> Int -> Int
  -> Maybe (CollTrajMeta -> [DynCollTraj (Vector Double)] -> IO Bool)
  -> IO (Either String String)
solveStaticOcp solverStuff dae mayer bc ocp tbnds n deg cb =
  reifyOcpPhase dae mayer bc ocp tbnds n deg woo
    where
      woo ocpphase meta = solveOcp solverStuff n deg (cb <*> pure meta) ocpphase
