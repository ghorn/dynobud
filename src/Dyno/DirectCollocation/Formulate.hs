{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Formulate
       ( CovTraj(..)
       , CollProblem(..)
       , makeCollProblem
       , mkTaus
       , makeGuess
       , makeGuessSim
       ) where

import GHC.Generics ( Generic, Generic1 )

import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear.Matrix hiding ( trace )
import Linear.V

import Casadi.DMatrix ( DMatrix )
import Casadi.MX ( MX )
import Casadi.SX ( SX )

import Dyno.SXElement ( sxCatJV, sxSplitJV )
import Dyno.View.View ( View(..), J, jfill, JTuple(..), JNone(..), v2d, d2v )
import qualified Dyno.View.M as M
import Dyno.View.JV ( JV, splitJV, catJV )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.Fun
import Dyno.View.JVec( JVec(..), jreplicate )
import Dyno.View.Scheme ( Scheme )
import Dyno.Vectorize ( Vectorize(..), Id(..), fill, vlength, vzipWith )
import Dyno.TypeVecs ( Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )
import Dyno.Nlp ( Nlp(..), Bounds )
import Dyno.Ocp

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( MetaProxy(..), DynPlotPoints, dynPlotPoints )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..), mkTaus, interpolate, timesFromTaus )
import Dyno.DirectCollocation.Robust

data CollProblem x z u p r o c h q n deg =
  CollProblem
  { cpNlp :: Nlp (CollTraj x z u p n deg)
                 JNone
                 (CollOcpConstraints x r c h n deg) MX
  , cpOcp :: OcpPhase x z u p r o c h q
  , cpPlotPoints :: J (CollTraj x z u p n deg) (Vector Double)
                    -> IO (DynPlotPoints Double)
  , cpHellaOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                      -> IO ( DynPlotPoints Double
                            , Vec n ( Vec deg ( J (JV o) (Vector Double)
                                              , J (JV x) (Vector Double)
                                              , J (JV h) (Vector Double)
                                              )
                                    , J (JV x) (Vector Double)
                                    )
                            )
  , cpOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                 -> IO (Vec n ( Vec deg ( o Double
                                        , x Double
                                        , h Double
                                        )
                              , x Double
                              )
                       )
  , cpTaus :: Vec deg Double
  , cpRoots :: QuadratureRoots
  , cpEvalQuadratures :: Vec n (Vec deg Double) -> Double -> IO Double
  , cpMetaProxy :: MetaProxy x z u p o q h
  }


data QuadratureIn x z u p a =
  -- x' x z u p t T
  QuadratureIn (J x a) (J x a) (J z a) (J u a) (J p a)
               (J (JV Id) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data QuadratureStageIn x z u p deg a =
  -- xzus p ts T
  QuadratureStageIn (J (CollStage x z u deg) a) (J p a) (J (JVec deg (JV Id)) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data PathCIn x z u p a =
  -- x' x z u p t
  PathCIn (J x a) (J x a) (J z a) (J u a) (J p a) (J (JV Id) a)
  deriving (Generic, Generic1)

data PathCStageIn x z u p deg a =
  -- xzus p ts T
  PathCStageIn (J (CollStage x z u deg) a) (J p a) (J (JVec deg (JV Id)) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data DaeIn x z u p a =
  -- t p x' (CollPoint x z u)
  DaeIn (J (JV Id) a) (J p a) (J x a) (J (CollPoint x z u) a)
  deriving (Generic, Generic1)

data DaeOut r o a =
  -- r o
  DaeOut (J r a) (J o a)
  deriving (Generic, Generic1)

instance (View x, View z, View u, View p) => Scheme (QuadratureIn x z u p)
instance (View x, View z, View u, View p, Dim deg) => Scheme (QuadratureStageIn x z u p deg)
instance (View x, View z, View u, View p) => Scheme (PathCIn x z u p)
instance (View x, View z, View u, View p, Dim deg) => Scheme (PathCStageIn x z u p deg)
instance (View x, View z, View u, View p) => Scheme (DaeIn x z u p)
instance (View r, View o) => Scheme (DaeOut r o)

makeCollProblem ::
  forall x z u p r o c h q deg n .
  ( Dim deg, Dim n
  , Vectorize x, Vectorize p, Vectorize u, Vectorize z
  , Vectorize r, Vectorize o, Vectorize h, Vectorize c, Vectorize q
  )
  => QuadratureRoots -> OcpPhase x z u p r o c h q
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> IO (CollProblem x z u p r o c h q n deg)
makeCollProblem roots ocp guess = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus roots

      n = reflectDim (Proxy :: Proxy n)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
      cijs = lagrangeDerivCoeffs (0 TV.<| taus)

      interpolate' :: (J (JV x) :*: J (JVec deg (JV x))) MX -> J (JV x) MX
      interpolate' (x0 :*: xs) = case roots of
        Legendre -> interpolate taus x0 (unJVec (split xs))
        Radau -> TV.tvlast $ unJVec $ split xs

      interpolateq' :: (J (JV q) :*: J (JVec deg (JV q))) MX -> J (JV q) MX
      interpolateq' (q0 :*: qs) = case roots of
        Legendre -> interpolate taus q0 (unJVec (split qs))
        Radau -> TV.tvlast $ unJVec $ split qs

      interpolateScalar' :: (J (JV Id) :*: J (JVec deg (JV Id))) MX -> J (JV Id) MX
      interpolateScalar' (x0 :*: xs) = case roots of
        Legendre -> interpolate taus x0 (unJVec (split xs))
        Radau -> TV.tvlast $ unJVec $ split xs

      dynamicsFunction :: DaeIn (JV x) (JV z) (JV u) (JV p) SX -> DaeOut (JV r) (JV o) SX
      dynamicsFunction (DaeIn t parm x' collPoint) = DaeOut (sxCatJV r) (sxCatJV o)
        where
          CollPoint x z u = split collPoint
          (r,o) = ocpDae ocp
                  (sxSplitJV x') (sxSplitJV x) (sxSplitJV z) (sxSplitJV u)
                  (sxSplitJV parm) (unId (sxSplitJV t))

  interpolateFun <- toMXFun "interpolate (JV x)" interpolate' >>= expandMXFun
  interpolateQFun <- toMXFun "interpolate (JV q)" interpolateq' >>= expandMXFun
  interpolateScalarFun <- toMXFun "interpolate (JV Id)" interpolateScalar' >>= expandMXFun

  let callInterpolateScalar :: J (JV Id) MX -> Vec deg (J (JV Id) MX) -> J (JV Id) MX
      callInterpolateScalar x0 xs = call interpolateScalarFun (x0 :*: cat (JVec xs))

      callInterpolate :: J (JV x) MX -> Vec deg (J (JV x) MX) -> J (JV x) MX
      callInterpolate x0 xs = call interpolateFun (x0 :*: cat (JVec xs))

      callInterpolateQ :: J (JV q) MX -> Vec deg (J (JV q) MX) -> J (JV q) MX
      callInterpolateQ q0 qs = call interpolateQFun (q0 :*: cat (JVec qs))

  let quadFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) SX -> J (JV q) SX
      quadFun (QuadratureIn x' x z u p t tf) = quad
        where
          daeIn = DaeIn t p x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          quad :: J (JV q) SX
          quad = sxCatJV $ ocpQuadratures ocp
                 (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV o)
                 (unId (sxSplitJV t)) (unId (sxSplitJV tf))

  let lagFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) SX -> J (JV Id) SX
      lagFun (QuadratureIn x' x z u p t tf) = lag
        where
          daeIn = DaeIn t p x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          lag :: J (JV Id) SX
          lag = sxCatJV $ Id $ ocpLagrange ocp
                (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV o)
                (unId (sxSplitJV t)) (unId (sxSplitJV tf))

  let pathCFun :: PathCIn (JV x) (JV z) (JV u) (JV p) SX -> J (JV h) SX
      pathCFun (PathCIn x' x z u p t) = h
        where
          daeIn = DaeIn t p x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          h :: J (JV h) SX
          h = sxCatJV $ ocpPathC ocp
              (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV o)
              (unId (sxSplitJV t))

  quadFunSX <- toSXFun "quadFun" quadFun
  lagFunSX <- toSXFun "lagFun" lagFun
  pathCFunSX <- toSXFun "pathCFun" pathCFun

  let -- later we could use the intermediate points as outputs, or in path cosntraints
      lagrangeStageFun qIn = qNext
        where
          (_,_,qNext) = toQuadratureFun n cijs callInterpolateScalar (call lagFunSX) qIn
      quadratureStageFun qIn = qNext
        where
          (_,_,qNext) = toQuadratureFun n cijs callInterpolateQ (call quadFunSX) qIn
      pathCStageFun pcIn = cat (JVec hs)
        where
          hs = toPathCFun n cijs (call pathCFunSX) pcIn
  lagrangeStageFunMX   <- toMXFun "lagrangeStageFun"   lagrangeStageFun
  quadratureStageFunMX <- toMXFun "quadratureStageFun" quadratureStageFun
  pathCStageFunMX <- toMXFun "pathCStageFun" pathCStageFun


  bcFun <- toSXFun "bc" $ \(x0:*:x1:*:x2:*:x3:*:x4) -> sxCatJV $ ocpBc ocp (sxSplitJV x0) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (unId (sxSplitJV x4))
  mayerFun <- toSXFun "mayer" $ \(x0:*:x1:*:x2:*:x3:*:x4) ->
    sxCatJV $ Id $ ocpMayer ocp (unId (sxSplitJV x0)) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (sxSplitJV x4)

  dynFun <- toSXFun "dynamics" dynamicsFunction

  dynamicsStageFun <- toMXFun "dynamicsStageFunction" (toDynamicsStage callInterpolate cijs dynFun)
  callDynamicsStageFun <- fmap call (expandMXFun dynamicsStageFun)

  let nlp :: Nlp (CollTraj x z u p n deg) JNone (CollOcpConstraints x r c h n deg) MX
      nlp = Nlp {
        nlpFG =
           getFg taus
           (bcFun :: SXFun (   J (JV x)
                           :*: J (JV x)
                           :*: J (JV q)
                           :*: J (JV p)
                           :*: J (JV Id)
                           )
                           (J (JV c))
           )
           (mayerFun :: SXFun (   J (JV Id)
                              :*: J (JV x)
                              :*: J (JV x)
                              :*: J (JV q)
                              :*: J (JV p)
                              )
                              (J (JV Id))
           )
           (call lagrangeStageFunMX)
           (call quadratureStageFunMX)
           (call pathCStageFunMX)
           (callDynamicsStageFun)
        , nlpBX = cat $ fillCollTraj'
                  (fill (Nothing, Nothing))
                  (ocpXbnd ocp)
                  (ocpZbnd ocp)
                  (ocpUbnd ocp)
                  (ocpPbnd ocp)
                  (ocpTbnd ocp)
        , nlpBG = cat (getBg ocp)
        , nlpX0 = guess :: J (CollTraj x z u p n deg) (Vector Double)
        , nlpP = cat JNone
        , nlpLamX0 = Nothing
        , nlpLamG0 = Nothing
        , nlpScaleF = ocpObjScale ocp
        , nlpScaleX = Just $ cat $ fillCollTraj
                      (fromMaybe (fill 1) (ocpXScale ocp))
                      (fromMaybe (fill 1) (ocpZScale ocp))
                      (fromMaybe (fill 1) (ocpUScale ocp))
                      (fromMaybe (fill 1) (ocpPScale ocp))
                      (fromMaybe       1  (ocpTScale ocp))

        , nlpScaleG = Just $ cat $ fillCollConstraints
                      (fromMaybe (fill 1) (ocpXScale ocp))
                      (fromMaybe (fill 1) (ocpResidualScale ocp))
                      (fromMaybe (fill 1) (ocpBcScale ocp))
                      (fromMaybe (fill 1) (ocpPathCScale ocp))
        }

  -- callbacks and quadrature outputs
  outputFun <- toMXFun "stageOutputs" $ outputFunction callInterpolate cijs taus dynFun
  genericQuadraturesFun <- toMXFun "generic quadratures" $
                           genericQuadraturesFunction callInterpolateScalar cijs n

  let (getHellaOutputs, getPlotPoints, getOutputs) = toCallbacks n roots taus outputFun pathCStageFunMX

      evalQuadratures :: Vec n (Vec deg Double) -> Double -> IO Double
      evalQuadratures qs' tf' = do
        let d2d :: Double -> J (JV Id) DMatrix
            d2d = realToFrac
            qs :: Vec n (J (JVec deg (JV Id)) DMatrix)
            qs = fmap (cat . JVec . fmap d2d) qs'
            tf :: J (JV Id) DMatrix
            tf = realToFrac tf'
            evalq :: J (JVec deg (JV Id)) DMatrix -> IO (J (JV Id) DMatrix)
            evalq q = eval genericQuadraturesFun (q :*: tf)
        stageIntegrals' <- T.mapM evalq qs :: IO (Vec n (J (JV Id) DMatrix))
        let stageIntegrals = fmap (unId . splitJV . d2v) stageIntegrals' :: Vec n Double
        return (F.sum stageIntegrals)

  return $ CollProblem { cpNlp = nlp
                       , cpOcp = ocp
                       , cpPlotPoints = getPlotPoints
                       , cpHellaOutputs = getHellaOutputs
                       , cpOutputs = getOutputs
                       , cpTaus = taus
                       , cpRoots = roots
                       , cpEvalQuadratures = evalQuadratures
                       , cpMetaProxy = MetaProxy
                       }

toCallbacks ::
  forall x z u p r o h n deg
  . ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
    , Vectorize o, Vectorize h, Vectorize r
    , Dim n, Dim deg
    )
  => Int
  -> QuadratureRoots
  -> Vec deg Double
  -> MXFun (   J (CollStage (JV x) (JV z) (JV u) deg)
           :*: J (JV p)
           :*: J (JV Id)
           :*: J (JV Id)
           )
           (   J (JVec deg (JV r))
           :*: J (JVec deg (JV x))
           :*: J (JVec deg (JV o))
           :*: J (JV x)
           )
  -> MXFun (PathCStageIn (JV x) (JV z) (JV u) (JV p) deg) (J (JVec deg (JV h)))
  -> ( J (CollTraj x z u p n deg) (Vector Double)
          -> IO ( DynPlotPoints Double
                , Vec n
                      ( Vec deg
                            ( J (JV o) (Vector Double)
                            , J (JV x) (Vector Double)
                            , J (JV h) (Vector Double)
                            )
                      , J (JV x) (Vector Double)
                      )
                )
     , J (CollTraj x z u p n deg) (Vector Double) -> IO (DynPlotPoints Double)
     , J (CollTraj x z u p n deg) (Vector Double)
       -> IO ( Vec n
                   ( Vec deg (o Double, x Double, h Double)
                   , x Double
                   )
             )
     )
toCallbacks n roots taus outputFun pathStageConFun = (getHellaOutputs, getPlotPoints, getOutputs)
  where
  -- prepare callbacks
    f :: J (JV o) DMatrix ->  J (JV x) DMatrix -> J (JV h) DMatrix
         -> (J (JV o) (Vector Double), J (JV x) (Vector Double), J (JV h) (Vector Double))
    f o' x' h' = (d2v o', d2v x', d2v h')

    callOutputFun :: J (JV p) (Vector Double)
                     -> J (JV Id) (Vector Double)
                     -> J (JV Id) (Vector Double)
                     -> J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double)
                     -> J (JV Id) (Vector Double)
                     -> IO ( Vec deg ( J (JV o) (Vector Double)
                                     , J (JV x) (Vector Double)
                                     , J (JV h) (Vector Double)
                                     )
                           , J (JV x) (Vector Double)
                           )
    callOutputFun p h tf stage k = do
      let p' = v2d p
      (_ :*: xdot :*: out :*: xnext) <-
        eval outputFun $ (v2d stage) :*: p' :*: (v2d h) :*: (v2d k)

      let stageTimes :: Vec deg (J (JV Id) DMatrix)
          stageTimes = fmap (\tau -> t0 + realToFrac tau * h') taus
            where
              t0 = h' * v2d k
              h' = v2d h
          pathCStageIn = PathCStageIn (v2d stage) p' (cat (JVec stageTimes)) (v2d tf)
      hs <- eval pathStageConFun pathCStageIn

      let outs0 = unJVec (split out) :: Vec deg (J (JV o) DMatrix)
          xdots0 = unJVec (split xdot) :: Vec deg (J (JV x) DMatrix)
          hs0 = unJVec (split hs) :: Vec deg (J (JV h) DMatrix)
      return (TV.tvzipWith3 f outs0 xdots0 hs0, d2v xnext)

    mapOutputFun :: J (CollTraj x z u p n deg) (Vector Double)
                    -> IO (Vec n ( Vec deg ( J (JV o) (Vector Double)
                                           , J (JV x) (Vector Double)
                                           , J (JV h) (Vector Double)
                                           )
                                 , J (JV x) (Vector Double)
                                 )
                          )
    mapOutputFun ct = do
      let CollTraj tf p stages _ = split ct
          h = catJV $ Id (tf' / fromIntegral n)
            where
              Id tf' = splitJV tf

          vstages = unJVec (split stages)
              :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
          ks :: Vec n (J (JV Id) (Vector Double))
          ks = TV.mkVec' $ map (catJV . Id . realToFrac) (take n [(0::Int)..])

      T.sequence $ TV.tvzipWith (callOutputFun p h tf) vstages ks

    getHellaOutputs ::
      J (CollTraj x z u p n deg) (Vector Double)
      -> IO ( DynPlotPoints Double
            , Vec n ( Vec deg ( J (JV o) (Vector Double)
                              , J (JV x) (Vector Double)
                              , J (JV h) (Vector Double)
                              )
                    , J (JV x) (Vector Double)
                    )
            )
    getHellaOutputs traj = do
      outputs <- mapOutputFun traj
      return (dynPlotPoints roots (split traj) outputs, outputs)

    getPlotPoints :: J (CollTraj x z u p n deg) (Vector Double)
                     -> IO (DynPlotPoints Double)
    getPlotPoints traj = fmap fst $ getHellaOutputs traj

    getOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                  -> IO (Vec n (Vec deg (o Double, x Double, h Double), x Double))
    getOutputs traj = do
      outputs <- mapOutputFun traj
      let devec :: Vec deg (J (JV o) (Vector Double), J (JV x) (Vector Double), J (JV h) (Vector Double))
                -> Vec deg (o Double, x Double, h Double)
          devec = fmap (\(x,y,z) -> (splitJV x, splitJV y, splitJV z))
      return $ fmap (\(x,y) -> (devec x, splitJV y)) outputs



getFg ::
  forall x z u p r c h q n deg .
  ( Dim deg, Dim n
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Vectorize r, Vectorize c, Vectorize h, Vectorize q
  )
  -- taus
  => Vec deg Double
  -- bcFun
  -> SXFun (   J (JV x)
           :*: J (JV x)
           :*: J (JV q)
           :*: J (JV p)
           :*: J (JV Id)
           )
           (J (JV c))
  -- mayerFun
  -> SXFun (   J (JV Id)
           :*: J (JV x)
           :*: J (JV x)
           :*: J (JV q)
           :*: J (JV p)
           )
           (J (JV Id))
  -- lagQuadFun
  -> (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) deg MX -> J (JV Id) MX)
  -- quadFun
  -> (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) deg MX -> J (JV q) MX)
  -- pathCStageFun
  -> (PathCStageIn (JV x) (JV z) (JV u) (JV p) deg MX -> J (JVec deg (JV h)) MX)
  -- stageFun
  -> ( (   J (JV x)
       :*: J (JVec deg (JTuple (JV x) (JV z)))
       :*: J (JVec deg (JV u))
       :*: J (JV Id)
       :*: J (JV p)
       :*: J (JVec deg (JV Id))
       ) MX
       -> (   J (JVec deg (JV r))
          :*: J (JV x)
          ) MX
     )
  -- collTraj
  -> J (CollTraj x z u p n deg) MX
  -- parameter
  -> J JNone MX
  -- (objective, constraints)
  -> (J (JV Id) MX, J (CollOcpConstraints x r c h n deg) MX)
getFg taus bcFun mayerFun lagQuadFun quadFun pathCStageFun dynamicsStageFun collTraj _ = (obj, cat g)
  where
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    obj = objLagrange + objMayer

    objMayer = call mayerFun (tf :*: x0 :*: xf :*: finalQuadratures :*: parm)

    objLagrange :: J (JV Id) MX
    objLagrange = F.sum $ TV.tvzipWith (oneQuadStage lagQuadFun) stages times'

    finalQuadratures :: J (JV q) MX
    finalQuadratures = F.sum $ TV.tvzipWith (oneQuadStage quadFun) stages times'

    oneQuadStage ::
      View qOrSomething
      => (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) deg MX -> J qOrSomething MX)
      -> J (CollStage (JV x) (JV z) (JV u) deg) MX
      -> J (JVec deg (JV Id)) MX
      -> J qOrSomething MX
    oneQuadStage qfun collStage stageTimes = qfun qInputs
      where
        qInputs :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) deg MX
        qInputs = QuadratureStageIn collStage parm stageTimes tf

    -- timestep
    dt = tf / fromIntegral n
    n = reflectDim (Proxy :: Proxy n)

    -- times at each collocation point
    times :: Vec n (Vec deg (J (JV Id) MX))
    times = fmap snd $ timesFromTaus 0 (fmap realToFrac taus) dt

    times' :: Vec n (J (JVec deg (JV Id)) MX)
    times' = fmap (cat . JVec) times

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    x0s = fmap (\(CollStage x0' _) -> x0') spstages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (J (JV x) MX)
    xfs = TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _) -> x0') (TV.tvhead spstages)
    g = CollOcpConstraints
        { coCollPoints = cat $ JVec dcs
        , coContinuity = cat $ JVec integratorMatchingConstraints
        , coPathC = cat $ JVec hs
        , coBc = call bcFun (x0 :*: xf :*: finalQuadratures :*: parm :*: tf)
        }

    integratorMatchingConstraints :: Vec n (J (JV x) MX) -- THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = vzipWith (-) interpolatedXs xfs

    dcs :: Vec n (J (JVec deg (JV r)) MX)
    hs :: Vec n (J (JVec deg (JV h)) MX)
    interpolatedXs :: Vec n (J (JV x) MX)
    (dcs, hs, interpolatedXs) = TV.tvunzip3 $ fmap fff $ TV.tvzip spstages times'
    fff :: (CollStage (JV x) (JV z) (JV u) deg MX, J (JVec deg (JV Id)) MX) ->
           (J (JVec deg (JV r)) MX, J (JVec deg (JV h)) MX, J (JV x) MX)
    fff (CollStage x0' xzus, stageTimes) = (dc, stageHs, interpolatedX')
      where
        -- todo: could share xdot here instead of embedding in pathc and dynamics
        dc :*: interpolatedX' =
          dynamicsStageFun (x0' :*: xzs :*: us :*: dt :*: parm :*: stageTimes)

        -- todo: don't split/cat this
        pathCStageIn = PathCStageIn (cat (CollStage x0 xzus)) parm stageTimes tf
        stageHs = pathCStageFun pathCStageIn

        xzs = cat (JVec xzs') :: J (JVec deg (JTuple (JV x) (JV z))) MX
        us = cat (JVec us') :: J (JVec deg (JV u)) MX
        (xzs', us') = TV.tvunzip $ fmap toTuple $ unJVec (split xzus)
        toTuple xzu = (cat (JTuple x z), u)
          where
            CollPoint x z u = split xzu



getBg :: forall x z u p r o c h q n deg .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize r, Vectorize c, Vectorize h
  )
  => OcpPhase x z u p r o c h q
  -> CollOcpConstraints x r c h n deg (Vector Bounds)
getBg ocp =
  CollOcpConstraints
  { coCollPoints = jreplicate (jfill (Just 0, Just 0)) -- dae residual constraint
  , coContinuity = jreplicate (jfill (Just 0, Just 0)) -- continuity constraint
  , coPathC = jreplicate (jreplicate hbnds)
  , coBc = catJV (ocpBcBnds ocp)
  }
  where
    hbnds :: J (JV h) (Vector Bounds)
    hbnds = catJV (ocpPathCBnds ocp)

toQuadratureFun ::
  forall x z u p q deg
  . ( View q, View x, View z, View u, Dim deg
    )
  => Int
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> (J q MX -> Vec deg (J q MX) -> J q MX)
  -> (QuadratureIn x z u p MX -> J q MX)
  -> QuadratureStageIn x z u p deg MX
  -> (Vec deg (J q MX), Vec deg (J q MX), J q MX)
toQuadratureFun n cijs interpolate' evalQuadDeriv (QuadratureStageIn collStage p stageTimes' tf) =
  (qdots, qs, qnext)
  where
    CollStage x0 xzus' = split collStage
    xzus = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u MX)
    h = tf / fromIntegral n

    xs :: Vec deg (J x MX)
    xs = fmap (\(CollPoint x _ _) -> x) xzus

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    quadratureIns :: Vec deg (QuadratureIn x z u p MX)
    quadratureIns = TV.tvzipWith3 (\x' (CollPoint x z u) t -> QuadratureIn x' x z u p t tf)
                                  xdots xzus stageTimes

    qdots :: Vec deg (J q MX)
    qdots = fmap evalQuadDeriv quadratureIns

    stageTimes :: Vec deg (J (JV Id) MX)
    stageTimes = unJVec (split stageTimes')

    qnext :: J q MX
    qnext = interpolate' (0 :: J q MX) qs

    qs :: Vec deg (J q MX)
    qs = cijInvFr !* qdots

    cijs' :: Vec deg (Vec deg Double)
    cijs' = TV.tvtail $ fmap TV.tvtail cijs

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs'

    cijInv' :: Mat.Matrix Double
    cijInv' = LA.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (J q MX))
    cijInvFr = fmap (fmap realToFrac) cijInv


toPathCFun ::
  forall x z u p h deg
  . ( View x, View z, View u, View h, Dim deg
    )
  => Int
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> (PathCIn x z u p MX -> J h MX)
  -> PathCStageIn x z u p deg MX
  -> Vec deg (J h MX)
toPathCFun n cijs evalPathC (PathCStageIn collStage p stageTimes' tf) = hs
  where
    CollStage x0 xzus' = split collStage
    xzus = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u MX)
    h = tf / fromIntegral n

    xs :: Vec deg (J x MX)
    xs = fmap (\(CollPoint x _ _) -> x) xzus

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    pathCIns :: Vec deg (PathCIn x z u p MX)
    pathCIns = TV.tvzipWith3 (\x' (CollPoint x z u) t -> PathCIn x' x z u p t)
                                  xdots xzus stageTimes

    hs :: Vec deg (J h MX)
    hs = fmap evalPathC pathCIns

    stageTimes :: Vec deg (J (JV Id) MX)
    stageTimes = unJVec (split stageTimes')


-- todo: merging this with evaluateQuadraturesFunction would reduce duplication,
-- but could be inefficient
genericQuadraturesFunction ::
  forall deg
  . Dim deg
  => (J (JV Id) MX -> Vec deg (J (JV Id) MX) -> J (JV Id) MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> Int
  -> (J (JVec deg (JV Id)) :*: J (JV Id)) MX
  -> J (JV Id) MX
genericQuadraturesFunction interpolate' cijs' n (qdots' :*: tf) =
  dt * qnext
  where
    dt = tf / fromIntegral n

    qdots :: Vec deg (J (JV Id) MX)
    qdots = unJVec $ split qdots'

    qnext :: J (JV Id) MX
    qnext = interpolate' 0 qs

    qs = cijInvFr !* qdots

    cijs :: Vec deg (Vec deg Double)
    cijs = TV.tvtail $ fmap TV.tvtail cijs'

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs

    cijInv' :: Mat.Matrix Double
    cijInv' = LA.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (J (JV Id) MX))
    cijInvFr = fmap (fmap realToFrac) cijInv


-- todo: code duplication
dot :: forall x deg a b. (Fractional (J x a), Real b, Dim deg) => Vec deg b -> Vec deg (J x a) -> J x a
dot cks xs = F.sum $ TV.unVec elemwise
  where
    elemwise :: Vec deg (J x a)
    elemwise = TV.tvzipWith smul cks xs

    smul :: b -> J x a -> J x a
    smul x y = realToFrac x * y


-- todo: code duplication
interpolateXDots' :: (Real b, Fractional (J x a), Dim deg) => Vec deg (Vec deg b) -> Vec deg (J x a) -> Vec deg (J x a)
interpolateXDots' cjks xs = fmap (`dot` xs) cjks

interpolateXDots ::
  (Real b, Dim deg, Fractional (J x a)) =>
  Vec (TV.Succ deg) (Vec (TV.Succ deg) b)
  -> Vec (TV.Succ deg) (J x a)
  -> Vec deg (J x a)
interpolateXDots cjks xs = TV.tvtail $ interpolateXDots' cjks xs


-- return dynamics constraints and interpolated state
toDynamicsStage ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> SXFun (DaeIn x z u p) (DaeOut r o)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J (JV Id) :*: J p :*: J (JVec deg (JV Id))) MX
  -> (J (JVec deg r) :*: J x) MX
toDynamicsStage interpolate' cijs dynFun (x0 :*: xzs' :*: us' :*: h :*: p :*: stageTimes') =
  cat (JVec dynConstrs) :*: xnext
  where
    xzs = fmap split (unJVec (split xzs')) :: Vec deg (JTuple x z MX)
    us = unJVec (split us') :: Vec deg (J u MX)

    -- interpolated final state
    xnext :: J x MX
    xnext = interpolate' x0 xs

    stageTimes = unJVec $ split stageTimes'

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    (dynConstrs, _) = TV.tvunzip $ TV.tvzipWith4 applyDae xdots xzs us stageTimes

    applyDae :: J x MX -> JTuple x z MX -> J u MX -> J (JV Id) MX -> (J r MX, J o MX)
    applyDae x' (JTuple x z) u t = (r, o)
      where
        DaeOut r o = call dynFun (DaeIn t p x' collPoint)
        collPoint = cat (CollPoint x z u)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap (\(JTuple x _) -> x) xzs


-- outputs
outputFunction ::
  forall x z u p r o deg . (Dim deg, View x, View z, View u, View p, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (DaeIn x z u p) (DaeOut r o)
  -> (J (CollStage x z u deg) :*: J p :*: J (JV Id) :*: J (JV Id)) MX
  -> (J (JVec deg r) :*: J (JVec deg x) :*: J (JVec deg o) :*: J x) MX
outputFunction callInterpolate cijs taus dynFun (collStage :*: p :*: h :*: k) =
  cat (JVec dynConstrs) :*: cat (JVec xdots) :*: cat (JVec outputs) :*: xnext
  where
    xzus = unJVec (split xzus') :: Vec deg (J (CollPoint x z u) MX)
    CollStage x0 xzus' = split collStage
    -- times at each collocation point
    stageTimes :: Vec deg (J (JV Id) MX)
    stageTimes = fmap (\tau -> t0 + realToFrac tau * h) taus
    t0 = k*h

    xnext = callInterpolate x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    outputs :: Vec deg (J o MX)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots xzus stageTimes

    applyDae :: J x MX -> J (CollPoint x z u) MX -> J (JV Id) MX -> (J r MX, J o MX)
    applyDae x' xzu t = (r, o)
      where
        DaeOut r o = call dynFun (DaeIn t p x' xzu)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap ((\(CollPoint x _ _) -> x) . split) xzus



-- | make an initial guess
makeGuess ::
  forall x z u p deg n .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  )
  => QuadratureRoots
  -> Double -> (Double -> x Double) -> (Double -> z Double) -> (Double -> u Double)
  -> p Double
  -> CollTraj x z u p n deg (Vector Double)
makeGuess quadratureRoots tf guessX guessZ guessU parm =
  CollTraj (jfill tf) (catJV parm) guesses (catJV (guessX tf))
  where
    -- timestep
    dt = tf / fromIntegral n
    n = vlength (Proxy :: Proxy (Vec n))

    -- initial time at each collocation stage
    t0s :: Vec n Double
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    -- times at each collocation point
    times :: Vec n (Double, Vec deg Double)
    times = fmap (\t0 -> (t0, fmap (\tau -> t0 + tau*dt) taus)) t0s

    mkGuess' :: (Double, Vec deg Double) -> CollStage (JV x) (JV z) (JV u) deg (Vector Double)
    mkGuess' (t,ts) =
      CollStage (catJV (guessX t)) $
      cat $ JVec $ fmap (\t' -> cat (CollPoint (catJV (guessX t')) (catJV (guessZ t')) (catJV (guessU t')))) ts

    guesses :: J (JVec n (CollStage (JV x) (JV z) (JV u) deg)) (Vector Double)
    guesses = cat $ JVec $ fmap (cat . mkGuess') times

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus quadratureRoots


-- | make an initial guess
makeGuessSim ::
  forall x z u p deg n .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  )
  => QuadratureRoots
  -> Double
  -> x Double
  -> (x Double -> u Double -> x Double)
  -> (x Double -> Double -> u Double)
  -> p Double
  -> CollTraj x z u p n deg (Vector Double)
makeGuessSim quadratureRoots tf x00 ode guessU p =
  CollTraj (jfill tf) (catJV p) (cat (JVec stages)) (catJV xf)
  where
    -- timestep
    dt = tf / fromIntegral n
    n = vlength (Proxy :: Proxy (Vec n))

    -- initial time at each collocation stage
    t0s :: Vec n Double
    t0s = TV.mkVec' $ take n [dt * fromIntegral k | k <- [(0::Int)..]]

    xf :: x Double
    stages :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
    (xf, stages) = T.mapAccumL stageGuess x00 t0s

    stageGuess :: x Double -> Double
                  -> (x Double, J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
    stageGuess x0 t0 = (integrate 1, cat (CollStage (catJV x0) points))
      where
        points = cat $ JVec $ fmap (toCollPoint . integrate) taus
        u = guessU x0 t0
        f x = ode x u
        toCollPoint x = cat $ CollPoint (catJV x) (catJV (fill 0 :: z Double)) (catJV u)
        integrate localTau = rk4 f (localTau * dt) x0

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus quadratureRoots

    rk4 :: (x Double -> x Double) -> Double -> x Double -> x Double
    rk4 f h x0 = x0 ^+^ ((k1 ^+^ (2 *^ k2) ^+^ (2 *^ k3) ^+^ k4) ^/ 6)
      where
        k1 = (f  x0)            ^* h
        k2 = (f (x0 ^+^ (k1^/2))) ^* h
        k3 = (f (x0 ^+^ (k2^/2))) ^* h
        k4 = (f (x0 ^+^ k3))    ^* h

        (^+^) :: x Double -> x Double -> x Double
        y0 ^+^ y1 = devectorize $ V.zipWith (+) (vectorize y0) (vectorize y1)

        (*^) :: Double -> x Double -> x Double
        y0 *^ y1 = devectorize $ V.map (y0 *) (vectorize y1)

        (^*) :: x Double -> Double -> x Double
        y0 ^* y1 = devectorize $ V.map (* y1) (vectorize y0)

        (^/) :: x Double -> Double -> x Double
        y0 ^/ y1 = devectorize $ V.map (/ y1) (vectorize y0)
