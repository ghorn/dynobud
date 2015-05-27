{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Formulate
       ( CollProblem(..)
       , StageOutputs(..)
       , makeCollProblem
       , mkTaus
       , makeGuess
       , makeGuessSim
       , ocpPhaseBx
       , ocpPhaseBg
       ) where

import GHC.Generics ( Generic, Generic1 )

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.State ( StateT(..), runStateT )
import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Packed.Matrix as Mat
import qualified Numeric.LinearAlgebra.Algorithms as LA
import Linear hiding ( dot )

import Casadi.DMatrix ( DMatrix )
import Casadi.MX ( MX )
import Casadi.SX ( SX )

import Dyno.SXElement ( sxCatJV, sxSplitJV )
import Dyno.View.View ( View(..), J, jfill, JTuple(..), v2d, d2v )
import qualified Dyno.View.M as M
import Dyno.View.JV ( JV, splitJV, catJV )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.Fun
import Dyno.View.JVec( JVec(..), jreplicate )
import Dyno.View.Scheme ( Scheme )
import Dyno.Vectorize ( Vectorize(..), Id(..), fill, vlength, vzipWith )
import Dyno.TypeVecs ( Vec, Dim, reflectDim )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )
import Dyno.Nlp ( Nlp(..), Bounds )
import Dyno.Ocp

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( MetaProxy(..), DynPlotPoints, dynPlotPoints )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..), mkTaus, interpolate, timesFromTaus )

data CollProblem x z u p r o c h q qo po fp n deg =
  CollProblem
  { cpNlp :: Nlp (CollTraj x z u p n deg)
                 (JV fp)
                 (CollOcpConstraints x r c h n deg) MX
  , cpOcp :: OcpPhase x z u p r o c h q qo po fp
  , cpPlotPoints :: J (CollTraj x z u p n deg) (Vector Double)
                    -> J (JV fp) (Vector Double)
                    -> IO (DynPlotPoints Double)
  , cpHellaOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                      -> J (JV fp) (Vector Double)
                      -> IO ( DynPlotPoints Double
                            , Vec n (StageOutputs x o h q qo po deg Double)
                            , Quadratures q qo Double
                            )
  , cpOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                 -> J (JV fp) (Vector Double)
                 -> IO (Vec n (StageOutputs x o h q qo po deg Double))
  , cpTaus :: Vec deg Double
  , cpRoots :: QuadratureRoots
  , cpEvalQuadratures :: Vec n (Vec deg Double) -> Double -> IO Double
  , cpMetaProxy :: MetaProxy x z u p o q qo po h
  }


data QuadraturePlottingIn x z u p o q qo fp a =
  -- x z u p fp o q qo t T
  QuadraturePlottingIn (J x a) (J z a) (J u a) (J p a) (J o a) (J q a) (J qo a) (J fp a)
  (J (JV Id) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data QuadratureIn x z u p fp a =
  -- x' x z u p fp t T
  QuadratureIn (J x a) (J x a) (J z a) (J u a) (J p a) (J fp a)
               (J (JV Id) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data QuadratureStageIn x z u p fp deg a =
  -- xzus p fp ts h
  QuadratureStageIn (J (CollStage x z u deg) a) (J p a) (J fp a) (J (JVec deg (JV Id)) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data QuadratureStageOut q deg a =
  -- qdots qs qNext
  QuadratureStageOut (J (JVec deg q) a) (J (JVec deg q) a) (J q a)
  deriving (Generic, Generic1)

data PathCIn x z u p fp a =
  -- x' x z u p t
  PathCIn (J x a) (J x a) (J z a) (J u a) (J p a) (J fp a) (J (JV Id) a)
  deriving (Generic, Generic1)

data PathCStageIn x z u p fp deg a =
  -- xzus p fp ts h
  PathCStageIn (J (CollStage x z u deg) a) (J p a) (J fp a) (J (JVec deg (JV Id)) a) (J (JV Id) a)
  deriving (Generic, Generic1)

data DaeIn x z u p fp a =
  -- t p fp x' (CollPoint x z u)
  DaeIn (J (JV Id) a) (J p a) (J fp a) (J x a) (J (CollPoint x z u) a)
  deriving (Generic, Generic1)

data DaeOut r o a =
  -- r o
  DaeOut (J r a) (J o a)
  deriving (Generic, Generic1)

instance (View x, View z, View u, View p, View o, View q, View qo, View fp)
         => Scheme (QuadraturePlottingIn x z u p o q qo fp)
instance (View x, View z, View u, View p, View fp) => Scheme (QuadratureIn x z u p fp)
instance (View x, View z, View u, View p, View fp, Dim deg) => Scheme (QuadratureStageIn x z u p fp deg)
instance (View q, Dim deg) => Scheme (QuadratureStageOut q deg)
instance (View x, View z, View u, View p, View fp) => Scheme (PathCIn x z u p fp)
instance (View x, View z, View u, View p, View fp, Dim deg) => Scheme (PathCStageIn x z u p fp deg)
instance (View x, View z, View u, View p, View fp) => Scheme (DaeIn x z u p fp)
instance (View r, View o) => Scheme (DaeOut r o)


makeCollProblem ::
  forall x z u p r o c h q qo po fp deg n .
  ( Dim deg, Dim n
  , Vectorize x, Vectorize p, Vectorize u, Vectorize z
  , Vectorize r, Vectorize o, Vectorize h, Vectorize c, Vectorize q
  , Vectorize po, Vectorize fp, Vectorize qo
  )
  => QuadratureRoots
  -> OcpPhase x z u p r o c h q qo po fp
  -> OcpPhaseInputs x z u p c h fp
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> IO (CollProblem x z u p r o c h q qo po fp n deg)
makeCollProblem roots ocp ocpInputs guess = do
  let -- the collocation points
      taus :: Vec deg Double
      taus = mkTaus roots

      n = reflectDim (Proxy :: Proxy n)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
      cijs = lagrangeDerivCoeffs (0 TV.<| taus)

      interpolate' :: View f => (J f :*: J (JVec deg f)) MX -> J f MX
      interpolate' (x0 :*: xs) = case roots of
        Legendre -> interpolate taus x0 (unJVec (split xs))
        Radau -> TV.tvlast $ unJVec $ split xs

      dynamicsFunction :: DaeIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> DaeOut (JV r) (JV o) SX
      dynamicsFunction (DaeIn t parm fixedParm x' collPoint) = DaeOut (sxCatJV r) (sxCatJV o)
        where
          CollPoint x z u = split collPoint
          (r,o) = ocpDae ocp
                  (sxSplitJV x') (sxSplitJV x) (sxSplitJV z) (sxSplitJV u)
                  (sxSplitJV parm) (sxSplitJV fixedParm) (unId (sxSplitJV t))

  interpolateFun <- toMXFun "interpolate (JV x)" interpolate' >>= expandMXFun
  interpolateQFun <- toMXFun "interpolate (JV q)" interpolate' >>= expandMXFun
  interpolateQoFun <- toMXFun "interpolate (JV qo)" interpolate' >>= expandMXFun
  interpolateScalarFun <- toMXFun "interpolate (JV Id)" interpolate' >>= expandMXFun

  let callInterpolateScalar :: J (JV Id) MX -> Vec deg (J (JV Id) MX) -> J (JV Id) MX
      callInterpolateScalar x0 xs = call interpolateScalarFun (x0 :*: cat (JVec xs))

      callInterpolate :: J (JV x) MX -> Vec deg (J (JV x) MX) -> J (JV x) MX
      callInterpolate x0 xs = call interpolateFun (x0 :*: cat (JVec xs))

      callInterpolateQ :: J (JV q) MX -> Vec deg (J (JV q) MX) -> J (JV q) MX
      callInterpolateQ q0 qs = call interpolateQFun (q0 :*: cat (JVec qs))

      callInterpolateQo :: J (JV qo) MX -> Vec deg (J (JV qo) MX) -> J (JV qo) MX
      callInterpolateQo q0 qs = call interpolateQoFun (q0 :*: cat (JVec qs))

  let quadFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV q) SX
      quadFun (QuadratureIn x' x z u p fp t tf) = quad
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          quad :: J (JV q) SX
          quad = sxCatJV $ ocpQuadratures ocp
                 (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV fp) (sxSplitJV o)
                 (unId (sxSplitJV t)) (unId (sxSplitJV tf))

  let quadOutFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV qo) SX
      quadOutFun (QuadratureIn x' x z u p fp t tf) = quad
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          quad :: J (JV qo) SX
          quad = sxCatJV $ ocpQuadratureOutputs ocp
                 (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV fp) (sxSplitJV o)
                 (unId (sxSplitJV t)) (unId (sxSplitJV tf))

  let lagFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV Id) SX
      lagFun (QuadratureIn x' x z u p fp t tf) = lag
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          lag :: J (JV Id) SX
          lag = sxCatJV $ Id $ ocpLagrange ocp
                (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV fp) (sxSplitJV o)
                (unId (sxSplitJV t)) (unId (sxSplitJV tf))

  let pathCFun :: PathCIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV h) SX
      pathCFun (PathCIn x' x z u p fp t) = h
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          h :: J (JV h) SX
          h = sxCatJV $ ocpPathC ocp
              (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p) (sxSplitJV fp) (sxSplitJV o)
              (unId (sxSplitJV t))

  quadFunSX <- toSXFun "quadFun" quadFun
  quadOutFunSX <- toSXFun "quadOutFun" quadOutFun
  lagFunSX <- toSXFun "lagFun" lagFun
  pathCFunSX <- toSXFun "pathCFun" pathCFun

  let quadraturePlottingFun ::
        QuadraturePlottingIn (JV x) (JV z) (JV u) (JV p) (JV o) (JV q) (JV qo) (JV fp) SX -> J (JV po) SX
      quadraturePlottingFun (QuadraturePlottingIn x z u p o q qo fp t tf) =
        sxCatJV $ ocpPlotOutputs ocp (sxSplitJV x) (sxSplitJV z) (sxSplitJV u) (sxSplitJV p)
        (sxSplitJV o) (sxSplitJV q) (sxSplitJV qo) (sxSplitJV fp)
        (unId (sxSplitJV t)) (unId (sxSplitJV tf))
  quadPlotFunSX <- toSXFun "quadPlotFun" quadraturePlottingFun

  let -- later we could use the intermediate points as outputs, or in path cosntraints
      lagrangeStageFun :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
                          -> QuadratureStageOut (JV Id) deg MX
      lagrangeStageFun qIn = QuadratureStageOut (cat (JVec qdots)) (cat (JVec qs)) qNext
        where
          (qdots,qs,qNext) = toQuadratureFun n cijs callInterpolateScalar (call lagFunSX) qIn
      quadratureStageFun :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
                            -> QuadratureStageOut (JV q) deg MX
      quadratureStageFun qIn = QuadratureStageOut (cat (JVec qdots)) (cat (JVec qs)) qNext
        where
          (qdots,qs,qNext) = toQuadratureFun n cijs callInterpolateQ (call quadFunSX) qIn
      quadratureOutStageFun :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
                               -> QuadratureStageOut (JV qo) deg MX
      quadratureOutStageFun qIn = QuadratureStageOut (cat (JVec qdots)) (cat (JVec qs)) qNext
        where
          (qdots,qs,qNext) = toQuadratureFun n cijs callInterpolateQo (call quadOutFunSX) qIn
      pathCStageFun pcIn = cat (JVec hs)
        where
          hs = toPathCFun cijs (call pathCFunSX) pcIn
  lagrangeStageFunMX   <- toMXFun "lagrangeStageFun" $
    (\(QuadratureStageOut _ _ q) -> q) . lagrangeStageFun
  quadratureStageFunMX <- toMXFun "quadratureStageFun" $
    (\(QuadratureStageOut _ _ q) -> q) . quadratureStageFun
  pathCStageFunMX <- toMXFun "pathCStageFun" pathCStageFun


  bcFun <- toSXFun "bc" $ \(x0:*:x1:*:x2:*:x3:*:x4:*:x5) -> sxCatJV $ ocpBc ocp (sxSplitJV x0) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (sxSplitJV x4) (unId (sxSplitJV x5))
  mayerFun <- toSXFun "mayer" $ \(x0:*:x1:*:x2:*:x3:*:x4:*:x5) ->
    sxCatJV $ Id $ ocpMayer ocp (unId (sxSplitJV x0)) (sxSplitJV x1) (sxSplitJV x2) (sxSplitJV x3) (sxSplitJV x4) (sxSplitJV x5)

  dynFun <- toSXFun "dynamics" dynamicsFunction

  dynamicsStageFun <- toMXFun "dynamicsStageFunction" (toDynamicsStage callInterpolate cijs dynFun)
  callDynamicsStageFun <- fmap call (expandMXFun dynamicsStageFun)

  let nlp :: Nlp (CollTraj x z u p n deg) (JV fp) (CollOcpConstraints x r c h n deg) MX
      nlp = Nlp {
        nlpFG =
           getFg taus
           (bcFun :: SXFun (   J (JV x)
                           :*: J (JV x)
                           :*: J (JV q)
                           :*: J (JV p)
                           :*: J (JV fp)
                           :*: J (JV Id)
                           )
                           (J (JV c))
           )
           (mayerFun :: SXFun (   J (JV Id)
                              :*: J (JV x)
                              :*: J (JV x)
                              :*: J (JV q)
                              :*: J (JV p)
                              :*: J (JV fp)
                              )
                              (J (JV Id))
           )
           (call lagrangeStageFunMX)
           (call quadratureStageFunMX)
           (call pathCStageFunMX)
           (callDynamicsStageFun)
        , nlpBX = cat (ocpPhaseBx ocpInputs)
        , nlpBG = cat (ocpPhaseBg ocpInputs)
        , nlpX0 = guess :: J (CollTraj x z u p n deg) (Vector Double)
        , nlpP = catJV (ocpFixedP ocpInputs)
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
  lagrangeStageFunFullMX   <- toMXFun "lagrangeStageFunFull"   lagrangeStageFun
  quadratureStageFunFullMX <- toMXFun "quadratureStageFunFull" quadratureStageFun
  quadratureOutStageFunFullMX <- toMXFun "quadratureOutStageFunFull" quadratureOutStageFun

  outputFun <- toMXFun "stageOutputs" $ outputFunction callInterpolate cijs taus dynFun
  genericQuadraturesFun <- toMXFun "generic quadratures" $
                           genericQuadraturesFunction callInterpolateScalar cijs n

  let (getHellaOutputs, getPlotPoints, getOutputs) = toCallbacks n roots taus outputFun pathCStageFunMX lagrangeStageFunFullMX quadratureStageFunFullMX quadratureOutStageFunFullMX quadPlotFunSX

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
  forall x z u p fp r o h q qo po n deg
  . ( Vectorize x, Vectorize z, Vectorize u, Vectorize p
    , Vectorize o, Vectorize h, Vectorize r, Vectorize q
    , Vectorize po, Vectorize qo
    , Vectorize fp
    , Dim n, Dim deg
    )
  => Int
  -> QuadratureRoots
  -> Vec deg Double
  -> MXFun (   J (CollStage (JV x) (JV z) (JV u) deg)
           :*: J (JV p)
           :*: J (JV fp)
           :*: J (JV Id)
           :*: J (JV Id)
           )
           (   J (JVec deg (JV r))
           :*: J (JVec deg (JV x))
           :*: J (JVec deg (JV o))
           :*: J (JV x)
           )
  -> MXFun (PathCStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (J (JVec deg (JV h)))
  -> MXFun (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (QuadratureStageOut (JV Id) deg)
  -> MXFun (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (QuadratureStageOut (JV q) deg)
  -> MXFun (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (QuadratureStageOut (JV qo) deg)
  -> SXFun (QuadraturePlottingIn (JV x) (JV z) (JV u) (JV p) (JV o) (JV q) (JV qo) (JV fp)) (J (JV po))
  -> ( J (CollTraj x z u p n deg) (Vector Double)
       -> J (JV fp) (Vector Double)
          -> IO ( DynPlotPoints Double
                , Vec n (StageOutputs x o h q qo po deg Double)
                , Quadratures q qo Double
                )
     , J (CollTraj x z u p n deg) (Vector Double)
       -> J (JV fp) (Vector Double)
       -> IO (DynPlotPoints Double)
     , J (CollTraj x z u p n deg) (Vector Double)
       -> J (JV fp) (Vector Double)
       -> IO (Vec n (StageOutputs x o h q qo po deg Double))
     )
toCallbacks n roots taus outputFun pathStageConFun lagQuadFun quadFun quadOutFun quadPlotFun =
  (getHellaOutputs, getPlotPoints, getOutputs)
  where
    -- prepare callbacks
    f :: J (JV o) DMatrix ->  J (JV x) DMatrix -> J (JV h) DMatrix -> J (JV po) DMatrix
         -> Quadratures q qo Double -> Quadratures q qo Double
         -> ( J (JV o) (Vector Double), J (JV x) (Vector Double), J (JV h) (Vector Double)
            , J (JV po) (Vector Double)
            , Quadratures q qo Double, Quadratures q qo Double
            )
    f o' x' h' po' q q' = (d2v o', d2v x', d2v h', d2v po', q, q')

    callOutputFun :: J (JV p) (Vector Double)
                     -> J (JV fp) (Vector Double)
                     -> J (JV Id) (Vector Double)
                     -> J (JV Id) DMatrix
                     -> Quadratures q qo Double
                     -> ( J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double)
                        , J (JV Id) (Vector Double)
                        )
                     -> IO ( StageOutputs x o h q qo po deg Double
                           , Quadratures q qo Double
                           )
    callOutputFun p fp h tf previousQuadratures (stage, k) = do
      let p' = v2d p
          fp' = v2d fp
          stage' = v2d stage
      (_ :*: xdot :*: out :*: xnext) <-
        eval outputFun $ stage' :*: p' :*: fp' :*: (v2d h) :*: (v2d k)

      let stageTimes :: Vec deg (J (JV Id) DMatrix)
          stageTimes = fmap (\tau -> t0 + realToFrac tau * h') taus
            where
              t0 = h' * v2d k
          stageTimes' = cat (JVec stageTimes)
          h' = v2d h
          pathCStageIn = PathCStageIn stage' p' fp' stageTimes' h'
          quadratureStageIn = QuadratureStageIn stage' p' fp' stageTimes' h'
      hs <- eval pathStageConFun pathCStageIn
      QuadratureStageOut lagrQdots lagrQs lagrQNext <- eval lagQuadFun quadratureStageIn
      QuadratureStageOut userQdots userQs userQNext <- eval quadFun quadratureStageIn
      QuadratureStageOut outQdots   outQs  outQNext <- eval quadOutFun quadratureStageIn

      let outs0 = unJVec (split out) :: Vec deg (J (JV o) DMatrix)
          xdots0 = unJVec (split xdot) :: Vec deg (J (JV x) DMatrix)
          hs0 = unJVec (split hs) :: Vec deg (J (JV h) DMatrix)
          lagrQs0 = fmap (unId . splitJV . d2v) $ unJVec (split lagrQs) :: Vec deg Double
          userQs0 = fmap        (splitJV . d2v) $ unJVec (split userQs) :: Vec deg (q Double)
          outQs0  = fmap        (splitJV . d2v) $ unJVec (split  outQs) :: Vec deg (qo Double)
          lagrQdots0 = fmap (unId . splitJV . d2v) $ unJVec (split lagrQdots) :: Vec deg Double
          userQdots0 = fmap (splitJV . d2v) $ unJVec (split userQdots) :: Vec deg (q Double)
          outQdots0  = fmap (splitJV . d2v) $ unJVec (split  outQdots) :: Vec deg (qo Double)
          qdots = TV.tvzipWith3 Quadratures lagrQdots0 userQdots0 outQdots0
          qs    = fmap (previousQuadratures ^+^) $ TV.tvzipWith3 Quadratures lagrQs0 userQs0 outQs0

          nextQuadratures =
            Quadratures
            { qLagrange = unId (splitJV (d2v lagrQNext))
            , qUser = splitJV (d2v userQNext)
            , qOutputs = splitJV (d2v outQNext)
            } ^+^ previousQuadratures

      let quadPlotInputs ::
            Vec deg
            (QuadraturePlottingIn (JV x) (JV z) (JV u) (JV p) (JV o) (JV q) (JV qo) (JV fp) DMatrix)
          quadPlotInputs =
            toQuadPlotIn <$> xs <*> zs <*> us <*> outs0 <*> qUsers <*> qOuts <*> stageTimes
          qUsers = fmap (v2d . catJV . qUser) qs
          qOuts = fmap (v2d . catJV . qOutputs) qs
          (xs,zs,us) = TV.tvunzip3 $ fmap (toXzu . split) (unJVec (split xzus))
            where
              toXzu (CollPoint x z u) = (x, z, u)
              CollStage _ xzus = split stage'
          toQuadPlotIn x z u o q qo t = QuadraturePlottingIn x z u p' o q qo fp' t tf

      pos <- T.mapM (eval quadPlotFun) quadPlotInputs

      let stageOutputs =
            StageOutputs
            { soVec = TV.tvzipWith6 f outs0 xdots0 hs0 pos qs qdots
            , soXNext = d2v xnext
            , soQNext = nextQuadratures
            }

      return (stageOutputs, nextQuadratures)

    mapOutputFun :: J (CollTraj x z u p n deg) (Vector Double)
                    -> J (JV fp) (Vector Double)
                    -> IO ( Vec n (StageOutputs x o h q qo po deg Double)
                          , Quadratures q qo Double
                          )
    mapOutputFun ct fp = do
      let CollTraj tf p stages _ = split ct
          h = catJV $ Id (tf' / fromIntegral n)
            where
              Id tf' = splitJV tf

          vstages = unJVec (split stages)
              :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) (Vector Double))
          ks :: Vec n (J (JV Id) (Vector Double))
          ks = TV.mkVec' $ map (catJV . Id . realToFrac) (take n [(0::Int)..])

          quadratures0 :: Quadratures q qo Double
          quadratures0 = fill 0
      mapAccumM (callOutputFun p fp h (v2d tf)) quadratures0 (TV.tvzip vstages ks)

    getHellaOutputs ::
      J (CollTraj x z u p n deg) (Vector Double)
      -> J (JV fp) (Vector Double)
      -> IO ( DynPlotPoints Double
            , Vec n (StageOutputs x o h q qo po deg Double)
            , Quadratures q qo Double
            )
    getHellaOutputs traj fp = do
      (outputs, quadratures) <- mapOutputFun traj fp
      return (dynPlotPoints roots (split traj) outputs, outputs, quadratures)

    getPlotPoints :: J (CollTraj x z u p n deg) (Vector Double)
                  -> J (JV fp) (Vector Double)
                     -> IO (DynPlotPoints Double)
    getPlotPoints traj fp = do
      (dpp, _, _) <- getHellaOutputs traj fp
      return dpp

    getOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                  -> J (JV fp) (Vector Double)
                  -> IO (Vec n (StageOutputs x o h q qo po deg Double))
    getOutputs traj fp = do
      (outputs, _) <- mapOutputFun traj fp
      return outputs


getFg ::
  forall x z u p r c h q fp n deg .
  ( Dim deg, Dim n
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Vectorize r, Vectorize c, Vectorize h, Vectorize q, Vectorize fp
  )
  -- taus
  => Vec deg Double
  -- bcFun
  -> SXFun (   J (JV x)
           :*: J (JV x)
           :*: J (JV q)
           :*: J (JV p)
           :*: J (JV fp)
           :*: J (JV Id)
           )
           (J (JV c))
  -- mayerFun
  -> SXFun (   J (JV Id)
           :*: J (JV x)
           :*: J (JV x)
           :*: J (JV q)
           :*: J (JV p)
           :*: J (JV fp)
           )
           (J (JV Id))
  -- lagQuadFun
  -> (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> J (JV Id) MX)
  -- quadFun
  -> (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> J (JV q) MX)
  -- pathCStageFun
  -> (PathCStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> J (JVec deg (JV h)) MX)
  -- stageFun
  -> ( (   J (JV x)
       :*: J (JVec deg (JTuple (JV x) (JV z)))
       :*: J (JVec deg (JV u))
       :*: J (JV Id)
       :*: J (JV p)
       :*: J (JV fp)
       :*: J (JVec deg (JV Id))
       ) MX
       -> (   J (JVec deg (JV r))
          :*: J (JV x)
          ) MX
     )
  -- collTraj
  -> J (CollTraj x z u p n deg) MX
  -- parameter
  -> J (JV fp) MX
  -- (objective, constraints)
  -> (J (JV Id) MX, J (CollOcpConstraints x r c h n deg) MX)
getFg taus bcFun mayerFun lagQuadFun quadFun pathCStageFun dynamicsStageFun collTraj fixedParm = (obj, cat g)
  where
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) deg MX)

    obj = objLagrange + objMayer

    objMayer = call mayerFun (tf :*: x0 :*: xf :*: finalQuadratures :*: parm :*: fixedParm)

    objLagrange :: J (JV Id) MX
    objLagrange = F.sum $ TV.tvzipWith (oneQuadStage lagQuadFun) stages times'

    finalQuadratures :: J (JV q) MX
    finalQuadratures = F.sum $ TV.tvzipWith (oneQuadStage quadFun) stages times'

    oneQuadStage ::
      View qOrSomething
      => (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> J qOrSomething MX)
      -> J (CollStage (JV x) (JV z) (JV u) deg) MX
      -> J (JVec deg (JV Id)) MX
      -> J qOrSomething MX
    oneQuadStage qfun collStage stageTimes = qfun qInputs
      where
        qInputs :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
        qInputs = QuadratureStageIn collStage parm fixedParm stageTimes dt

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
        , coBc = call bcFun (x0 :*: xf :*: finalQuadratures :*: parm :*: fixedParm :*: tf)
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
          dynamicsStageFun (x0' :*: xzs :*: us :*: dt :*: parm :*: fixedParm :*: stageTimes)

        -- todo: don't split/cat this
        pathCStageIn = PathCStageIn (cat (CollStage x0 xzus)) parm fixedParm stageTimes dt
        stageHs = pathCStageFun pathCStageIn

        xzs = cat (JVec xzs') :: J (JVec deg (JTuple (JV x) (JV z))) MX
        us = cat (JVec us') :: J (JVec deg (JV u)) MX
        (xzs', us') = TV.tvunzip $ fmap toTuple $ unJVec (split xzus)
        toTuple xzu = (cat (JTuple x z), u)
          where
            CollPoint x z u = split xzu


ocpPhaseBx :: forall x z u p c h fp n deg .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  )
  => OcpPhaseInputs x z u p c h fp
  -> CollTraj x z u p n deg (Vector Bounds)
ocpPhaseBx ocpInputs =
  fillCollTraj'
  (fill (Nothing, Nothing))
  (ocpXbnd ocpInputs)
  (ocpZbnd ocpInputs)
  (ocpUbnd ocpInputs)
  (ocpPbnd ocpInputs)
  (ocpTbnd ocpInputs)

ocpPhaseBg :: forall x z u p r c h fp n deg .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize r, Vectorize c, Vectorize h
  )
  => OcpPhaseInputs x z u p c h fp
  -> CollOcpConstraints x r c h n deg (Vector Bounds)
ocpPhaseBg ocpInputs =
  CollOcpConstraints
  { coCollPoints = jreplicate (jfill (Just 0, Just 0)) -- dae residual constraint
  , coContinuity = jreplicate (jfill (Just 0, Just 0)) -- continuity constraint
  , coPathC = jreplicate (jreplicate hbnds)
  , coBc = catJV (ocpBcBnds ocpInputs)
  }
  where
    hbnds :: J (JV h) (Vector Bounds)
    hbnds = catJV (ocpPathCBnds ocpInputs)

toQuadratureFun ::
  forall x z u p fp q deg
  . ( View q, View x, View z, View u, Dim deg
    )
  => Int
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> (J q MX -> Vec deg (J q MX) -> J q MX)
  -> (QuadratureIn x z u p fp MX -> J q MX)
  -> QuadratureStageIn x z u p fp deg MX
  -> (Vec deg (J q MX), Vec deg (J q MX), J q MX)
toQuadratureFun n cijs interpolate' evalQuadDeriv (QuadratureStageIn collStage p fp stageTimes' h) =
  (qdots, qs, qnext)
  where
    CollStage x0 xzus' = split collStage
    xzus = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u MX)
    tf = h * fromIntegral n

    xs :: Vec deg (J x MX)
    xs = fmap (\(CollPoint x _ _) -> x) xzus

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    quadratureIns :: Vec deg (QuadratureIn x z u p fp MX)
    quadratureIns = TV.tvzipWith3 (\x' (CollPoint x z u) t -> QuadratureIn x' x z u p fp t tf)
                                  xdots xzus stageTimes

    qdots :: Vec deg (J q MX)
    qdots = fmap evalQuadDeriv quadratureIns

    stageTimes :: Vec deg (J (JV Id) MX)
    stageTimes = unJVec (split stageTimes')

    qnext :: J q MX
    qnext = interpolate' (0 :: J q MX) qs

    qs = fmap timesH qsOverH
      where
        timesH q = M.uncol $ M.ms (M.col q) h

    qsOverH :: Vec deg (J q MX)
    qsOverH = cijInvFr !* qdots

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
  forall x z u p fp h deg
  . ( View x, View z, View u, View h, Dim deg
    )
  => Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> (PathCIn x z u p fp MX -> J h MX)
  -> PathCStageIn x z u p fp deg MX
  -> Vec deg (J h MX)
toPathCFun cijs evalPathC (PathCStageIn collStage p fp stageTimes' h) = hs
  where
    CollStage x0 xzus' = split collStage
    xzus = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u MX)

    xs :: Vec deg (J x MX)
    xs = fmap (\(CollPoint x _ _) -> x) xzus

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    pathCIns :: Vec deg (PathCIn x z u p fp MX)
    pathCIns = TV.tvzipWith3 (\x' (CollPoint x z u) t -> PathCIn x' x z u p fp t)
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
  forall x z u p fp r o deg . (Dim deg, View x, View z, View u, View p, View fp, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double)
  -> SXFun (DaeIn x z u p fp) (DaeOut r o)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: J (JV Id) :*: J p :*: J fp :*: J (JVec deg (JV Id))) MX
  -> (J (JVec deg r) :*: J x) MX
toDynamicsStage interpolate' cijs dynFun (x0 :*: xzs' :*: us' :*: h :*: p :*: fp :*: stageTimes') =
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
        DaeOut r o = call dynFun (DaeIn t p fp x' collPoint)
        collPoint = cat (CollPoint x z u)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.vs` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap (\(JTuple x _) -> x) xzs


-- outputs
outputFunction ::
  forall x z u p fp r o deg . (Dim deg, View x, View z, View u, View p, View fp, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (TV.Succ deg) (Vec (TV.Succ deg) Double) -> Vec deg Double
  -> SXFun (DaeIn x z u p fp) (DaeOut r o)
  -> (J (CollStage x z u deg) :*: J p :*: J fp :*: J (JV Id) :*: J (JV Id)) MX
  -> (J (JVec deg r) :*: J (JVec deg x) :*: J (JVec deg o) :*: J x) MX
outputFunction callInterpolate cijs taus dynFun (collStage :*: p :*: fp :*: h :*: k) =
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
        DaeOut r o = call dynFun (DaeIn t p fp x' xzu)

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
  , Additive x
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


-- http://stackoverflow.com/questions/11652809/how-to-implement-mapaccumm
-- thanks rconner
mapAccumM :: (Monad m, Functor m, T.Traversable t) => (a -> b -> m (c, a)) -> a -> t b -> m (t c, a)
mapAccumM f = flip (runStateT . (T.traverse (StateT . (flip f))))
