{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Dyno.DirectCollocation.Formulate
       ( CollProblem(..), CollProblem'
       , DirCollOptions(..)
       , MapStrategy(..)
       , makeCollProblem
       , mkTaus
       , makeGuess
       , makeGuessSim
       , ocpPhaseBx
       , ocpPhaseBg
       , Default(..) -- for default options
       ) where

import GHC.Generics ( Generic, Generic1 )
import GHC.TypeLits
import GHC.TypeLits.Witnesses

import Control.Applicative
import Control.Monad.State ( StateT(..), runStateT )
import Data.Default.Class ( Default(..) )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Numeric.LinearAlgebra as Mat
import Linear hiding ( dot )
import Prelude -- BBP workaround

import Casadi.DM ( DM )
import Casadi.MX ( MX )
import Casadi.GenericType ( GType(..) )
import Casadi.SX ( SX )

import Dyno.Integrate ( InitialTime(..), TimeStep(..), rk45 )
import Dyno.View.View
       ( View(..), JTuple(..), J, S, JV
       , splitJV, catJV, jfill, v2d, d2v )
import Dyno.View.M ( M, vcat, vsplit )
import qualified Dyno.View.M as M
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.Fun
import Dyno.View.MapFun
import Dyno.View.JVec( JVec(..), jreplicate )
import Dyno.View.Scheme ( Scheme )
import Dyno.View.Vectorize ( Vectorize(..), Id(..), fill, vlength, unId )
import Dyno.TypeVecs ( Vec, Dim, reflectDim )
import qualified Dyno.TypeVecs as TV
import Dyno.LagrangePolynomials ( lagrangeDerivCoeffs )
import Dyno.Nlp ( Nlp(..), NlpIn(..), Bounds )
import Dyno.Ocp

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( MetaProxy(..), DynPlotPoints, dynPlotPoints )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots(..), mkTaus, interpolate )

data CollProblem x z u p r o c h q qo po fp n deg =
  CollProblem
  { cpNlp :: Nlp (CollTraj x z u p n deg)
                 (JV fp)
                 (CollOcpConstraints x p r c h n deg) MX
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
  , cpConstraints :: J (CollTraj x z u p n deg) (Vector Double)
                     -> J (JV fp) (Vector Double)
                     -> IO (J (CollOcpConstraints x p r c h n deg) (Vector Double))
  , cpOutputs :: J (CollTraj x z u p n deg) (Vector Double)
                 -> J (JV fp) (Vector Double)
                 -> IO (Vec n (StageOutputs x o h q qo po deg Double))
  , cpTaus :: Vec deg Double
  , cpDirCollOpts :: DirCollOptions
  , cpEvalQuadratures :: Vec n (Vec deg Double) -> Double -> IO Double
  , cpMetaProxy :: MetaProxy x z u p o q qo po h
--  , cpJacSparsitySpy :: String
--  , cpHessSparsitySpy :: String
  }

type CollProblem' ocp =
  CollProblem
  (X ocp)
  (Z ocp)
  (U ocp)
  (P ocp)
  (R ocp)
  (O ocp)
  (C ocp)
  (H ocp)
  (Q ocp)
  (QO ocp)
  (PO ocp)
  (FP ocp)

data DirCollOptions =
  DirCollOptions
  { collocationRoots :: QuadratureRoots -- ^ which collocation roots to use
  , mapStrategy :: MapStrategy
  , mapOptions :: M.Map String GType
  , unrollMapInHaskell :: Bool -- TODO(greg): remove this sooner or later
  } deriving Show

instance Default DirCollOptions where
  def =
    DirCollOptions
    { mapStrategy = Unroll
    , mapOptions = M.empty
    , collocationRoots = Radau
    , unrollMapInHaskell = False
    }

data QuadraturePlottingIn x z u p o q qo fp a =
  -- x0 xF x z u p fp o q qo t T
  QuadraturePlottingIn (J x a) (J x a) (J x a) (J z a) (J u a) (J p a) (J o a) (J q a) (J qo a) (J fp a)
  (S a) (S a)
  deriving (Generic, Generic1)

data QuadratureIn x z u p fp a =
  -- x' x z u p fp t T
  QuadratureIn (J x a) (J x a) (J z a) (J u a) (J p a) (J fp a)
               (S a) (S a)
  deriving (Generic, Generic1)

data QuadratureStageIn x z u p fp deg a =
  -- k xzusptf fp
  QuadratureStageIn (S a) (J (CollStage x z u p deg) a) (J fp a)
  deriving (Generic, Generic1)

data QuadratureStageOut q deg a =
  -- qdots qs qNext
  QuadratureStageOut (J (JVec deg q) a) (J (JVec deg q) a) (J q a)
  deriving (Generic, Generic1)

data PathCIn x z u p fp a =
  -- x' x z u p t
  PathCIn (J x a) (J x a) (J z a) (J u a) (J p a) (J fp a) (S a)
  deriving (Generic, Generic1)

data PathCStageIn x z u p fp deg a =
  -- xzusp fp ts
  PathCStageIn (J (CollStage x z u p deg) a) (J fp a) (J (JVec deg (JV Id)) a)
  deriving (Generic, Generic1)

data DaeIn x z u p fp a =
  -- t p fp x' (CollPoint x z u)
  DaeIn (S a) (J p a) (J fp a) (J x a) (J (CollPoint x z u) a)
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
  => DirCollOptions
  -> OcpPhase x z u p r o c h q qo po fp
  -> OcpPhaseInputs x z u p c h fp
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> IO (CollProblem x z u p r o c h q qo po fp n deg)
makeCollProblem dirCollOpts ocp ocpInputs guess = do
  let -- the collocation points
      roots = collocationRoots dirCollOpts
      taus :: Vec deg Double
      taus = mkTaus roots

      n = reflectDim (Proxy :: Proxy n)

      -- coefficients for getting xdot by lagrange interpolating polynomials
      cijs :: Vec (deg + 1) (Vec (deg + 1) Double)
      cijs =
        withNatOp (%+) (Proxy :: Proxy deg) (Proxy :: Proxy 1) $
        lagrangeDerivCoeffs (0 TV.<| taus)

      interpolate' :: View f => (J f :*: J (JVec deg f)) MX -> J f MX
      interpolate' (x0 :*: xs) = case roots of
        Legendre -> interpolate taus x0 (unJVec (split xs))
        Radau -> TV.tvlast $ unJVec $ split xs

      dynamicsFunction :: DaeIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> DaeOut (JV r) (JV o) SX
      dynamicsFunction (DaeIn t parm fixedParm x' collPoint) = DaeOut (vcat r) (vcat o)
        where
          CollPoint x z u = split collPoint
          (r,o) = ocpDae ocp
                  (vsplit x') (vsplit x) (vsplit z) (vsplit u)
                  (vsplit parm) (vsplit fixedParm) (unId (vsplit t))

  interpolateFun <- toFun "interpolate_JV_x" interpolate' mempty >>= expandFun
  interpolateQFun <- toFun "interpolate_JV_q" interpolate' mempty >>= expandFun
  interpolateQoFun <- toFun "interpolate_JV_qo" interpolate' mempty >>= expandFun
  interpolateScalarFun <- toFun "interpolate_JV_Id" interpolate' mempty >>= expandFun

  let callInterpolateScalar :: S MX -> Vec deg (S MX) -> S MX
      callInterpolateScalar x0 xs = callMX interpolateScalarFun (x0 :*: cat (JVec xs))

      callInterpolate :: J (JV x) MX -> Vec deg (J (JV x) MX) -> J (JV x) MX
      callInterpolate x0 xs = callMX interpolateFun (x0 :*: cat (JVec xs))

      callInterpolateQ :: J (JV q) MX -> Vec deg (J (JV q) MX) -> J (JV q) MX
      callInterpolateQ q0 qs = callMX interpolateQFun (q0 :*: cat (JVec qs))

      callInterpolateQo :: J (JV qo) MX -> Vec deg (J (JV qo) MX) -> J (JV qo) MX
      callInterpolateQo q0 qs = callMX interpolateQoFun (q0 :*: cat (JVec qs))

  let quadFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV q) SX
      quadFun (QuadratureIn x' x z u p fp t tf) = quad
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          quad :: J (JV q) SX
          quad = vcat $ ocpQuadratures ocp
                 (vsplit x) (vsplit z) (vsplit u) (vsplit p) (vsplit fp) (vsplit o)
                 (unId (vsplit t)) (unId (vsplit tf))

  let quadOutFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV qo) SX
      quadOutFun (QuadratureIn x' x z u p fp t tf) = quad
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          quad :: J (JV qo) SX
          quad = vcat $ ocpQuadratureOutputs ocp
                 (vsplit x) (vsplit z) (vsplit u) (vsplit p) (vsplit fp) (vsplit o)
                 (unId (vsplit t)) (unId (vsplit tf))

  let lagFun :: QuadratureIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> S SX
      lagFun (QuadratureIn x' x z u p fp t tf) = lag
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          lag :: S SX
          lag = vcat $ Id $ ocpLagrange ocp
                (vsplit x) (vsplit z) (vsplit u) (vsplit p) (vsplit fp) (vsplit o)
                (unId (vsplit t)) (unId (vsplit tf))

  let pathCFun :: PathCIn (JV x) (JV z) (JV u) (JV p) (JV fp) SX -> J (JV h) SX
      pathCFun (PathCIn x' x z u p fp t) = h
        where
          daeIn = DaeIn t p fp x' (cat (CollPoint x z u))
          DaeOut _ o = dynamicsFunction daeIn

          h :: J (JV h) SX
          h = vcat $ ocpPathC ocp
              (vsplit x) (vsplit z) (vsplit u) (vsplit p) (vsplit fp) (vsplit o)
              (unId (vsplit t))

  quadFunSX <- toFun "quadFun" quadFun mempty
  quadOutFunSX <- toFun "quadOutFun" quadOutFun mempty
  lagFunSX <- toFun "lagFun" lagFun mempty
  pathCFunSX <- toFun "pathCFun" pathCFun mempty

  let quadraturePlottingFun ::
        QuadraturePlottingIn (JV x) (JV z) (JV u) (JV p) (JV o) (JV q) (JV qo) (JV fp) SX
        -> J (JV po) SX
      quadraturePlottingFun (QuadraturePlottingIn x0 xF x z u p o q qo fp t tf) =
        vcat $ ocpPlotOutputs ocp (vsplit x0, vsplit xF)
        (vsplit x) (vsplit z) (vsplit u) (vsplit p)
        (vsplit o) (vsplit q) (vsplit qo) (vsplit fp)
        (unId (vsplit t)) (unId (vsplit tf))
  quadPlotFunSX <- toFun "quadPlotFun" quadraturePlottingFun mempty

  let -- later we could use the intermediate points as outputs, or in path cosntraints
      lagrangeStageFun :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
                          -> QuadratureStageOut (JV Id) deg MX
      lagrangeStageFun qIn = QuadratureStageOut (cat (JVec qdots)) (cat (JVec qs)) qNext
        where
          (qdots,qs,qNext) = toQuadratureFun n taus cijs callInterpolateScalar (callMX lagFunSX) qIn
      quadratureStageFun :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
                            -> QuadratureStageOut (JV q) deg MX
      quadratureStageFun qIn = QuadratureStageOut (cat (JVec qdots)) (cat (JVec qs)) qNext
        where
          (qdots,qs,qNext) = toQuadratureFun n taus cijs callInterpolateQ (callMX quadFunSX) qIn
      quadratureOutStageFun :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
                               -> QuadratureStageOut (JV qo) deg MX
      quadratureOutStageFun qIn = QuadratureStageOut (cat (JVec qdots)) (cat (JVec qs)) qNext
        where
          (qdots,qs,qNext) = toQuadratureFun n taus cijs callInterpolateQo (callMX quadOutFunSX) qIn
      pathCStageFun pcIn = cat (JVec hs)
        where
          hs = toPathCFun n cijs (callMX pathCFunSX) pcIn
  lagrangeStageFunMX   <- toFun "lagrangeStageFun"
    ((\(QuadratureStageOut _ _ q) -> q) . lagrangeStageFun) mempty
  quadratureStageFunMX <- toFun "quadratureStageFun"
    ((\(QuadratureStageOut _ _ q) -> q) . quadratureStageFun) mempty
  pathCStageFunMX <- toFun "pathCStageFun" pathCStageFun mempty


  bcFun <- toFun "bc" (\(x0:*:x1:*:x2:*:x3:*:x4:*:x5) -> vcat $ ocpBc ocp (vsplit x0) (vsplit x1) (vsplit x2) (vsplit x3) (vsplit x4) (unId (vsplit x5))) mempty
  mayerFun <- toFun "mayer"
    (\(x0:*:x1:*:x2:*:x3:*:x4:*:x5) ->
       vcat $ Id $ ocpMayer ocp (unId (vsplit x0)) (vsplit x1) (vsplit x2) (vsplit x3) (vsplit x4) (vsplit x5)) mempty

  dynFun <- toFun "dynamics" dynamicsFunction mempty

  dynamicsStageFun <-
    toFun "dynamicsStageFunction" (toDynamicsStage callInterpolate cijs dynFun) mempty
    >>= expandFun
    :: IO (Fun
           (J (JV x)
            :*: J (JVec deg (JTuple (JV x) (JV z)))
            :*: J (JVec deg (JV u))
            :*: S
            :*: J (JV p)
            :*: J (JV fp)
            :*: J (JVec deg (JV Id))
           )
           (J (JVec deg (JV r))
            :*: J (JV x)
           )
          )
--  let callDynamicsStageFun = callMX dynamicsStageFun

  -- fixedParm has to be repeated
  -- that is why it is a row matrix
  let stageFun :: (    S
                   :*: M (JV Id) (CollStage (JV x) (JV z) (JV u) (JV p) deg)
                   :*: M (JV Id) (JV fp)
                  ) MX ->
                  (    M (JV Id) (JVec deg (JV r))
                   :*: M (JV Id) (JVec deg (JV h))
                   :*: M (JV Id) (JV x)
                  ) MX
      stageFun (k :*: collStageRow :*: fixedParm') =
        (M.trans dc :*: M.trans stageHs :*: M.trans interpolatedX')
        where
          fixedParm = M.trans fixedParm'

          dt = tf / fromIntegral n

          stageTimes :: J (JVec deg (JV Id)) MX
          stageTimes = cat (JVec stageTimes')
            where
              stageT0 = k * dt
              stageTimes' = pure stageT0 ^+^ dt *^ fmap realToFrac taus

          collStage = M.trans collStageRow
          CollStage x0 xzus parm tf = split collStage
          dc :*: interpolatedX' =
            callMX dynamicsStageFun
            (x0 :*: xzs :*: us :*: dt :*: parm :*: fixedParm :*: stageTimes)

          pathCStageIn = PathCStageIn collStage fixedParm stageTimes
          stageHs = pathCStageFun pathCStageIn

          xzs = cat (JVec xzs') :: J (JVec deg (JTuple (JV x) (JV z))) MX
          us = cat (JVec us') :: J (JVec deg (JV u)) MX
          (xzs', us') = TV.tvunzip $ fmap toTuple $ unJVec (split xzus)
          toTuple xzu = (cat (JTuple x z), u)
            where
              CollPoint x z u = split xzu
  stageFunMX <- toFun "stageFun" stageFun mempty

  mapStageFunMX <- mapFun' (Proxy :: Proxy n) stageFunMX "mapDynamicsStageFun"
    (mapStrategy dirCollOpts) (mapOptions dirCollOpts)
-- use repeated outputs for now
    :: IO (Fun
           (   M (JV Id) (JVec n (JV Id))
           :*: M (JV Id) (JVec n (CollStage (JV x) (JV z) (JV u) (JV p) deg))
           :*: M (JV Id) (JVec n (JV fp))
           )
           (   M (JV Id) (JVec n (JVec deg (JV r)))
           :*: M (JV Id) (JVec n (JVec deg (JV h)))
           :*: M (JV Id) (JVec n (JV x))
           )
          )
---- non-repeated outputs don't work yet, and we need them for exact hessian
--    :: IO (Fun
--           (S
--            :*: M (JV Id) (JVec n (CollStage (JV x) (JV z) (JV u) deg))
--            :*: M (JV Id) (JVec n (JVec deg (JV Id)))
--            :*: M (JV Id) (JV p)
--            :*: M (JV Id) (JV fp)
--           )
--           (M (JV Id) (JVec n (JVec deg (JV r)))
--            :*: M (JV Id) (JVec n (JVec deg (JV h)))
--            :*: M (JV Id) (JVec n (JV x))
--           )
--          )
  let mapStageFun ::
        ( Vec n (S MX)
        , J (JVec n (CollStage (JV x) (JV z) (JV u) (JV p) deg)) MX
        , J (JV fp) MX
        )
        -> ( J (JVec n (JVec deg (JV r))) MX
           , J (JVec n (JVec deg (JV h))) MX
           , J (JVec n (JV x)) MX
           )
      mapStageFun (ks, stages, fixedParm')
        | unrollMapInHaskell dirCollOpts =
            let fixedParm = M.trans fixedParm'
                (dcs, hs, xnexts) =
                  TV.tvunzip3 $ f <$> ks <*> unJVec (split stages)
                f k stage = (M.trans dc, M.trans h, M.trans xnext)
                  where
                    dc :*: h :*: xnext =
                      callMX stageFunMX
                      (k :*: M.trans stage :*: fixedParm)
--                    dc :*: h :*: xnext =
--                      stageFun
--                      (dt :*: (M.trans stage) :*: (M.trans stageTimes) :*: fixedParm)
            in (cat (JVec dcs), cat (JVec hs), cat (JVec xnexts))

        | otherwise =
            let fixedParm = jreplicate fixedParm' :: J (JVec n (JV fp)) MX
                dcs :*: hs :*: xnexts =
                  callMX mapStageFunMX $
                  M.trans (cat (JVec ks)) :*: M.trans stages :*: M.trans fixedParm
            in (M.trans dcs, M.trans hs, M.trans xnexts)

  let nlp :: Nlp (CollTraj x z u p n deg) (JV fp) (CollOcpConstraints x p r c h n deg) MX
      nlp = Nlp {
        nlpFG =
           getFg
           (bcFun :: Fun (     J (JV x)
                           :*: J (JV x)
                           :*: J (JV q)
                           :*: J (JV p)
                           :*: J (JV fp)
                           :*: S
                           )
                           (J (JV c))
           )
           (mayerFun :: Fun (     S
                              :*: J (JV x)
                              :*: J (JV x)
                              :*: J (JV q)
                              :*: J (JV p)
                              :*: J (JV fp)
                              )
                              S
           )
           (callMX lagrangeStageFunMX)
           (callMX quadratureStageFunMX)
           mapStageFun
        , nlpIn =
            NlpIn
            { nlpBX = cat (ocpPhaseBx ocpInputs)
            , nlpBG = cat (ocpPhaseBg ocpInputs)
            , nlpX0 = guess :: J (CollTraj x z u p n deg) (Vector Double)
            , nlpP = catJV (ocpFixedP ocpInputs)
            , nlpLamX0 = Nothing
            , nlpLamG0 = Nothing
            }
        , nlpScaleF = ocpObjScale ocp
        , nlpScaleX = Just $ cat $ fillCollTraj
                      (fromMaybe (fill 1) (ocpXScale ocp))
                      (fromMaybe (fill 1) (ocpZScale ocp))
                      (fromMaybe (fill 1) (ocpUScale ocp))
                      (fromMaybe (fill 1) (ocpPScale ocp))
                      (fromMaybe       1  (ocpTScale ocp))

        , nlpScaleG = Just $ cat $ fillCollConstraints
                      (fromMaybe (fill 1) (ocpXScale ocp))
                      (fromMaybe (fill 1) (ocpPScale ocp))
                      (fromMaybe (fill 1) (ocpResidualScale ocp))
                      (fromMaybe (fill 1) (ocpBcScale ocp))
                      (fromMaybe (fill 1) (ocpPathCScale ocp))
                      (fromMaybe 1 (ocpTScale ocp))
        }

  -- callbacks and quadrature outputs
  lagrangeStageFunFullMX   <- toFun "lagrangeStageFunFull"   lagrangeStageFun mempty
  quadratureStageFunFullMX <- toFun "quadratureStageFunFull" quadratureStageFun mempty
  quadratureOutStageFunFullMX <- toFun "quadratureOutStageFunFull" quadratureOutStageFun mempty

  outputFun <- toFun "stageOutputs" (outputFunction n callInterpolate cijs taus dynFun) mempty
  genericQuadraturesFun <- toFun "generic_quadratures"
                           (genericQuadraturesFunction callInterpolateScalar cijs n)
                           mempty

  let (getHellaOutputs, getPlotPoints, getOutputs) = toCallbacks n roots taus outputFun pathCStageFunMX lagrangeStageFunFullMX quadratureStageFunFullMX quadratureOutStageFunFullMX quadPlotFunSX

      evalQuadratures :: Vec n (Vec deg Double) -> Double -> IO Double
      evalQuadratures qs' tf' = do
        let d2d :: Double -> S DM
            d2d = realToFrac
            qs :: Vec n (J (JVec deg (JV Id)) DM)
            qs = fmap (cat . JVec . fmap d2d) qs'
            tf :: S DM
            tf = realToFrac tf'
            evalq :: J (JVec deg (JV Id)) DM -> IO (S DM)
            evalq q = callDM genericQuadraturesFun (q :*: tf)
        stageIntegrals' <- T.mapM evalq qs :: IO (Vec n (S DM))
        let stageIntegrals = fmap (unId . splitJV . d2v) stageIntegrals' :: Vec n Double
        return (F.sum stageIntegrals)


  nlpConstraints <- toFun "nlp_constraints" (\(x:*:p) -> snd (nlpFG nlp x p)) mempty
  let evalConstraints x p = do
        g <- callDM nlpConstraints (v2d x :*: v2d p)
        return (d2v g)

  return $ CollProblem { cpNlp = nlp
                       , cpOcp = ocp
                       , cpPlotPoints = getPlotPoints
                       , cpHellaOutputs = getHellaOutputs
                       , cpConstraints = evalConstraints
                       , cpOutputs = getOutputs
                       , cpTaus = taus
                       , cpDirCollOpts = dirCollOpts
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
  -> Fun (     J (CollStage (JV x) (JV z) (JV u) (JV p) deg)
           :*: J (JV fp)
           :*: S
           )
           (   J (JVec deg (JV r))
           :*: J (JVec deg (JV x))
           :*: J (JVec deg (JV o))
           :*: J (JV x)
           )
  -> Fun (PathCStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (J (JVec deg (JV h)))
  -> Fun (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (QuadratureStageOut (JV Id) deg)
  -> Fun (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (QuadratureStageOut (JV q) deg)
  -> Fun (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg) (QuadratureStageOut (JV qo) deg)
  -> Fun (QuadraturePlottingIn (JV x) (JV z) (JV u) (JV p) (JV o) (JV q) (JV qo) (JV fp)) (J (JV po))
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
    f :: S DM
         -> J (JV o) DM ->  J (JV x) DM -> J (JV h) DM -> J (JV po) DM
         -> Quadratures q qo Double -> Quadratures q qo Double
         -> StageOutputsPoint x o h q qo po Double
    f t o' x' h' po' q q' =
      StageOutputsPoint
      { sopT = unId $ splitJV $ d2v t
      , sopO = d2v o'
      , sopXDot = d2v x'
      , sopH = d2v h'
      , sopPo = d2v po'
      , sopQs = q
      , sopQDots = q'
      }


    callOutputFun :: (J (JV x) DM, J (JV x) DM)
                     -> J (JV fp) (Vector Double)
                     -> Quadratures q qo Double
                     -> ( J (CollStage (JV x) (JV z) (JV u) (JV p) deg) (Vector Double)
                        , S (Vector Double)
                        )
                     -> IO ( StageOutputs x o h q qo po deg Double
                           , Quadratures q qo Double
                           )
    callOutputFun (x0,xF) fp previousQuadratures (stage, k) = do
      let fp' = v2d fp
          stage' = v2d stage
          CollStage stageX0 xzus p tf = split stage'
          h = tf / fromIntegral n
      (_ :*: xdot :*: out :*: xnext) <-
        callDM outputFun $ stage' :*: fp' :*: v2d k

      let stageTimes :: Vec deg (S DM)
          stageTimes = fmap (\tau -> stageT0 + realToFrac tau * h) taus
          stageT0 = h * v2d k
          stageTimes' = cat (JVec stageTimes)
          pathCStageIn = PathCStageIn stage' fp' stageTimes'
          quadratureStageIn = QuadratureStageIn (v2d k) stage' fp'
      hs <- callDM pathStageConFun pathCStageIn
      QuadratureStageOut lagrQdots lagrQs lagrQNext <- callDM lagQuadFun quadratureStageIn
      QuadratureStageOut userQdots userQs userQNext <- callDM quadFun quadratureStageIn
      QuadratureStageOut outQdots   outQs  outQNext <- callDM quadOutFun quadratureStageIn

      let outs0 = unJVec (split out) :: Vec deg (J (JV o) DM)
          xdots0 = unJVec (split xdot) :: Vec deg (J (JV x) DM)
          hs0 = unJVec (split hs) :: Vec deg (J (JV h) DM)
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
            (QuadraturePlottingIn (JV x) (JV z) (JV u) (JV p) (JV o) (JV q) (JV qo) (JV fp) DM)
          quadPlotInputs =
            toQuadPlotIn <$> xs <*> zs <*> us <*> outs0 <*> qUsers <*> qOuts <*> stageTimes
          qUsers = fmap (v2d . catJV . qUser) qs
          qOuts = fmap (v2d . catJV . qOutputs) qs
          (xs,zs,us) = TV.tvunzip3 $ fmap (toXzu . split) (unJVec (split xzus))
            where
              toXzu (CollPoint x z u) = (x, z, u)
          toQuadPlotIn x z u o q qo t = QuadraturePlottingIn x0 xF x z u p o q qo fp' t tf

      pos <- T.mapM (callDM quadPlotFun) quadPlotInputs

      let stageOutputs =
            StageOutputs
            { soX0 = d2v stageX0
            , soT0 = unId $ splitJV $ d2v stageT0
            , soVec = f <$> stageTimes <*> outs0 <*> xdots0 <*> hs0 <*> pos <*> qs <*> qdots
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
      let CollTraj _ _ stages xF = split ct

          vstages = unJVec (split stages)
              :: Vec n (J (CollStage (JV x) (JV z) (JV u) (JV p) deg) (Vector Double))
          ks :: Vec n (S (Vector Double))
          ks = TV.mkVec' $ map (catJV . Id . realToFrac) (take n [(0::Int)..])

          CollStage x0 _ _ _ = split (TV.tvhead vstages)
          quadratures0 :: Quadratures q qo Double
          quadratures0 = fill 0
      mapAccumM (callOutputFun (v2d x0, v2d xF) fp) quadratures0 (TV.tvzip vstages ks)

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
  -- bcFun
  => Fun (     J (JV x)
           :*: J (JV x)
           :*: J (JV q)
           :*: J (JV p)
           :*: J (JV fp)
           :*: S
           )
           (J (JV c))
  -- mayerFun
  -> Fun (     S
           :*: J (JV x)
           :*: J (JV x)
           :*: J (JV q)
           :*: J (JV p)
           :*: J (JV fp)
           )
           S
  -- lagQuadFun
  -> (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> S MX)
  -- quadFun
  -> (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> J (JV q) MX)
  -- stageFun
  -> ( ( Vec n (S MX)
       , J (JVec n (CollStage (JV x) (JV z) (JV u) (JV p) deg)) MX
       , J (JV fp) MX
       )
       -> ( J (JVec n (JVec deg (JV r))) MX
          , J (JVec n (JVec deg (JV h))) MX
          , J (JVec n (JV x)) MX
          )
     )
  -- collTraj
  -> J (CollTraj x z u p n deg) MX
  -- parameter
  -> J (JV fp) MX
  -- (objective, constraints)
  -> (S MX, J (CollOcpConstraints x p r c h n deg) MX)
getFg bcFun mayerFun lagQuadFun quadFun
  mapStageFun collTraj fixedParm = (obj, cat g)
  where
    -- split up the design vars
    CollTraj masterTf masterParm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) (JV p) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) (JV p) deg MX)

    obj = objLagrange + objMayer

    objMayer = callMX mayerFun (masterTf :*: x0 :*: xf :*: finalQuadratures :*: masterParm :*: fixedParm)

    objLagrange :: S MX
    objLagrange = F.sum $ oneQuadStage lagQuadFun <$> ks <*> stages

    finalQuadratures :: J (JV q) MX
    finalQuadratures = F.sum $ oneQuadStage quadFun <$> ks <*> stages

    oneQuadStage ::
      (QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX -> J qOrSomething MX)
      -> S MX
      -> J (CollStage (JV x) (JV z) (JV u) (JV p) deg) MX
      -> J qOrSomething MX
    oneQuadStage qfun k collStage = qfun qInputs
      where
        qInputs :: QuadratureStageIn (JV x) (JV z) (JV u) (JV p) (JV fp) deg MX
        qInputs = QuadratureStageIn k collStage fixedParm

    ks :: Vec n (S MX)
    ks = fmap realToFrac $ TV.mkVec' $ take n [(0::Int)..]
      where
        n = reflectDim (Proxy :: Proxy n)

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    -- parameter at each stage
    ps :: Vec n (J (JV p) MX)
    -- trajectory end time from each stage
    tfs :: Vec n (S MX)
    (x0s, ps, tfs) = TV.tvunzip3 $ fmap (\(CollStage x0' _ p' tf') -> (x0', p', tf')) spstages

    -- final point at each stage (for matching constraint)
    xfs :: Vec n (J (JV x) MX)
    xfs = TV.tvshiftl x0s xf

    x0 = (\(CollStage x0' _ _ _) -> x0') (TV.tvhead spstages)
    g = CollOcpConstraints
        { coCollPoints = dcs
        , coContinuity = integratorMatchingConstraints
        , coPathC = hs
        , coBc = callMX bcFun (x0 :*: xf :*: finalQuadratures :*: masterParm :*: fixedParm :*: masterTf)
        , coParams = cat $ JVec $ fmap (masterParm -) ps
        , coTfs = cat $ JVec $ fmap (masterTf -) tfs
        }

    integratorMatchingConstraints :: J (JVec n (JV x)) MX -- THIS SHOULD BE A NONLINEAR FUNCTION
    integratorMatchingConstraints = interpolatedXs - (cat (JVec xfs))

    dcs :: J (JVec n (JVec deg (JV r))) MX
    hs :: J (JVec n (JVec deg (JV h))) MX
    interpolatedXs :: J (JVec n (JV x)) MX
    (dcs, hs, interpolatedXs) = mapStageFun (ks, stages', fixedParm)


ocpPhaseBx :: forall x z u p c h fp n deg .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  )
  => OcpPhaseInputs x z u p c h fp
  -> CollTraj x z u p n deg (Vector Bounds)
ocpPhaseBx ocpInputs =
  CollTraj
  { ctTf = catJV (Id (ocpTbnd ocpInputs))
  , ctP = catJV (ocpPbnd ocpInputs)
  , ctStages = jreplicate (cat stageBounds)
  , ctXf = jfill (Nothing, Nothing)
  }
  where
    stageBounds :: CollStage (JV x) (JV z) (JV u) (JV p) deg (Vector Bounds)
    stageBounds = CollStage
                  (jfill (Nothing, Nothing))
                  (jreplicate (cat pointBounds))
                  (jfill (Nothing, Nothing))
                  (jfill (Nothing, Nothing))

    pointBounds :: CollPoint (JV x) (JV z) (JV u) (Vector Bounds)
    pointBounds = CollPoint
                  (catJV (ocpXbnd ocpInputs))
                  (catJV (ocpZbnd ocpInputs))
                  (catJV (ocpUbnd ocpInputs))

ocpPhaseBg :: forall x z u p r c h fp n deg .
  ( Dim n, Dim deg
  , Vectorize x, Vectorize p, Vectorize r, Vectorize c, Vectorize h
  )
  => OcpPhaseInputs x z u p c h fp
  -> CollOcpConstraints x p r c h n deg (Vector Bounds)
ocpPhaseBg ocpInputs =
  CollOcpConstraints
  { coCollPoints = jreplicate (jfill (Just 0, Just 0)) -- dae residual constraint
  , coContinuity = jreplicate (jfill (Just 0, Just 0)) -- continuity constraint
  , coPathC = jreplicate (jreplicate hbnds)
  , coBc = catJV (ocpBcBnds ocpInputs)
  , coParams = jreplicate (jfill (Just 0, Just 0))
  , coTfs = jreplicate (jfill (Just 0, Just 0))
  }
  where
    hbnds :: J (JV h) (Vector Bounds)
    hbnds = catJV (ocpPathCBnds ocpInputs)

toQuadratureFun ::
  forall x z u p fp q deg
  . ( View q, View x, View z, View u, View p, Dim deg
    )
  => Int
  -> Vec deg Double
  -> Vec (deg + 1) (Vec (deg + 1) Double)
  -> (J q MX -> Vec deg (J q MX) -> J q MX)
  -> (QuadratureIn x z u p fp MX -> J q MX)
  -> QuadratureStageIn x z u p fp deg MX
  -> (Vec deg (J q MX), Vec deg (J q MX), J q MX)
toQuadratureFun n taus cijs interpolate' evalQuadDeriv (QuadratureStageIn k collStage fp) =
  (qdots, qs, qnext)
  where
    CollStage x0 xzus' p tf = split collStage
    xzus = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u MX)
    h = tf / fromIntegral n

    xs :: Vec deg (J x MX)
    xs = fmap (\(CollPoint x _ _) -> x) xzus

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.ms` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    quadratureIns :: Vec deg (QuadratureIn x z u p fp MX)
    quadratureIns = TV.tvzipWith3 (\x' (CollPoint x z u) t -> QuadratureIn x' x z u p fp t tf)
                                  xdots xzus stageTimes

    qdots :: Vec deg (J q MX)
    qdots = fmap evalQuadDeriv quadratureIns

    stageTimes :: Vec deg (S MX)
    stageTimes = pure stageT0 ^+^ h *^ fmap realToFrac taus
      where
        stageT0 = k * h

    qnext :: J q MX
    qnext = interpolate' (0 :: J q MX) qs

    qs = fmap timesH qsOverH
      where
        timesH q = M.ms q h

    qsOverH :: Vec deg (J q MX)
    qsOverH = cijInvFr !* qdots

    cijs' :: Vec deg (Vec deg Double)
    cijs' = TV.tvtail $ fmap TV.tvtail cijs

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs'

    cijInv' :: Mat.Matrix Double
    cijInv' = Mat.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (J q MX))
    cijInvFr = fmap (fmap realToFrac) cijInv


toPathCFun ::
  forall x z u p fp h deg
  . ( View x, View z, View u, View p, Dim deg
    )
  => Int
  -> Vec (deg + 1) (Vec (deg + 1) Double)
  -> (PathCIn x z u p fp MX -> J h MX)
  -> PathCStageIn x z u p fp deg MX
  -> Vec deg (J h MX)
toPathCFun n cijs evalPathC (PathCStageIn collStage fp stageTimes') = hs
  where
    CollStage x0 xzus' p tf = split collStage
    h = tf / fromIntegral n
    xzus = fmap split (unJVec (split xzus')) :: Vec deg (CollPoint x z u MX)

    xs :: Vec deg (J x MX)
    xs = fmap (\(CollPoint x _ _) -> x) xzus

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.ms` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    pathCIns :: Vec deg (PathCIn x z u p fp MX)
    pathCIns = TV.tvzipWith3 (\x' (CollPoint x z u) t -> PathCIn x' x z u p fp t)
                                  xdots xzus stageTimes

    hs :: Vec deg (J h MX)
    hs = fmap evalPathC pathCIns

    stageTimes :: Vec deg (S MX)
    stageTimes = unJVec (split stageTimes')


-- todo: merging this with evaluateQuadraturesFunction would reduce duplication,
-- but could be inefficient
genericQuadraturesFunction ::
  forall deg
  . Dim deg
  => (S MX -> Vec deg (S MX) -> S MX)
  -> Vec (deg + 1) (Vec (deg + 1) Double)
  -> Int
  -> (J (JVec deg (JV Id)) :*: S) MX
  -> S MX
genericQuadraturesFunction interpolate' cijs' n (qdots' :*: tf) =
  dt * qnext
  where
    dt = tf / fromIntegral n

    qdots :: Vec deg (S MX)
    qdots = unJVec $ split qdots'

    qnext :: S MX
    qnext = interpolate' 0 qs

    qs = cijInvFr !* qdots

    cijs :: Vec deg (Vec deg Double)
    cijs = TV.tvtail $ fmap TV.tvtail cijs'

    cijMat :: Mat.Matrix Double
    cijMat = Mat.fromLists $ F.toList $ fmap F.toList cijs

    cijInv' :: Mat.Matrix Double
    cijInv' = Mat.inv cijMat

    cijInv :: Vec deg (Vec deg Double)
    cijInv = TV.mkVec' (map TV.mkVec' (Mat.toLists cijInv'))

    cijInvFr :: Vec deg (Vec deg (S MX))
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
  forall b deg x a
  . (Real b, Dim deg, Fractional (J x a))
  => Vec (deg + 1) (Vec (deg + 1) b)
  -> Vec (deg + 1) (J x a)
  -> Vec deg (J x a)
interpolateXDots cjks xs =
  withNatOp (%+) (Proxy :: Proxy deg) (Proxy :: Proxy 1) $
  TV.tvtail $ interpolateXDots' cjks xs

-- return dynamics constraints and interpolated state
toDynamicsStage ::
  forall x z u p fp r o deg . (Dim deg, View x, View z, View u, View p, View fp, View r, View o)
  => (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (deg + 1) (Vec (deg + 1) Double)
  -> Fun (DaeIn x z u p fp) (DaeOut r o)
  -> (J x :*: J (JVec deg (JTuple x z)) :*: J (JVec deg u) :*: S :*: J p :*: J fp :*: J (JVec deg (JV Id))) MX
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

    applyDae :: J x MX -> JTuple x z MX -> J u MX -> S MX -> (J r MX, J o MX)
    applyDae x' (JTuple x z) u t = (r, o)
      where
        DaeOut r o = callMX dynFun (DaeIn t p fp x' collPoint)
        collPoint = cat (CollPoint x z u)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.ms` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

    xs :: Vec deg (J x MX)
    xs = fmap (\(JTuple x _) -> x) xzs


-- outputs
outputFunction ::
  forall x z u p fp r o deg . (Dim deg, View x, View z, View u, View p, View fp, View r, View o)
  => Int
  -> (J x MX -> Vec deg (J x MX) -> J x MX)
  -> Vec (deg + 1) (Vec (deg + 1) Double) -> Vec deg Double
  -> Fun (DaeIn x z u p fp) (DaeOut r o)
  -> (J (CollStage x z u p deg) :*: J fp :*: S) MX
  -> (J (JVec deg r) :*: J (JVec deg x) :*: J (JVec deg o) :*: J x) MX
outputFunction n callInterpolate cijs taus dynFun (collStage :*: fp :*: k) =
  cat (JVec dynConstrs) :*: cat (JVec xdots) :*: cat (JVec outputs) :*: xnext
  where
    xzus = unJVec (split xzus') :: Vec deg (J (CollPoint x z u) MX)
    CollStage x0 xzus' p tf = split collStage
    -- times at each collocation point
    stageTimes :: Vec deg (S MX)
    stageTimes = fmap (\tau -> t0 + realToFrac tau * h) taus
    t0 = k*h
    h = tf / fromIntegral n

    xnext = callInterpolate x0 xs

    -- dae constraints (dynamics)
    dynConstrs :: Vec deg (J r MX)
    outputs :: Vec deg (J o MX)
    (dynConstrs, outputs) = TV.tvunzip $ TV.tvzipWith3 applyDae xdots xzus stageTimes

    applyDae :: J x MX -> J (CollPoint x z u) MX -> S MX -> (J r MX, J o MX)
    applyDae x' xzu t = (r, o)
      where
        DaeOut r o = callMX dynFun (DaeIn t p fp x' xzu)

    -- state derivatives, maybe these could be useful as outputs
    xdots :: Vec deg (J x MX)
    xdots = fmap (`M.ms` (1/h)) $ interpolateXDots cijs (x0 TV.<| xs)

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
  CollTraj (catJV (Id tf)) (catJV parm) guesses (catJV (guessX tf))
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

    mkGuess' :: (Double, Vec deg Double) -> CollStage (JV x) (JV z) (JV u) (JV p) deg (Vector Double)
    mkGuess' (t,ts) =
      CollStage (catJV (guessX t))
      (cat $ JVec $ fmap (\t' -> cat (CollPoint (catJV (guessX t')) (catJV (guessZ t')) (catJV (guessU t')))) ts)
      (catJV parm)
      (catJV (Id tf))

    guesses :: J (JVec n (CollStage (JV x) (JV z) (JV u) (JV p) deg)) (Vector Double)
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
  -> (Double -> x Double -> u Double -> x Double)
  -> (Double -> x Double -> u Double)
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
    stages :: Vec n (J (CollStage (JV x) (JV z) (JV u) (JV p) deg) (Vector Double))
    (xf, stages) = T.mapAccumL stageGuess x00 t0s

    stageGuess :: x Double -> Double
                  -> (x Double, J (CollStage (JV x) (JV z) (JV u) (JV p) deg) (Vector Double))
    stageGuess x0 t0 = (fst (integrate 1), cat (CollStage (catJV x0) points (catJV p) (catJV (Id tf))))
      where
        points = cat $ JVec $ fmap (toCollPoint . integrate) taus
        f :: Double -> x Double -> x Double
        f t x = ode t x u
          where
            u = guessU t x
        toCollPoint (x,u) = cat $ CollPoint (catJV x) (catJV (fill 0 :: z Double)) (catJV u)
        integrate localTau = (x, u)
          where
            t = localTau * dt
            x = rk45 f (InitialTime t0) (TimeStep t) x0
            u = guessU t x

    -- the collocation points
    taus :: Vec deg Double
    taus = mkTaus quadratureRoots


-- http://stackoverflow.com/questions/11652809/how-to-implement-mapaccumm
-- thanks rconner
mapAccumM :: (Monad m, T.Traversable t) => (a -> b -> m (c, a)) -> a -> t b -> m (t c, a)
mapAccumM f = flip (runStateT . (T.traverse (StateT . (flip f))))
