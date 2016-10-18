{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Dyno.DirectCollocation.FormulateCov
       ( CollCovProblem(..)
       , CovTraj(..)
       , makeCollCovProblem
       ) where

import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Foldable as F

import Casadi.DM ( DM )
import Casadi.MX ( MX )

import Dyno.View.M ( vcat, vsplit )
import Dyno.View.View ( View(..), J, S, JV, catJV, jfill, v2d, d2v )
import Dyno.View.Cov ( Cov )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.Fun
import Dyno.View.JVec( JVec(..), jreplicate )
import Dyno.View.Vectorize ( Vectorize(..), Id(..), None(..), fill, unId )
import Dyno.TypeVecs ( Dim, Vec, reflectDim )
import qualified Dyno.TypeVecs as TV
import Dyno.Nlp ( Nlp(..), NlpIn(..), Bounds )
import Dyno.Ocp

import Dyno.DirectCollocation.Types
import Dyno.DirectCollocation.Dynamic ( DynPlotPoints )
import Dyno.DirectCollocation.Quadratures ( timesFromTaus )
import Dyno.DirectCollocation.Robust
import Dyno.DirectCollocation.Formulate

data CollCovProblem ocp n deg sx sw sh shr sc =
  CollCovProblem
  { ccpNlp :: Nlp
              (CollTrajCov sx (X ocp) (Z ocp) (U ocp) (P ocp) n deg)
              (JV None)
              (CollOcpCovConstraints ocp n deg sh shr sc) MX
  , ccpPlotPoints :: J (CollTrajCov sx (X ocp) (Z ocp) (U ocp) (P ocp) n deg)
                     (Vector Double)
                     -> IO (DynPlotPoints Double)
  , ccpOutputs ::
       J (CollTrajCov sx (X ocp) (Z ocp) (U ocp) (P ocp) n deg) (Vector Double)
       -> IO ( Vec n (StageOutputs (X ocp) (O ocp) (H ocp) (Q ocp) (QO ocp) (PO ocp) deg Double)
             , Vec n (J (Cov (JV sx)) (Vector Double))
             , J (Cov (JV sx)) (Vector Double)
             )
  , ccpSensitivities :: Fun
                        (J (CollTraj' ocp n deg))
                        (CovarianceSensitivities (JV sx) (JV sw) n)
  , ccpCovariances :: Fun
                      (J (Cov (JV sx)) :*: J (CollTraj (X ocp) (Z ocp) (U ocp) (P ocp) n deg))
                      (J (CovTraj sx n))
  , ccpDirCollOpts :: DirCollOptions
  }




makeCollCovProblem ::
  forall ocp x z u p fp r o c h q qo po sx sz sw sr sh shr sc deg n .
  ( Dim deg, Dim n, Vectorize x, Vectorize p, Vectorize u, Vectorize z
  , Vectorize sr, Vectorize sw, Vectorize sz, Vectorize sx
  , Vectorize r, Vectorize o, Vectorize h, Vectorize c, Vectorize q, Vectorize po
  , Vectorize qo
  , View sh, Vectorize shr, View sc
  , x ~ X ocp
  , q ~ Q ocp
  , qo ~ QO ocp
  , h ~ H ocp
  , c ~ C ocp
  , o ~ O ocp
  , r ~ R ocp
  , p ~ P ocp
  , u ~ U ocp
  , z ~ Z ocp
  , po ~ PO ocp
  , fp ~ None
  , None ~ FP ocp
  )
  => DirCollOptions
  -> OcpPhase' ocp
  -> OcpPhaseInputs x z u p c h fp
  -> OcpPhaseWithCov ocp sx sz sw sr sh shr sc
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> IO (CollCovProblem ocp n deg sx sw sh shr sc)
makeCollCovProblem dirCollOpts ocp ocpInputs ocpCov guess = do
  let -- the collocation points
      roots = collocationRoots dirCollOpts
      taus :: Vec deg Double
      taus = mkTaus roots

  computeSensitivities <- mkComputeSensitivities roots (ocpCovDae ocpCov)
  computeCovariances <- mkComputeCovariances continuousToDiscreetNoiseApprox
                        (computeSensitivities) (ocpCovSq ocpCov)

  sbcFun <- toSXFun "sbc" $ \(x0:*:x1) -> ocpCovSbc ocpCov x0 x1
  shFun <- toSXFun "sh" $ \(x0:*:x1) -> ocpCovSh ocpCov (vsplit x0) x1
  mayerFun <- toSXFun "cov mayer" $ \(x0:*:x1:*:x2:*:x3:*:x4) ->
    vcat $ Id $ ocpCovMayer ocpCov (unId (vsplit x0)) (vsplit x1) (vsplit x2) x3 x4
  lagrangeFun <- toSXFun "cov lagrange" $ \(x0:*:x1:*:x2:*:x3) ->
    vcat $ Id $ ocpCovLagrange ocpCov (unId (vsplit x0)) (vsplit x1) x2 (unId (vsplit x3))

  cp0 <- makeCollProblem dirCollOpts ocp ocpInputs guess

  robustify <- mkRobustifyFunction (ocpCovProjection ocpCov) (ocpCovRobustifyPathC ocpCov)

  let nlp0 = cpNlp cp0
      gammas' = ocpCovGammas ocpCov :: shr Double

      gammas :: J (JV shr) MX
      gammas = vcat (fmap realToFrac gammas')

      rpathCUb :: shr Bounds
      rpathCUb = fill (Nothing, Just 0)

      robustPathCUb :: J (JV shr) (Vector Bounds)
      robustPathCUb = catJV rpathCUb

      -- the NLP
      fg :: J (CollTrajCov sx x z u p n deg) MX
            -> J (JV fp) MX
            -> (S MX, J (CollOcpCovConstraints ocp n deg sh shr sc) MX)
      fg = getFgCov taus
        computeCovariances
        gammas
        (robustify :: (J (JV shr) MX -> J (JV p) MX -> J (JV x) MX -> J (Cov (JV sx)) MX -> J (JV shr) MX))
        (sbcFun :: Fun (J (Cov (JV sx)) :*: J (Cov (JV sx))) (J sc))
        (shFun :: Fun (J (JV x) :*: J (Cov (JV sx))) (J sh))
        (lagrangeFun :: Fun (S :*: J (JV x) :*: J (Cov (JV sx)) :*: S) S)
        (mayerFun :: Fun (S :*: (J (JV x) :*: (J (JV x) :*: (J (Cov (JV sx)) :*: J (Cov (JV sx)))))) S)
        (nlpFG nlp0)

  computeCovariancesFun' <- toMXFun "compute covariances" (\(x :*: y) -> computeCovariances x y)
  -- callbacks
  let getPlotPoints :: J (CollTrajCov sx x z u p n deg) (Vector Double)
                       -> IO (DynPlotPoints Double)
      getPlotPoints collTrajCov = do
        let CollTrajCov _ collTraj = split collTrajCov
        cpPlotPoints cp0 collTraj (catJV None)

      getOutputs :: J (CollTrajCov sx x z u p n deg) (Vector Double)
                    -> IO ( Vec n (StageOutputs x o h q qo po deg Double)
                          , Vec n (J (Cov (JV sx)) (Vector Double))
                          , J (Cov (JV sx)) (Vector Double)
                          )
      getOutputs collTrajCov = do
        let CollTrajCov p0 collTraj = split collTrajCov
        outputs <- (cpOutputs cp0) collTraj (catJV None)
        covTraj <- fmap split $ callDM computeCovariancesFun' (v2d p0 :*: v2d collTraj)
        let covs' = ctAllButLast covTraj
            pF = ctLast covTraj
        let covs = unJVec (split covs') :: Vec n (J (Cov (JV sx)) DM)
        return (outputs, fmap d2v covs, d2v pF)

      nlp =
        Nlp
        { nlpFG = fg
        , nlpIn =
          NlpIn
          { nlpBX = cat $ CollTrajCov (ocpCovS0bnd ocpCov) (nlpBX (nlpIn nlp0))
          , nlpBG = cat $ CollOcpCovConstraints
                    { cocNormal = nlpBG (nlpIn nlp0)
                    , cocCovPathC = jreplicate (ocpCovShBnds ocpCov)
                    , cocCovRobustPathC = jreplicate robustPathCUb
                    , cocSbc = ocpCovSbcBnds ocpCov
                    }
          , nlpX0 = cat $ CollTrajCov (jfill 0) (nlpX0 (nlpIn nlp0))
          , nlpP = catJV None
          , nlpLamX0 = Nothing
          , nlpLamG0 = Nothing
          }
        , nlpScaleF = ocpObjScale ocp
        , nlpScaleX = Just $ cat $
                      CollTrajCov (fromMaybe (jfill 1) (ocpCovSScale ocpCov)) $
                      cat $ fillCollTraj
                      (fromMaybe (fill 1) (ocpXScale ocp))
                      (fromMaybe (fill 1) (ocpZScale ocp))
                      (fromMaybe (fill 1) (ocpUScale ocp))
                      (fromMaybe (fill 1) (ocpPScale ocp))
                      (fromMaybe       1  (ocpTScale ocp))

        , nlpScaleG = Just $ cat $ CollOcpCovConstraints
                      { cocNormal = cat $ fillCollConstraints
                                    (fromMaybe (fill 1) (ocpXScale ocp))
                                    (fromMaybe (fill 1) (ocpPScale ocp))
                                    (fromMaybe (fill 1) (ocpResidualScale ocp))
                                    (fromMaybe (fill 1) (ocpBcScale ocp))
                                    (fromMaybe (fill 1) (ocpPathCScale ocp))
                                    (fromMaybe 1 (ocpTScale ocp))
                      , cocCovPathC = jreplicate (fromMaybe (jfill 1) (ocpCovPathCScale ocpCov))
                      , cocCovRobustPathC = jreplicate $
                                            fromMaybe (jfill 1) $
                                            fmap catJV (ocpCovRobustPathCScale ocpCov)
                      , cocSbc = fromMaybe (jfill 1) (ocpCovSbcScale ocpCov)
                      }
        }
  computeSensitivitiesFun' <- toMXFun "compute sensitivities" computeSensitivities
  return $ CollCovProblem { ccpNlp = nlp
                          , ccpPlotPoints = getPlotPoints
                          , ccpOutputs = getOutputs
                          , ccpSensitivities = computeSensitivitiesFun'
                          , ccpCovariances = computeCovariancesFun'
                          , ccpDirCollOpts = dirCollOpts
                          }


getFgCov ::
  forall ocp x z u p r c h fp sx sh shr sc n deg .
  ( Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Vectorize h, Vectorize c, Vectorize r
  , Vectorize sx, View sc, View sh, Vectorize shr
  , X ocp ~ x
  , P ocp ~ p
  , R ocp ~ r
  , C ocp ~ c
  , H ocp ~ h
  )
  -- taus
  => Vec deg Double
  -> (J (Cov (JV sx)) MX -> J (CollTraj x z u p n deg) MX -> J (CovTraj sx n) MX)
  -- gammas
  -> J (JV shr) MX
  -- robustify
  -> (J (JV shr) MX -> J (JV p) MX -> J (JV x) MX -> J (Cov (JV sx)) MX -> J (JV shr) MX)
   -- sbcFun
  -> Fun (J (Cov (JV sx)) :*: J (Cov (JV sx))) (J sc)
   -- shFun
  -> Fun (J (JV x) :*: J (Cov (JV sx))) (J sh)
   -- lagrangeFun
  -> Fun
      (S :*: J (JV x) :*: J (Cov (JV sx)) :*: S) S
   -- mayerFun
  -> Fun
      (S :*: J (JV x) :*: J (JV x) :*: J (Cov (JV sx)) :*: J (Cov (JV sx))) S
  -> (J (CollTraj x z u p n deg) MX -> J (JV fp) MX
      -> (S MX, J (CollOcpConstraints' ocp n deg) MX)
     )
  -> J (CollTrajCov sx x z u p n deg) MX
  -> J (JV fp) MX
  -> (S MX, J (CollOcpCovConstraints ocp n deg sh shr sc) MX)
getFgCov
  taus computeCovariances
  gammas robustify sbcFun shFun lagrangeFun mayerFun
  normalFG collTrajCov nlpParams =
  (obj0 + objectiveLagrangeCov + objectiveMayerCov, cat g)
  where
    CollTrajCov p0 collTraj = split collTrajCov
    (obj0, g0) = normalFG collTraj nlpParams

    g = CollOcpCovConstraints
        { cocNormal = g0
        , cocCovPathC = cat (JVec covPathConstraints)
        , cocCovRobustPathC = cat (JVec robustifiedPathC)
        , cocSbc = callMX sbcFun (p0 :*: pF)
        }
    -- split up the design vars
    CollTraj tf parm stages' xf = split collTraj
    stages = unJVec (split stages') :: Vec n (J (CollStage (JV x) (JV z) (JV u) (JV p) deg) MX)
    spstages = fmap split stages :: Vec n (CollStage (JV x) (JV z) (JV u) (JV p) deg MX)

    objectiveMayerCov = callMX mayerFun (tf :*: x0 :*: xf :*: p0 :*: pF)

    -- timestep
    dt = tf / fromIntegral n
    n = reflectDim (Proxy :: Proxy n)

    -- times at each collocation point
    t0s :: Vec n (S MX)
    (t0s, _) = TV.tvunzip $ timesFromTaus 0 (fmap realToFrac taus) dt

    -- initial point at each stage
    x0s :: Vec n (J (JV x) MX)
    x0s = fmap (\(CollStage x0' _ _ _) -> x0') spstages -- todo(greg): lift out ps/tfs for turbo graph coloring

    x0 = (\(CollStage x0' _ _ _) -> x0') (TV.tvhead spstages) -- todo(greg): lift out ps/tfs for turbo graph coloring

--    sensitivities = callMX computeSensitivities collTraj

    covs :: Vec n (J (Cov (JV sx)) MX)
    covs = unJVec (split covs')

    covs' :: J (JVec n (Cov (JV sx))) MX -- all but last covariance
    pF :: J (Cov (JV sx)) MX -- last covariances
    CovTraj covs' pF = split (computeCovariances p0 collTraj)

    -- lagrange term
    objectiveLagrangeCov = (lagrangeF + lagrange0s) / fromIntegral n
      where
      lagrangeF = callMX lagrangeFun (tf :*: xf :*: pF :*: tf)
      lagrange0s =
        sum $ F.toList $
        TV.tvzipWith3 (\tk xk pk -> callMX lagrangeFun (tk :*: xk :*: pk :*: tf)) t0s x0s covs

    covPathConstraints :: Vec n (J sh MX)
    covPathConstraints = TV.tvzipWith (\xk pk -> callMX shFun (xk:*:pk)) x0s covs

    robustifiedPathC :: Vec n (J (JV shr) MX)
    robustifiedPathC = TV.tvzipWith (robustify gammas parm) x0s covs

