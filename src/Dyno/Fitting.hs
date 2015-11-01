{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.Fitting
       ( l1Fit
       , l2Fit
       , lInfFit
       ) where

import GHC.Generics ( Generic )

import Casadi.MX ( MX )
import Casadi.Option ( Opt(..) )
import Casadi.Overloading ( ArcTan2 )
import qualified Data.Map as M
import Data.Proxy ( Proxy(..) )

import Dyno.Nlp ( Bounds, Nlp(..), NlpOut(..) )
import Dyno.NlpUtils ( solveNlp )
import Dyno.Solvers ( Solver )
import Dyno.Vectorize ( Vectorize, Id(..), None(..) )
import Dyno.TypeVecs ( Dim, Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.View.Fun ( Fun, SXFun, call, toSXFun )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.JVec ( JVec(..) )
import Dyno.View.M ( M, fromDMatrix, hcat', sumRows, trans, vcat, vsplit )
import Dyno.View.MapFun ( mapFun' )
import Dyno.View.View ( J, View(..), JTuple(..), JV, catJV, splitJV, jfill, v2d)

data L1X q n a =
  L1X (J (JV q) a) (J (JVec n (JV Id)) a)
  deriving Generic
instance (Vectorize q, Dim n) => View (L1X q n)

data GSlacks g n a =
  GSlacks (J (JV g) a) (J (JVec n (JV Id)) a) (J (JVec n (JV Id)) a)
  deriving Generic
instance (Vectorize g, Dim n) => View (GSlacks g n)

-- | Minimize the L1 norm of model mismatch.
--
-- > minimize:  || f(x_k, q) - y_k ||_1
-- >     q
-- >
-- > subject to:   qlb <=  q   <= qub
-- >               glb <= g(q) <= gub
--
-- reformulated as:
--
-- > minimize:              Sum(s_k)
-- >  q, s_k
-- > subject to:  qlb <=          q         <= qub
-- >              glb <=        g(q)        <= gub
-- >                     y_x - f(x_k) - s_k <= 0
-- >                0 <= y_x - f(x_k) + s_k
--
-- where q is the parameter vector, x_k are features, y_k are data,
-- and g is a nonlinear constraint on the parameters.
l1Fit ::
  forall n q g x
  . (Vectorize q, Vectorize g, Vectorize x, Dim n)
  => Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> q Bounds -> g Bounds -> M.Map String Opt
  -> Vec n (x Double, Double) -> IO (Either String (q Double))
l1Fit solver fitModel qConstraints qbnds gbnds mapOpts featuresData = do
  let fitModel' (q :*: x :*: y :*: s) = f - y + s
        where
          f = vcat $ Id (fitModel (vsplit q) (vsplit x))

  fitModelFun <- toSXFun "fit_model" fitModel'
                 :: IO (SXFun
                        (J (JV q) :*: J (JV x) :*: J (JV Id) :*: J (JV Id))
                        (J (JV Id))
                       )

  mapFitModel <- mapFun' (Proxy :: Proxy n) "map_fit_model" fitModelFun mapOpts
                 :: IO (Fun
                        (J (JV q)
                         :*: M (JV x) (JVec n (JV Id))
                         :*: M (JV Id) (JVec n (JV Id))
                         :*: M (JV Id) (JVec n (JV Id))
                        )
                        (M (JV Id) (JVec n (JV Id)))
                       )
  let fitFeatures :: Vec n (x Double)
      fitData :: Vec n Double
      (fitFeatures, fitData) = TV.tvunzip featuresData

  let fg :: J (L1X q n) MX -> J (JV None) MX -> (J (JV Id) MX, J (GSlacks g n) MX)
      fg dvs _ = (f, cat g)
        where
          q :: J (JV q) MX
          s' :: J (JVec n (JV Id)) MX
          L1X q s' = split dvs

          s :: M (JV Id) (JVec n (JV Id)) MX
          s = trans s'

          ys :: M (JV Id) (JVec n (JV Id)) MX
          ys = trans $ cat $ JVec (fmap realToFrac fitData)

          xs :: M (JV x) (JVec n (JV Id)) MX
          xs = fromDMatrix $ hcat' $ fmap (v2d . catJV) fitFeatures

          gs0 :: J (JVec n (JV Id)) MX
          gs0 = trans $ call mapFitModel (q :*: xs :*: ys :*: (-s))

          gs1 :: J (JVec n (JV Id)) MX
          gs1 = trans $ call mapFitModel (q :*: xs :*: ys :*: s)

          f = sumRows s'

          g :: GSlacks g n MX
          g = GSlacks (vcat (qConstraints (vsplit q))) gs0 gs1

  let nlp :: Nlp (L1X q n) (JV None) (GSlacks g n) MX
      nlp =
        Nlp
        { nlpFG = fg
        , nlpBG = cat $ GSlacks
                  (catJV gbnds)
                  (jfill (Nothing, Just 0))
                  (jfill (Just 0, Nothing))
        , nlpX0 = jfill 0
        , nlpBX = cat $ L1X
                  (catJV qbnds)
                  (jfill (Nothing, Nothing))
        , nlpP = catJV None
        , nlpLamX0 = Nothing
        , nlpLamG0 = Nothing
        , nlpScaleF = Nothing
        , nlpScaleX = Nothing
        , nlpScaleG = Nothing
        }

  (eret, out) <- solveNlp solver nlp Nothing
  let L1X xopt _ = split (xOpt out)

  return $ case eret of
    Left msg -> Left msg
    Right _ -> Right (splitJV xopt)



-- | Minimize the L2 norm of model mismatch.
--
-- > minimize:  0.5 * || f(x_k, q) - y_k ||_2^2
-- >     q
-- >
-- > subject to:   qlb <=  q   <= qub
-- >               glb <= g(q) <= gub
--
-- where q is the parameter vector, x_k are features, y_k are data,
-- and g is a nonlinear constraint on the parameters.
l2Fit ::
  forall n q g x
  . (Vectorize q, Vectorize g, Vectorize x, Dim n)
  => Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> q Bounds -> g Bounds -> M.Map String Opt
  -> Vec n (x Double, Double) -> IO (Either String (q Double))
l2Fit solver fitModel qConstraints qbnds gbnds mapOpts featuresData = do
  let fitModel' (q :*: x :*: y) = err * err
        where
          err = f - y
          f = vcat $ Id (fitModel (vsplit q) (vsplit x))
  fitModelFun <- toSXFun "fit_model" fitModel'
                 :: IO (SXFun (J (JV q) :*: J (JV x) :*: J (JV Id)) (J (JV Id)))

  mapFitModel <- mapFun' (Proxy :: Proxy n) "map_fit_model" fitModelFun mapOpts
                 :: IO (Fun
                        (J (JV q)
                         :*: M (JV x) (JVec n (JV Id))
                         :*: M (JV Id) (JVec n (JV Id))
                        )
                        (J (JV Id))
                       )
  let fitFeatures :: Vec n (x Double)
      fitData :: Vec n Double
      (fitFeatures, fitData) = TV.tvunzip featuresData

  let fg :: J (JV q) MX -> J (JV None) MX -> (J (JV Id) MX, J (JV g) MX)
      fg q _ = (0.5 * f, g)
        where
          -- fit data
          ys :: M (JV Id) (JVec n (JV Id)) MX
          ys = trans $ cat $ JVec (fmap realToFrac fitData)

          -- fit features
          xs :: M (JV x) (JVec n (JV Id)) MX
          xs = fromDMatrix $ hcat' $ fmap (v2d . catJV) fitFeatures

          -- objective function
          f :: J (JV Id) MX
          f = call mapFitModel (q :*: xs :*: ys)

          -- nonlinear parameter constraints
          g :: J (JV g) MX
          g = vcat (qConstraints (vsplit q))

  let nlp :: Nlp (JV q) (JV None) (JV g) MX
      nlp =
        Nlp
        { nlpFG = fg
        , nlpBG = catJV gbnds
        , nlpX0 = jfill 0
        , nlpBX = catJV qbnds
        , nlpP = catJV None
        , nlpLamX0 = Nothing
        , nlpLamG0 = Nothing
        , nlpScaleF = Nothing
        , nlpScaleX = Nothing
        , nlpScaleG = Nothing
        }

  (eret, out) <- solveNlp solver nlp Nothing
  return $ case eret of
    Left msg -> Left msg
    Right _ -> Right (splitJV (xOpt out))


-- | Minimize the L-infinity norm of model mismatch.
--
-- > minimize:  || f(x_k, q) - y_k ||_inf
-- >     q
-- >
-- > subject to:   qlb <=  q   <= qub
-- >               glb <= g(q) <= gub
--
-- reformulated as:
--
-- > minimize:                    s
-- >  q, s
-- > subject to:  qlb <=          q       <= qub
-- >              glb <=        g(q)      <= gub
-- >                     y_x - f(x_k) - s <= 0
-- >                0 <= y_x - f(x_k) + s
--
-- where q is the parameter vector, x_k are features, y_k are data,
-- and g is a nonlinear constraint on the parameters.
lInfFit ::
  forall n q g x
  . (Vectorize q, Vectorize g, Vectorize x, Dim n)
  => Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> q Bounds -> g Bounds -> M.Map String Opt
  -> Vec n (x Double, Double) -> IO (Either String (q Double))
lInfFit solver fitModel qConstraints qbnds gbnds mapOpts featuresData = do
  let fitModel' (q :*: x :*: y :*: s) = f - y + s
        where
          f = vcat $ Id (fitModel (vsplit q) (vsplit x))

  fitModelFun <- toSXFun "fit_model" fitModel'
                 :: IO (SXFun
                        (J (JV q) :*: J (JV x) :*: J (JV Id) :*: J (JV Id))
                        (J (JV Id))
                       )

  mapFitModel <- mapFun' (Proxy :: Proxy n) "map_fit_model" fitModelFun mapOpts
                 :: IO (Fun
                        (J (JV q)
                         :*: M (JV x) (JVec n (JV Id))
                         :*: M (JV Id) (JVec n (JV Id))
                         :*: J (JV Id)
                        )
                        (M (JV Id) (JVec n (JV Id)))
                       )
  let fitFeatures :: Vec n (x Double)
      fitData :: Vec n Double
      (fitFeatures, fitData) = TV.tvunzip featuresData

  let fg :: J (JTuple (JV q) (JV Id)) MX -> J (JV None) MX
            -> (J (JV Id) MX, J (GSlacks g n) MX)
      fg dvs _ = (s, cat g)
        where
          q :: J (JV q) MX
          s :: J (JV Id) MX
          JTuple q s = split dvs

          ys :: M (JV Id) (JVec n (JV Id)) MX
          ys = trans $ cat $ JVec (fmap realToFrac fitData)

          xs :: M (JV x) (JVec n (JV Id)) MX
          xs = fromDMatrix $ hcat' $ fmap (v2d . catJV) fitFeatures

          gs0 :: J (JVec n (JV Id)) MX
          gs0 = trans $ call mapFitModel (q :*: xs :*: ys :*: (-s))

          gs1 :: J (JVec n (JV Id)) MX
          gs1 = trans $ call mapFitModel (q :*: xs :*: ys :*: s)

          g :: GSlacks g n MX
          g = GSlacks (vcat (qConstraints (vsplit q))) gs0 gs1

  let nlp :: Nlp (JTuple (JV q) (JV Id)) (JV None) (GSlacks g n) MX
      nlp =
        Nlp
        { nlpFG = fg
        , nlpBG = cat $ GSlacks
                  (catJV gbnds)
                  (jfill (Nothing, Just 0))
                  (jfill (Just 0, Nothing))
        , nlpX0 = jfill 0
        , nlpBX = cat $ JTuple
                  (catJV qbnds)
                  (catJV (Id (Nothing, Nothing)))
        , nlpP = catJV None
        , nlpLamX0 = Nothing
        , nlpLamG0 = Nothing
        , nlpScaleF = Nothing
        , nlpScaleX = Nothing
        , nlpScaleG = Nothing
        }

  (eret, out) <- solveNlp solver nlp Nothing
  let JTuple xopt _ = split (xOpt out)

  return $ case eret of
    Left msg -> Left msg
    Right _ -> Right (splitJV xopt)
