{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Dyno.Fitting
       ( l1Fit, l1Fits, withL1Fit
       , l2Fit, l2Fits, withL2Fit
       , lInfFit, lInfFits, withLInfFit
       , L1X(..), GSlacks(..)
       ) where

import GHC.Generics ( Generic )

import Casadi.MX ( MX )
import Casadi.Option ( Opt(..) )
import Casadi.Overloading ( ArcTan2 )
import qualified Data.Map as M
import Data.Vector ( Vector )
import Data.Proxy ( Proxy(..) )

import Dyno.Nlp ( Bounds, NlpOut(..) )
import Dyno.NlpSolver ( NlpSolver, withNlpSolver )
import Dyno.Solvers ( Solver )
import Dyno.Vectorize ( Vectorize, Id(..) )
import Dyno.TypeVecs ( Dim, Vec )
import qualified Dyno.TypeVecs as TV
import Dyno.View.Fun ( Fun, SXFun, call, toSXFun )
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.JVec ( JVec(..) )
import Dyno.View.M ( M, mm, ones, reshape, reshape', sm, sumRows, trans, vcat, vsplit )
import Dyno.View.MapFun ( mapFun' )
import Dyno.View.View ( J, S, View(..), JTuple(..), JV, catJV, splitJV, jfill)

data L1X q y n a =
  L1X (J (JV q) a) (J (JVec n (JV y)) a)
  deriving Generic
instance (Vectorize q, Vectorize y, Dim n) => View (L1X q y n)

data GSlacks g y n a =
  GSlacks (J (JV g) a) (J (JVec n (JV y)) a) (J (JVec n (JV y)) a)
  deriving Generic
instance (Vectorize g, Vectorize y, Dim n) => View (GSlacks g y n)


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
  forall n q g x y
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> Maybe (q Double) -> q Bounds -> g Bounds -> M.Map String Opt
  -> Vec n (x Double, y Double) -> IO (Either String (q Double))
l1Fit eps solver fitModel qConstraints mq0 qbnds gbnds mapOpts featuresData =
  unId <$> l1Fits eps solver fitModel qConstraints mapOpts (Id input)
  where
    input :: (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
    input = (mq0, qbnds, gbnds, featuresData)


-- | Solve multiple L1 fitting problems with the same structure.
-- This is equivilent to but more efficient than calling
-- 'l1Fit' many times.
l1Fits ::
  forall n q g x y t
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Traversable t, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> M.Map String Opt
  -> t (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
  -> IO (t (Either String (q Double)))
l1Fits eps solver fitModel qConstraints mapOpts inputs =
  withL1Fit eps solver fitModel qConstraints mapOpts (\fit -> mapM fit inputs)


-- | Low level interface to L1 fitting.
withL1Fit ::
  forall n q g x y b
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> M.Map String Opt
  -> (((Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
       -> NlpSolver (L1X q y n)
                    (JTuple (JVec n (JV x)) (JVec n (JV y)))
                    (GSlacks g y n)
                    (Either String (q Double))
      ) -> NlpSolver (L1X q y n)
                     (JTuple (JVec n (JV x)) (JVec n (JV y)))
                     (GSlacks g y n)
                     b
     ) -> IO b
withL1Fit eps solver fitModel qConstraints mapOpts userFun = do
  let fitModel' (q :*: x :*: y :*: s) = f - y + s
        where
          f = vcat $ fitModel (vsplit q) (vsplit x)

  fitModelFun <- toSXFun "fit_model" fitModel'
                 :: IO (SXFun
                        (J (JV q) :*: J (JV x) :*: J (JV y) :*: J (JV y))
                        (J (JV y))
                       )

  mapFitModel <- mapFun' (Proxy :: Proxy n) "map_fit_model" fitModelFun mapOpts
                 :: IO (Fun
                        (J (JV q)
                         :*: M (JV x) (JVec n (JV Id))
                         :*: M (JV y) (JVec n (JV Id))
                         :*: M (JV y) (JVec n (JV Id))
                        )
                        (M (JV y) (JVec n (JV Id)))
                       )
  let fg :: J (L1X q y n) MX
            -> J (JTuple (JVec n (JV x)) (JVec n (JV y))) MX
            -> (S MX, J (GSlacks g y n) MX)
      fg dvs featuresData = (f, cat g)
        where
          fitFeatures :: J (JVec n (JV x)) MX
          fitData :: J (JVec n (JV y)) MX
          JTuple fitFeatures fitData = split featuresData

          q :: J (JV q) MX
          s' :: J (JVec n (JV y)) MX
          L1X q s' = split dvs

          s :: M (JV y) (JVec n (JV Id)) MX
          s = reshape s'

          ys :: M (JV y) (JVec n (JV Id)) MX
          ys = reshape fitData

          xs :: M (JV x) (JVec n (JV Id)) MX
          xs = reshape fitFeatures

          gs0 :: J (JVec n (JV y)) MX
          gs0 = reshape' $ call mapFitModel (q :*: xs :*: ys :*: (-s))

          gs1 :: J (JVec n (JV y)) MX
          gs1 = reshape' $ call mapFitModel (q :*: xs :*: ys :*: s)

          f = realToFrac eps `sm` trans q `mm` q + sumRows s'

          g :: GSlacks g y n MX
          g = GSlacks (vcat (qConstraints (vsplit q))) gs0 gs1

  let action solveOne = userFun solveOne'
        where
          solveOne' :: (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
                      -> NlpSolver
                         (L1X q y n)
                         (JTuple (JVec n (JV x)) (JVec n (JV y)))
                         (GSlacks g y n)
                         (Either String (q Double))
          solveOne' (mq0, qbnds, gbnds', featuresData) =
            fmap (fmap toSol) (solveOne x0 p xbnds gbnds)
            where
              toSol out = splitJV xopt
                where
                  L1X xopt _ = split (xOpt out)

              p :: J (JTuple (JVec n (JV x)) (JVec n (JV y))) (Vector Double)
              p = cat $ JTuple fs' ds'
                where
                  fitFeatures :: Vec n (x Double)
                  (fitFeatures, fitData) = TV.tvunzip featuresData
                  fs' = cat $ JVec $ fmap catJV fitFeatures
                  ds' = cat $ JVec $ fmap catJV fitData

              xbnds :: J (L1X q y n) (Vector Bounds)
              xbnds = cat $ L1X (catJV qbnds) (jfill (Nothing, Nothing))

              gbnds :: J (GSlacks g y n) (Vector Bounds)
              gbnds = cat $ GSlacks (catJV gbnds')
                      (jfill (Nothing, Just 0)) (jfill ((Just 0, Nothing)))

              x0 :: J (L1X q y n) (Vector Double)
              x0 = case mq0 of
                Nothing -> jfill 0
                Just q0 -> cat (L1X (catJV q0) (jfill 0))

  withNlpSolver solver fg Nothing Nothing Nothing Nothing action


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
  forall n q g x y
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> Maybe (q Double) -> q Bounds -> g Bounds -> M.Map String Opt
  -> Vec n (x Double, y Double) -> IO (Either String (q Double))
l2Fit eps solver fitModel qConstraints mq0 qbnds gbnds mapOpts featuresData = do
  unId <$> l2Fits eps solver fitModel qConstraints mapOpts (Id input)
  where
    input :: (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
    input = (mq0, qbnds, gbnds, featuresData)


-- | Solve multiple L2 fitting problems with the same structure.
-- This is equivilent to but more efficient than calling
-- 'l2Fit' many times.
l2Fits ::
  forall n q g x y t
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Traversable t, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> M.Map String Opt
  -> t (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
  -> IO (t (Either String (q Double)))
l2Fits eps solver fitModel qConstraints mapOpts inputs =
  withL2Fit eps solver fitModel qConstraints mapOpts (\fit -> mapM fit inputs)


-- | Low level interface to L2 fitting.
withL2Fit ::
  forall n q g x y b
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> M.Map String Opt
  -> (((Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
       -> NlpSolver (JV q)
                    (JTuple (JVec n (JV x)) (JVec n (JV y)))
                    (JV g)
                    (Either String (q Double))
      ) -> NlpSolver (JV q)
                     (JTuple (JVec n (JV x)) (JVec n (JV y)))
                     (JV g)
                     b
     ) -> IO b
withL2Fit eps solver fitModel qConstraints mapOpts userFun = do
  let fitModel' (q :*: x :*: y) = err * err
        where
          err = f - y
          f = vcat $ fitModel (vsplit q) (vsplit x)
  fitModelFun <- toSXFun "fit_model" fitModel'
                 :: IO (SXFun (J (JV q) :*: J (JV x) :*: J (JV y)) (J (JV y)))

  mapFitModel <- mapFun' (Proxy :: Proxy n) "map_fit_model" fitModelFun mapOpts
                 :: IO (Fun
                        (J (JV q)
                         :*: M (JV x) (JVec n (JV Id))
                         :*: M (JV y) (JVec n (JV Id))
                        )
                        (J (JV y))
                       )
  let fg :: J (JV q) MX -> J (JTuple (JVec n (JV x)) (JVec n (JV y))) MX
            -> (S MX, J (JV g) MX)
      fg q featuresData = (0.5 * f, g)
        where
          fitFeatures :: J (JVec n (JV x)) MX
          fitData :: J (JVec n (JV y)) MX
          JTuple fitFeatures fitData = split featuresData

          -- fit data
          ys :: M (JV y) (JVec n (JV Id)) MX
          ys = reshape fitData

          -- fit features
          xs :: M (JV x) (JVec n (JV Id)) MX
          xs = reshape fitFeatures

          -- objective function
          f :: S MX
          f = realToFrac eps `sm` trans q `mm` q + sumRows (call mapFitModel (q :*: xs :*: ys))

          -- nonlinear parameter constraints
          g :: J (JV g) MX
          g = vcat (qConstraints (vsplit q))

  let action solveOne = userFun solveOne'
        where
          solveOne' :: (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
                      -> NlpSolver
                         (JV q)
                         (JTuple (JVec n (JV x)) (JVec n (JV y)))
                         (JV g)
                         (Either String (q Double))
          solveOne' (mq0, qbnds, gbnds', featuresData) =
            fmap (fmap (splitJV . xOpt)) (solveOne x0 p xbnds gbnds)
            where
              p :: J (JTuple (JVec n (JV x)) (JVec n (JV y))) (Vector Double)
              p = cat $ JTuple fs' ds'
                where
                  fitFeatures :: Vec n (x Double)
                  (fitFeatures, fitData) = TV.tvunzip featuresData
                  fs' = cat $ JVec $ fmap catJV fitFeatures
                  ds' = cat $ JVec $ fmap catJV fitData
              xbnds = catJV qbnds
              gbnds = catJV gbnds'
              x0 = case mq0 of
                Nothing -> jfill 0
                Just q0 -> catJV q0
  withNlpSolver solver fg Nothing Nothing Nothing Nothing action


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
  forall n q g x y
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> Maybe (q Double) -> q Bounds -> g Bounds -> M.Map String Opt
  -> Vec n (x Double, y Double) -> IO (Either String (q Double))
lInfFit eps solver fitModel qConstraints mq0 qbnds gbnds mapOpts featuresData =
  unId <$> lInfFits eps solver fitModel qConstraints mapOpts (Id input)
  where
    input :: (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
    input = (mq0, qbnds, gbnds, featuresData)


-- | Solve multiple L-infinity fitting problems with the same structure.
-- This is equivilent to but more efficient than calling
-- 'lInfFit' many times.
lInfFits ::
  forall n q g x y t
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Traversable t, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> M.Map String Opt
  -> t (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
  -> IO (t (Either String (q Double)))
lInfFits eps solver fitModel qConstraints mapOpts inputs = do
  withLInfFit eps solver fitModel qConstraints mapOpts (\fit -> mapM fit inputs)


-- | Low-level interface to L-infinity fitting.
withLInfFit ::
  forall n q g x y b
  . (Vectorize q, Vectorize g, Vectorize x, Vectorize y, Dim n)
  => Double
  -> Solver
  -> (forall a . (Floating a, ArcTan2 a) => q a -> x a -> y a)
  -> (forall a . (Floating a, ArcTan2 a) => q a -> g a)
  -> M.Map String Opt
  -> (((Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
       -> NlpSolver (JTuple (JV q) (JV Id))
                    (JTuple (JVec n (JV x)) (JVec n (JV y)))
                    (GSlacks g y n)
                    (Either String (q Double))
      ) -> NlpSolver (JTuple (JV q) (JV Id))
                     (JTuple (JVec n (JV x)) (JVec n (JV y)))
                     (GSlacks g y n)
                     b
     ) -> IO b
withLInfFit eps solver fitModel qConstraints mapOpts userFun = do
  let fitModel' (q :*: x :*: y :*: s0) = f - y + s
        where
          s = s0 `sm` ones
          f = vcat $ fitModel (vsplit q) (vsplit x)

  fitModelFun <- toSXFun "fit_model" fitModel'
                 :: IO (SXFun
                        (J (JV q) :*: J (JV x) :*: J (JV y) :*: S)
                        (J (JV y))
                       )

  mapFitModel <- mapFun' (Proxy :: Proxy n) "map_fit_model" fitModelFun mapOpts
                 :: IO (Fun
                        (J (JV q)
                         :*: M (JV x) (JVec n (JV Id))
                         :*: M (JV y) (JVec n (JV Id))
                         :*: S
                        )
                        (M (JV y) (JVec n (JV Id)))
                       )

  let fg :: J (JTuple (JV q) (JV Id)) MX
            -> J (JTuple (JVec n (JV x)) (JVec n (JV y))) MX
            -> (S MX, J (GSlacks g y n) MX)
      fg dvs featuresData = (f, cat g)
        where
          fitFeatures :: J (JVec n (JV x)) MX
          fitData :: J (JVec n (JV y)) MX
          JTuple fitFeatures fitData = split featuresData

          f = realToFrac eps `sm` trans q `mm` q + s
          q :: J (JV q) MX
          s :: S MX
          JTuple q s = split dvs

          ys :: M (JV y) (JVec n (JV Id)) MX
          ys = reshape fitData

          xs :: M (JV x) (JVec n (JV Id)) MX
          xs = reshape fitFeatures

          gs0 :: J (JVec n (JV y)) MX
          gs0 = reshape' $ call mapFitModel (q :*: xs :*: ys :*: (-s))

          gs1 :: J (JVec n (JV y)) MX
          gs1 = reshape' $ call mapFitModel (q :*: xs :*: ys :*: s)

          g :: GSlacks g y n MX
          g = GSlacks (vcat (qConstraints (vsplit q))) gs0 gs1

  let action solveOne = userFun solveOne'
        where
          solveOne' :: (Maybe (q Double), q Bounds, g Bounds, Vec n (x Double, y Double))
                      -> NlpSolver
                         (JTuple (JV q) (JV Id))
                         (JTuple (JVec n (JV x)) (JVec n (JV y)))
                         (GSlacks g y n)
                         (Either String (q Double))
          solveOne' (mq0, qbnds, gbnds', featuresData) =
            fmap (fmap toSol) (solveOne x0 p xbnds gbnds)
            where
              toSol out = splitJV xopt
                where
                  JTuple xopt _ = split (xOpt out)

              p :: J (JTuple (JVec n (JV x)) (JVec n (JV y))) (Vector Double)
              p = cat $ JTuple fs' ds'
                where
                  fitFeatures :: Vec n (x Double)
                  (fitFeatures, fitData) = TV.tvunzip featuresData
                  fs' = cat $ JVec $ fmap catJV fitFeatures
                  ds' = cat $ JVec $ fmap catJV fitData
              xbnds = cat $ JTuple (catJV qbnds) (catJV (Id (Nothing, Nothing)))
              gbnds = cat $ GSlacks (catJV gbnds')
                      (jfill (Nothing, Just 0)) (jfill (Just 0, Nothing))
              x0 = case mq0 of
                Nothing -> jfill 0
                Just q0 -> cat (JTuple (catJV q0) (catJV (Id 0)))

  withNlpSolver solver fg Nothing Nothing Nothing Nothing action
