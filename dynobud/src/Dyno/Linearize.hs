{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | utility functions for convenient linearization
module Dyno.Linearize
       ( linearize', linearize, linearizeDM', linearizeDM
       ) where

import Casadi.DM ( DM )
import Casadi.Matrix ( SMatrix )

import Dyno.Vectorize ( Vectorize(..), None(..), unId )
import Dyno.View.View
import Dyno.View.HList ( (:*:)(..) )
import Dyno.View.M
import Dyno.View.Fun


linearize' :: forall x f g p a
              . (Vectorize x, Vectorize f, Vectorize g, Vectorize p, SMatrix a)
              => (x (S a) -> p (S a) -> (f (S a), g (S a)))
              -> IO (x Double -> p Double -> IO (f (x Double), f Double, g Double))
linearize' userF = do
  funJac <- linearizeDM' userF

  let retFun :: x Double -> p Double -> IO (f (x Double), f Double, g Double)
      retFun x p = do
        (dfdx', f', g') <- funJac x p
        let _ = dfdx' :: M (JV f) (JV x) DM

            f :: f Double
            f = splitJV (d2v f')

            g :: g Double
            g = splitJV (d2v g')

            dfdx  :: f (x Double)
            dfdx =  fmap (fmap (unId . splitJV . d2v) . hsplit) (vsplit dfdx')

        return (dfdx, f, g)

  return retFun


linearize :: forall x f a
             . (Vectorize x, Vectorize f, SMatrix a)
             => (x (S a) -> f (S a))
             -> IO (x Double -> IO (f (x Double), f Double))
linearize userF = do
  jac <- linearize' (\x None -> (userF x, None))
  let retFun x = do
        (dfdx, f, None) <- jac x None
        return (dfdx, f)
  return retFun


linearizeDM' :: forall x f g p a
              . (Vectorize x, Vectorize f, Vectorize g, Vectorize p, SMatrix a)
              => (x (S a) -> p (S a) -> (f (S a), g (S a)))
              -> IO (x Double -> p Double -> IO (M (JV f) (JV x) DM, J (JV f) DM, J (JV g) DM))
linearizeDM' userF = do
  let userF' :: (J (JV x) :*: J (JV p)) a
             -> (M (JV f) (JV x) :*: J (JV f) :*: J (JV g)) a
      userF' (x :*: p) = jacobian f x :*: f :*: g
        where
          f = vcat f'
          g = vcat g'
          (f', g') = userF (vsplit x) (vsplit p)

  sxfun <- toFun "yolo" userF' mempty

  let retFun :: x Double -> p Double -> IO (M (JV f) (JV x) DM, J (JV f) DM, J (JV g) DM)
      retFun x p = do
        dfdx :*: f :*: g <- callDM sxfun (v2d (catJV x) :*: v2d (catJV p))
        return (dfdx, f, g)

  return retFun


linearizeDM :: forall x f a
             . (Vectorize x, Vectorize f, SMatrix a)
             => (x (S a) -> f (S a))
             -> IO (x Double -> IO (M (JV f) (JV x) DM, J (JV f) DM))
linearizeDM userF = do
  jac <- linearizeDM' (\x None -> (userF x, None))
  let retFun x = do
        (dfdg, g, _) <- jac x None
        return (dfdg, g)
  return retFun
