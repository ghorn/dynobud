{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.Linearize
       ( linearize', linearize, linearizeDM', linearizeDM
       , OdeJacobian
       , ErrorOdeJacobian
       , makeOdeJacobian
       , makeErrorOdeJacobian
       , evalOdeJacobian
       , evalErrorOdeJacobian
       ) where

import Dyno.View.Vectorize ( Vectorize(..), Triple(..), None(..), unId, fill )
import Dyno.View.View
import Dyno.View.M
import Dyno.View.Fun
import Dyno.View.FunJac

import Casadi.MX ( MX )
import Casadi.SX ( SX )
import Casadi.DM ( DM )

toOdeSX ::
  (Vectorize x, Vectorize u, Vectorize w, Vectorize p, Vectorize sc, Vectorize o)
  => (x (S SX) -> u (S SX) -> w (S SX) -> p (S SX) -> sc (S SX)
      -> (x (S SX), o (S SX)))
  -> JacIn (JQuad (JV x) (JV u) (JV w) (JV p)) (J (JV sc)) SX
  -> JacOut (JTuple (JV x) (JV o)) (J JNone) SX
toOdeSX ode jacIn = jacOut
  where
    jacOut = JacOut (cat (JTuple (vcat dx) (vcat outputs))) (cat JNone)
    JacIn xuwp sc = jacIn
    JQuad x u w p = split xuwp
    (dx, outputs) =
      ode (vsplit x) (vsplit u) (vsplit w) (vsplit p) (vsplit sc)

toErrorOdeSX ::
  ( Vectorize x, Vectorize e, Vectorize u, Vectorize w
  , Vectorize p, Vectorize sc, Vectorize o)
  => (x (S SX) -> e (S SX) -> u (S SX) -> u (S SX) -> w (S SX) -> p (S SX)
      -> sc (S SX) -> (e (S SX), o (S SX)))
  -> JacIn (JQuad (JV e) (JV u) (JV w) (JV p)) (J (JV (Triple x u sc))) SX
  -> JacOut (JTuple (JV e) (JV o)) (J JNone) SX
toErrorOdeSX errorOde jacIn = jacOut
  where
    jacOut = JacOut (cat (JTuple (vcat de) (vcat outputs))) (cat JNone)
    JacIn euwp nominalInputs = jacIn
    JQuad e du w p = split euwp
    Triple fs0 u0 sc = vsplit nominalInputs
    (de, outputs) = errorOde fs0 (vsplit e) u0 (vsplit du) (vsplit w)
                    (vsplit p) sc

newtype OdeJacobian x u w p sc o =
  OdeJacobian
  (Fun
   (JacIn
    (JQuad (JV x) (JV u) (JV w) (JV p))
    (J (JV sc)))
   (Jac
    (JQuad (JV x) (JV u) (JV w) (JV p))
    (JTuple (JV x) (JV o))
    (J JNone)))

newtype ErrorOdeJacobian x e u w p sc o =
  ErrorOdeJacobian
  (Fun
   (JacIn
    (JQuad (JV e) (JV u) (JV w) (JV p))
    (J (JV (Triple x u sc))))
   (Jac
    (JQuad (JV e) (JV u) (JV w) (JV p))
    (JTuple (JV e) (JV o))
    (J JNone)))

makeOdeJacobian ::
  forall x u w p sc o
  . (Vectorize x, Vectorize u, Vectorize w, Vectorize p, Vectorize sc, Vectorize o)
  => (x (S SX) -> u (S SX) -> w (S SX) -> p (S SX) -> sc (S SX)
      -> (x (S SX), o (S SX)))
  -> IO (OdeJacobian x u w p sc o)
makeOdeJacobian ode = do
  f <- toFun "odeSX" (toOdeSX ode) mempty
  fmap OdeJacobian (toFunJac f)

makeErrorOdeJacobian ::
  ( Vectorize x, Vectorize e, Vectorize u, Vectorize w
  , Vectorize p, Vectorize sc, Vectorize o)
  => (x (S SX) -> e (S SX) -> u (S SX) -> u (S SX) -> w (S SX) -> p (S SX)
      -> sc (S SX) -> (e (S SX), o (S SX)))
  -> IO (ErrorOdeJacobian x e u w p sc o)
makeErrorOdeJacobian errorOde = do
  f <- toFun "errorOdeSX" (toErrorOdeSX errorOde) mempty
  fmap ErrorOdeJacobian (toFunJac f)


evalOdeJacobian ::
  forall x u w p sc o
  . ( Vectorize x, Vectorize u, Vectorize w
    , Vectorize p, Vectorize o, Vectorize sc
    )
  => OdeJacobian x u w p sc o
  -> x Double
  -> u Double
  -> p Double
  -> sc Double
  -> IO ( M (JV x) (JV x) DM
        , M (JV x) (JV u) DM
        , M (JV x) (JV w) DM
        , M (JV x) (JV p) DM
        , M (JV o) (JV x) DM
        , M (JV o) (JV u) DM
        , M (JV o) (JV w) DM
        , M (JV o) (JV p) DM
        , J (JV x) DM
        , J (JV o) DM
        )
evalOdeJacobian (OdeJacobian fj) x0 u0 p0 sc0 = do
  let w  = vcat (fill 0)
      x  = vcat (fmap realToFrac x0)
      u  = vcat (fmap realToFrac u0)
      p  = vcat (fmap realToFrac p0)
      sc = vcat (fmap realToFrac sc0)
      jacIn = JacIn (cat (JQuad x u w p)) sc
  jacOut <- callDM fj jacIn
  let Jac dxo_dxup xo' _ = jacOut
      (x',o) = vsplitTup xo'
      (dxo_dx,dxo_du,dxo_dw,dxo_dp) = hsplitQuad dxo_dxup
      (dx_dx, do_dx) = vsplitTup dxo_dx
      (dx_du, do_du) = vsplitTup dxo_du
      (dx_dw, do_dw) = vsplitTup dxo_dw
      (dx_dp, do_dp) = vsplitTup dxo_dp
  return (dx_dx, dx_du, dx_dw, dx_dp, do_dx, do_du, do_dw, do_dp, x', o)


evalErrorOdeJacobian ::
  forall x e u w p sc o
  . ( Vectorize x, Vectorize e, Vectorize u, Vectorize w
    , Vectorize p, Vectorize o, Vectorize sc
    )
  => ErrorOdeJacobian x e u w p sc o
  -> x Double
  -> u Double
  -> p Double
  -> sc Double
  -> IO ( M (JV e) (JV e) DM
        , M (JV e) (JV u) DM
        , M (JV e) (JV w) DM
        , M (JV e) (JV p) DM
        , M (JV o) (JV e) DM
        , M (JV o) (JV u) DM
        , M (JV o) (JV w) DM
        , M (JV o) (JV p) DM
        , J (JV e) DM
        , J (JV o) DM
        )
evalErrorOdeJacobian (ErrorOdeJacobian fj) x0 u0 p0 sc0 = do
  let e = vcat (fill 0)
      w = vcat (fill 0)
      du = vcat (fill 0)
      p  = vcat (fmap realToFrac p0)
      x0u0sc0 = vcat $ fmap realToFrac $ Triple x0 u0 sc0
      jacIn = JacIn (cat (JQuad e du w p)) x0u0sc0
  jacOut <- callDM fj jacIn
  let Jac dxo_dxup xo' _ = jacOut
      (x',o) = vsplitTup xo'
      (dxo_dx,dxo_du,dxo_dw,dxo_dp) = hsplitQuad dxo_dxup
      (dx_dx, do_dx) = vsplitTup dxo_dx
      (dx_du, do_du) = vsplitTup dxo_du
      (dx_dw, do_dw) = vsplitTup dxo_dw
      (dx_dp, do_dp) = vsplitTup dxo_dp
  return (dx_dx, dx_du, dx_dw, dx_dp, do_dx, do_du, do_dw, do_dp, x', o)

linearize' :: forall f g h p
              . (Vectorize f, Vectorize g, Vectorize h, Vectorize p)
              => (f (S MX) -> p (S MX) -> (g (S MX), h (S MX)))
              -> IO (f Double -> p Double -> IO (g (f Double), g Double, h Double))
linearize' userF = do
  funJac <- linearizeDM' userF

  let callFun :: f Double -> p Double -> IO (g (f Double), g Double, h Double)
      callFun f p = do
        (dfdg', g', h') <- funJac f p
        let _ = dfdg' :: M (JV g) (JV f) DM

        let g :: g Double
            g = splitJV (d2v g')

            h :: h Double
            h = splitJV (d2v h')

            dfdg  :: g (f Double)
            dfdg =  fmap (fmap (unId . splitJV . d2v) . hsplit) (vsplit dfdg')

        return (dfdg, g, h)

  return callFun


linearize :: forall f g
             . (Vectorize f, Vectorize g)
             => (f (S MX) -> g (S MX))
             -> IO (f Double -> IO (g (f Double), g Double))
linearize userF = do
  jac <- linearize' (\x None -> (userF x, None))
  let retFun x = do
        (dfdg, g, None) <- jac x None
        return (dfdg, g)
  return retFun

linearizeDM' :: forall f g h p
              . (Vectorize f, Vectorize g, Vectorize h, Vectorize p)
              => (f (S MX) -> p (S MX) -> (g (S MX), h (S MX)))
              -> IO (f Double -> p Double -> IO (M (JV g) (JV f) DM, J (JV g) DM, J (JV h) DM))
linearizeDM' userF = do
  let userF' :: JacIn (JV f) (J (JV p)) MX -> JacOut (JV g) (J (JV h)) MX
      userF' (JacIn x p) = JacOut (vcat g) (vcat h)
        where
          (g, h) = userF (vsplit x) (vsplit p)

  sxUserF <- toFun "yolo" userF' mempty
  jacUserF <- toFunJac sxUserF

  let callFun :: f Double -> p Double -> IO (M (JV g) (JV f) DM, J (JV g) DM, J (JV h) DM)
      callFun f p = do
        let jacIn :: JacIn (JV f) (J (JV p)) DM
            jacIn = JacIn (v2d (catJV f)) (v2d (catJV p))

        Jac dfdg g h <- callDM jacUserF jacIn
        return (dfdg, g, h)

  return callFun


linearizeDM :: forall f g
             . (Vectorize f, Vectorize g)
             => (f (S MX) -> g (S MX))
             -> IO (f Double -> IO (M (JV g) (JV f) DM, J (JV g) DM))
linearizeDM userF = do
  jac <- linearizeDM' (\x None -> (userF x, None))
  let retFun x = do
        (dfdg, g, _) <- jac x None
        return (dfdg, g)
  return retFun
