{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}

module Dyno.Ocp ( Dae, OcpPhase(..) ) where

import Data.Vector ( Vector )
import Dyno.View.View
import Dyno.Cov
import Dyno.Nlp ( Bounds )
import Dyno.Casadi.MX ( MX )
import Dyno.Casadi.DMatrix ( DMatrix )

-- | fully implicit differential-algebraic equation of the form:
--
-- > f(x'(t), x(t), z(t), u(t), p, t) == 0
type Dae x z u p r o a = J x a -> J x a -> J z a -> J u a -> J p a -> J S a -> (J r a, J o a)

-- | One stage of an optimal control problem, solvable as a stand-alone optimal control problem.
--
-- >        minimize           Jm(x(T),T) + integrate( Jl(x(t),z(t),u(t),p,t), {t,0,T} )
-- > x(.), z(.), u(.), p, T
-- >
-- > subject to:
--
-- bound constraints:
--
-- > Tlb <= T <= Tub
-- > xlb <= x <= xub
-- > zlb <= z <= zub
-- > ulb <= u <= uub
--
-- nonlinear path constraints
--
-- > hlb <= h(x(t), z(t), u(t), p, t) <= hub
--
-- dynamics constraints:
--
-- > f(x'(t), x(t), z(t), u(t), p, t) == 0
--
-- boundary conditions:
--
-- > c(x(0), x(T)) == 0
--
-- perhaps this should be:
--
-- > c(x(0), 0, x(T), T) == 0

type M a = J a MX

data OcpPhase x z u p r o c h s sh sc =
  OcpPhase { -- | the Mayer term @Jm(T, x(0), x(T), P(0), P(t))@
             ocpMayer :: M S -> M x -> M x -> M (Cov s) -> M (Cov s) -> M S
             -- | the Lagrange term @Jl(x(t),z(t),u(t),p,o,t)@
           , ocpLagrange :: M x -> M z -> M u -> M p -> M o -> M S -> M S
             -- | the system dynamics of the stage: @f(x'(t), x(t), z(t), u(t), p, t)@
           , ocpDae :: Dae x z u p r o MX
             -- | the boundary conditions @clb <= c(x(0), x(T)) <= cub@
           , ocpBc :: M x -> M x -> M c
             -- | the path constraints @h(x(t), z(t), u(t), p), t)@
           , ocpPathC :: M x -> M z -> M u -> M p -> M o -> M S -> M h
             -- | the boundary condition bounds @clb <= c(x(0), x(T)) <= cub@
           , ocpBcBnds :: J c (Vector Bounds)
             -- | the path constraint bounds @(hlb, hub)@
           , ocpPathCBnds :: J h (Vector Bounds)
             -- | differential state bounds @(xlb, xub)@
           , ocpXbnd :: J x (Vector Bounds)
             -- | algebraic variable bounds @(zlb, zub)@
           , ocpZbnd :: J z (Vector Bounds)
             -- | control bounds @(ulb, uub)@
           , ocpUbnd :: J u (Vector Bounds)
             -- | parameter bounds @(plb, pub)@
           , ocpPbnd :: J p (Vector Bounds)
             -- | time bounds @(Tlb, Tub)@
           , ocpTbnd :: J S (Vector Bounds)

             -- covariance stuff
           , ocpSq :: J (Cov s) DMatrix
           , ocpSbnd :: J (Cov s) (Vector Bounds)
             -- | the covariance boundary conditions @c(s(0), s(T))@
           , ocpSc :: M (Cov s) -> M (Cov s) -> M sc
           , ocpScBnds :: J sc (Vector Bounds)
             -- | the covariance path constraints @h(s)@, only applied to first n Ss
           , ocpSh :: M x -> M (Cov s) -> M sh
           , ocpShBnds :: J sh (Vector Bounds)
           }
