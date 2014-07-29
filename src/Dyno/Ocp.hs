{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}

module Dyno.Ocp ( Dae, OcpPhase(..) ) where

import Data.Vector ( Vector )
import Dyno.View.View
import Dyno.Cov
import Dyno.Nlp ( Bounds )
import Dyno.Casadi.SX ( SX )
import Dyno.Casadi.SXElement ( SXElement )
import Dyno.Casadi.DMatrix ( DMatrix )

-- | fully implicit differential-algebraic equation of the form:
--
-- > f(x'(t), x(t), z(t), u(t), p, t) == 0
type Dae x z u p r o a = x a -> x a -> z a -> u a -> p a -> a -> (r a, o a)

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

type Sx a = J a SX
type Sxe = SXElement

data OcpPhase x z u p r o c h s sh sc =
  OcpPhase { -- | the Mayer term @Jm(T, x(0), x(T), P(0), P(t))@
             ocpMayer :: Sxe -> x Sxe -> x Sxe -> Sx (Cov s) -> Sx (Cov s) -> Sxe
             -- | the Lagrange term @Jl(x(t),z(t),u(t),p,o,t)@
           , ocpLagrange :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> o Sxe -> Sxe -> Sxe
             -- | the system dynamics of the stage: @f(x'(t), x(t), z(t), u(t), p, t)@
           , ocpDae :: Dae x z u p r o SXElement
             -- | the boundary conditions @clb <= c(x(0), x(T)) <= cub@
           , ocpBc :: x Sxe -> x Sxe -> c Sxe
             -- | the path constraints @h(x(t), z(t), u(t), p, t)@
           , ocpPathC :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> o Sxe -> Sxe -> h Sxe
             -- | the boundary condition bounds @clb <= c(x(0), x(T)) <= cub@
           , ocpBcBnds :: c Bounds
             -- | the path constraint bounds @(hlb, hub)@
           , ocpPathCBnds :: h Bounds
             -- | differential state bounds @(xlb, xub)@
           , ocpXbnd :: x Bounds
             -- | algebraic variable bounds @(zlb, zub)@
           , ocpZbnd :: z Bounds
             -- | control bounds @(ulb, uub)@
           , ocpUbnd :: u Bounds
             -- | parameter bounds @(plb, pub)@
           , ocpPbnd :: p Bounds
             -- | time bounds @(Tlb, Tub)@
           , ocpTbnd :: Bounds

             -- covariance stuff
           , ocpSq :: J (Cov s) DMatrix
           , ocpSbnd :: J (Cov s) (Vector Bounds)
             -- | the covariance boundary conditions @c(s(0), s(T))@
           , ocpSbc :: Sx (Cov s) -> Sx (Cov s) -> Sx sc
           , ocpSbcBnds :: J sc (Vector Bounds)
             -- | the covariance path constraints @h(s)@, only applied to first n Ss
           , ocpSh :: x SXElement -> Sx (Cov s) -> Sx sh
           , ocpShBnds :: J sh (Vector Bounds)
           }
