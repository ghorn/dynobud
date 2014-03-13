{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}

module Hascm.Ocp ( Dae, OcpPhase(..) ) where

import Hascm.Casadi.SXElement ( SXElement )

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

data OcpPhase x z u p r o c h =
  OcpPhase { -- | the Mayer term @Jm(x(T),T)@
             ocpMayer :: x SXElement -> SXElement -> SXElement
             -- | the Lagrange term @Jl(x(t),z(t),u(t),p,t)@
           , ocpLagrange :: x SXElement -> z SXElement -> u SXElement -> p SXElement -> o SXElement -> SXElement -> SXElement
             -- | the system dynamics of the stage: @f(x'(t), x(t), z(t), u(t), p, t)@
           , ocpDae :: Dae x z u p r o SXElement
             -- | the boundary conditions @c(x(0), x(T))@
           , ocpBc :: x SXElement -> x SXElement -> c SXElement
             -- | the path constraints @h(x(t), z(t), u(t), p), t)@
           , ocpPathC :: x SXElement -> z SXElement -> u SXElement -> p SXElement -> o SXElement -> SXElement -> h SXElement
             -- | the path constraint bounds @(hlb, hub)@
           , ocpPathCBnds :: h (Maybe Double, Maybe Double)
             -- | differential state bounds @(xlb, xub)@
           , ocpXbnd :: x (Maybe Double, Maybe Double)
             -- | algebraic variable bounds @(zlb, zub)@
           , ocpZbnd :: z (Maybe Double, Maybe Double)
             -- | control bounds @(ulb, uub)@
           , ocpUbnd :: u (Maybe Double, Maybe Double)
             -- | parameter bounds @(plb, pub)@
           , ocpPbnd :: p (Maybe Double, Maybe Double)
             -- | time bounds @(Tlb, Tub)@
           , ocpTbnd :: (Maybe Double, Maybe Double)
           }
