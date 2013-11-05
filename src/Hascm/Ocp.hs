{-# OPTIONS_GHC -Wall #-}

module Hascm.Ocp ( Dae, OcpPhase(..) ) where

-- | fully implicit differential-algebraic equation of the form:
--
-- > f(x'(t), x(t), z(t), u(t), p(t), t) == 0
type Dae x z u p r a = x a -> x a -> z a -> u a -> p a -> a -> r a

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
-- > hlb <= h(x(t), z(t), u(t), p(t), t) <= hub
--
-- dynamics constraints:
--
-- > f(x'(t), x(t), z(t), u(t), p(t), t) == 0
--
-- boundary conditions:
--
-- > c(x(0), x(T)) == 0
--
-- perhaps this should be:
--
-- > c(x(0), 0, x(T), T) == 0

data OcpPhase x z u p r c h a =
  OcpPhase { -- | the Mayer term @Jm(x(T),T)@
             ocpMayer :: x a -> a -> a
             -- | the Lagrange term @Jl(x(t),z(t),u(t),p,t)@
           , ocpLagrange :: x a -> z a -> u a -> p a -> a -> a
             -- | the system dynamics of the stage: @f(x'(t), x(t), z(t), u(t), p(t), t)@
           , ocpDae :: Dae x z u p r a
             -- | the boundary conditions @c(x(0), x(T))@
           , ocpBc :: x a -> x a -> c a
             -- | the path constraints @h(x(t), z(t), u(t), p(t), t)@
           , ocpPathC :: x a -> z a -> u a -> p a -> a -> h a
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
