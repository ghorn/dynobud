{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}

module Dyno.Ocp
       ( OcpPhase(..)
       , OcpPhaseWithCov(..)
       , X
       , Z
       , U
       , P
       , R
       , O
       , C
       , H
       , Q
       ) where

import Data.Vector ( Vector )

import Dyno.View.JV ( JV )
import Dyno.View.View ( J )
import Dyno.View.Cov ( Cov )
import Dyno.Nlp ( Bounds )
import Dyno.SXElement ( SXElement )
--import Dyno.Vectorize

import Casadi.SX ( SX )
import Casadi.DMatrix ( DMatrix )

type Sx a = J a SX
type Sxe = SXElement

-- | differential state
type family X a :: * -> *
-- | algebraic variable
type family Z a :: * -> *
-- | control
type family U a :: * -> *
-- | parameter
type family P a :: * -> *
-- | dae residual
type family R a :: * -> *
-- | output
type family O a :: * -> *
-- | boundary condition
type family C a :: * -> *
-- | path constraint
type family H a :: * -> *
-- | quadrature state
type family Q a :: * -> *


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
-- > c(x(0), x(T), q(T), p) == 0
--
-- perhaps this should be:
--
-- > c(x(0), 0, x(T), T) == 0
data OcpPhase ocp =
  OcpPhase
  { -- | the Mayer term @Jm(T, x(0), x(T), q(T), p)@
    ocpMayer :: Sxe -> X ocp Sxe -> X ocp Sxe -> Q ocp Sxe -> P ocp Sxe -> Sxe
    -- | the Lagrange term @Jl(x(t),z(t),u(t),p,o,t,T)@
  , ocpLagrange :: X ocp Sxe -> Z ocp Sxe -> U ocp Sxe -> P ocp Sxe -> O ocp Sxe -> Sxe -> Sxe -> Sxe
    -- | derivative of quadrature state @q(x(t),z(t),u(t),p,o,t,T)@
  , ocpQuadratures :: X ocp Sxe -> Z ocp Sxe -> U ocp Sxe -> P ocp Sxe -> O ocp Sxe -> Sxe -> Sxe -> Q ocp Sxe
    -- | fully implicit differential-algebraic equation of the form:
    --
    -- > f(x'(t), x(t), z(t), u(t), p, t) == 0
  , ocpDae :: X ocp Sxe -> X ocp Sxe -> Z ocp Sxe -> U ocp Sxe -> P ocp Sxe -> Sxe -> (R ocp Sxe, O ocp Sxe)
    -- | the boundary conditions @clb <= c(x(0), x(T), q(T), T) <= cub@
  , ocpBc :: X ocp Sxe -> X ocp Sxe -> Q ocp Sxe -> P ocp Sxe -> Sxe -> C ocp Sxe
    -- | the path constraints @h(x(t), z(t), u(t), p, t)@
  , ocpPathC :: X ocp Sxe -> Z ocp Sxe -> U ocp Sxe -> P ocp Sxe -> O ocp Sxe -> Sxe -> H ocp Sxe
    -- | the boundary condition bounds @clb <= c(x(0), x(T)) <= cub@
  , ocpBcBnds :: C ocp Bounds
    -- | the path constraint bounds @(hlb, hub)@
  , ocpPathCBnds :: H ocp Bounds
    -- | differential state bounds @(xlb, xub)@
  , ocpXbnd :: X ocp Bounds
    -- | algebraic variable bounds @(zlb, zub)@
  , ocpZbnd :: Z ocp Bounds
    -- | control bounds @(ulb, uub)@
  , ocpUbnd :: U ocp Bounds
    -- | parameter bounds @(plb, pub)@
  , ocpPbnd :: P ocp Bounds
    -- | time bounds @(Tlb, Tub)@
  , ocpTbnd :: Bounds
    -- | scaling
  , ocpObjScale      :: Maybe Double
  , ocpTScale        :: Maybe Double
  , ocpXScale        :: Maybe (X ocp Double)
  , ocpZScale        :: Maybe (Z ocp Double)
  , ocpUScale        :: Maybe (U ocp Double)
  , ocpPScale        :: Maybe (P ocp Double)
  , ocpResidualScale :: Maybe (R ocp Double)
  , ocpBcScale       :: Maybe (C ocp Double)
  , ocpPathCScale    :: Maybe (H ocp Double)
  }

data OcpPhaseWithCov ocp sx sz sw sr sh shr sc =
  OcpPhaseWithCov
  { -- | the Mayer term @Jm(T, x(0), x(T), P(0), P(t))@
    ocpCovMayer :: Sxe -> X ocp Sxe -> X ocp Sxe -> Sx (Cov (JV sx)) -> Sx (Cov (JV sx)) -> Sxe
    -- | the Lagrange term @Jl(t, x(t), P(t), T)@
  , ocpCovLagrange :: Sxe -> X ocp Sxe -> Sx (Cov (JV sx)) -> Sxe -> Sxe
    -- | the system dynamics of the stage: @f(x'(t), x(t), z(t), u(t), p, t)@
  , ocpCovDae :: X ocp Sxe -> X ocp Sxe -> Z ocp Sxe -> U ocp Sxe -> P ocp Sxe -> Sxe
                 -> sx Sxe -> sx Sxe -> sz Sxe -> sw Sxe
                 -> sr Sxe
    -- | the projection from covariance state to full state
  , ocpCovProjection :: X ocp Sxe -> sx Sxe -> X ocp Sxe
    -- | constraints which (g(x) <= 0) will be satisfied with some margin defined by gamma
    -- .
    -- TODO: user upper and lower bounds without adding another constraint, probably impossible
  , ocpCovRobustifyPathC :: X ocp Sxe -> sx Sxe -> P ocp Sxe -> shr Sxe
    -- | robust factors for the robustified constraints
  , ocpCovGammas :: shr Double
    -- | covariance injection
  , ocpCovSq :: J (Cov (JV sw)) DMatrix
    -- | bounds on the initial convariance
  , ocpCovS0bnd :: J (Cov (JV sx)) (Vector Bounds)
    -- | the covariance boundary conditions @c(s(0), s(T))@
  , ocpCovSbc :: Sx (Cov (JV sx)) -> Sx (Cov (JV sx)) -> Sx sc
  , ocpCovSbcBnds :: J sc (Vector Bounds)
    -- | the covariance path constraints @h(s)@, only applied to first n Ss
  , ocpCovSh :: X ocp SXElement -> Sx (Cov (JV sx)) -> Sx sh
  , ocpCovShBnds :: J sh (Vector Bounds)
    -- | scaling
  , ocpCovSScale :: Maybe (J (Cov (JV sx)) (Vector Double))
  , ocpCovPathCScale :: Maybe (J sh (Vector Double))
  , ocpCovRobustPathCScale :: Maybe (shr Double)
  , ocpCovSbcScale :: Maybe (J sc (Vector Double))
  }
