{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Dyno.Ocp
       ( OcpPhase(..)
       , OcpPhaseInputs(..)
       , OcpPhaseWithCov(..)
       , OcpPhase'
       , OcpPhaseInputs'
       , X
       , Z
       , U
       , P
       , R
       , O
       , C
       , H
       , Q
       , QO
       , PO
       , FP
       ) where

import GHC.Generics ( Generic )

import Data.Serialize ( Serialize )
import Data.Vector ( Vector )

import Dyno.View.View ( J, JV )
import Dyno.View.Cov ( Cov )
import Dyno.Nlp ( Bounds )
import Dyno.Vectorize ( Id )

import Casadi.SX ( SX )
import Casadi.DMatrix ( DMatrix )

type Sx a = J a SX
type Sxe = J (JV Id) SX

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
-- | quadrature output states (for plotting only)
type family QO a :: * -> *
-- | plot outputs
type family PO a :: * -> *
-- | fixed (hardcoded) parameters
type family FP a :: * -> *

-- | OcpPhase using type families to compress type parameters
type OcpPhase' ocp = OcpPhase (X ocp) (Z ocp) (U ocp) (P ocp) (R ocp) (O ocp) (C ocp) (H ocp) (Q ocp) (QO ocp) (PO ocp) (FP ocp)

type OcpPhaseInputs' ocp = OcpPhaseInputs (X ocp) (Z ocp) (U ocp) (P ocp) (C ocp) (H ocp) (FP ocp)

-- | One stage of an optimal control problem, solvable as a stand-alone optimal control problem.
--
-- >        minimize           Jm(x(T),T) + integrate( Jl(x(t),z(t),u(t),p,p',t), {t,0,T} )
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
-- > hlb <= h(x(t), z(t), u(t), p, p', t) <= hub
--
-- dynamics constraints:
--
-- > f(x'(t), x(t), z(t), u(t), p, p', t) == 0
--
-- boundary conditions:
--
-- > c(x(0), x(T), q(T), p, p') == 0
--
-- perhaps this should be:
--
-- > c(x(0), 0, x(T), T) == 0
--
--
-- The OcpPhase data type has all the symbolics necessary to set up a problem.
-- The OcpPhaseInputs data type provides bounds on all parameters.
-- It is split up this way because setting up a problem takes considerable overhead,
-- so solving many problem with different OcpPhaseInputs can save time.
data OcpPhase x z u p r o c h q qo po fp =
  OcpPhase
  { -- | the Mayer term @Jm(T, x(0), x(T), q(T), p, p')@
    ocpMayer :: Sxe -> x Sxe -> x Sxe -> q Sxe -> p Sxe -> fp Sxe -> Sxe
    -- | the Lagrange term @Jl(x(t),z(t),u(t),p,p',o,t,T)@
  , ocpLagrange :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> fp Sxe -> o Sxe -> Sxe -> Sxe -> Sxe
    -- | derivative of quadrature state @q(x(t),z(t),u(t),p,o,p',t,T)@
  , ocpQuadratures :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> fp Sxe -> o Sxe -> Sxe -> Sxe -> q Sxe
    -- | same as ocpQuadratures, but only used for plotting
  , ocpQuadratureOutputs :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> fp Sxe -> o Sxe -> Sxe -> Sxe -> qo Sxe
    -- | fully implicit differential-algebraic equation of the form:
    --
    -- > f(x'(t), x(t), z(t), u(t), p, p', t) == 0
  , ocpDae :: x Sxe -> x Sxe -> z Sxe -> u Sxe -> p Sxe -> fp Sxe -> Sxe -> (r Sxe, o Sxe)
    -- | the boundary conditions @clb <= c(x(0), x(T), q(T), p, p', T) <= cub@
  , ocpBc :: x Sxe -> x Sxe -> q Sxe -> p Sxe -> fp Sxe -> Sxe -> c Sxe
    -- | the path constraints @hbl <= h(x(t), z(t), u(t), p, p', o, t) <= hbu@
  , ocpPathC :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> fp Sxe -> o Sxe -> Sxe -> h Sxe
    -- | things you might want to plot, like total energy - integral(power)
    --
    -- > po((x(0), x(T)), x(t), z(t), u(t), p, o(t), q(t), qo(t), fp, t, T)
  , ocpPlotOutputs :: (x Sxe, x Sxe) -> x Sxe -> z Sxe -> u Sxe -> p Sxe -> o Sxe -> q Sxe -> qo Sxe -> fp Sxe -> Sxe -> Sxe -> po Sxe
    -- | scaling
  , ocpObjScale      :: Maybe Double
  , ocpTScale        :: Maybe Double
  , ocpXScale        :: Maybe (x Double)
  , ocpZScale        :: Maybe (z Double)
  , ocpUScale        :: Maybe (u Double)
  , ocpPScale        :: Maybe (p Double)
  , ocpResidualScale :: Maybe (r Double)
  , ocpBcScale       :: Maybe (c Double)
  , ocpPathCScale    :: Maybe (h Double)
  }


-- | Inputs to an OcpPhase problem, used to solve several different problems with one OcpPhase.
data OcpPhaseInputs x z u p c h fp =
  OcpPhaseInputs
  { -- | the boundary condition bounds @clb <= c(x(0), x(T), q(T), p, p', T) <= cub@
    ocpBcBnds :: c Bounds
    -- | the path constraint bounds @hbl <= h(x(t), z(t), u(t), p, p', o, t) <= hbu@
  , ocpPathCBnds :: h Bounds
    -- | differential state bounds @xlb <= x(t) <=  xub@
  , ocpXbnd :: x Bounds
    -- | algebraic variable bounds @zlb <= z(t) <= zub@
  , ocpZbnd :: z Bounds
    -- | control bounds @ulb <= u(t) <= uub@
  , ocpUbnd :: u Bounds
    -- | parameter bounds @plb <= p <= pub@
  , ocpPbnd :: p Bounds
    -- | time bounds @Tlb <= T <=  Tub@
  , ocpTbnd :: Bounds
    -- | fixed parameters (not optimization variables)
  , ocpFixedP :: fp Double
  } deriving ( Generic )

instance ( Serialize (x Bounds)
         , Serialize (z Bounds)
         , Serialize (u Bounds)
         , Serialize (p Bounds)
         , Serialize (c Bounds)
         , Serialize (h Bounds)
         , Serialize (fp Double)
         ) => Serialize (OcpPhaseInputs x z u p c h fp)

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
  , ocpCovSh :: X ocp Sxe -> Sx (Cov (JV sx)) -> Sx sh
  , ocpCovShBnds :: J sh (Vector Bounds)
    -- | scaling
  , ocpCovSScale :: Maybe (J (Cov (JV sx)) (Vector Double))
  , ocpCovPathCScale :: Maybe (J sh (Vector Double))
  , ocpCovRobustPathCScale :: Maybe (shr Double)
  , ocpCovSbcScale :: Maybe (J sc (Vector Double))
  }
