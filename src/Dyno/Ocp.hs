{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}

module Dyno.Ocp
       ( OcpPhase(..)
       , OcpPhaseWithCov(..)
       , OcpPhaseClass(..)
       ) where

import Data.Default ( Default(..) )
import Data.Vector ( Vector )

import Dyno.Vectorize ( Vectorize, None(..), fill )
import Dyno.View.JV
import Dyno.View.View
import Dyno.Cov
import Dyno.Nlp ( Bounds )
import Dyno.SXElement ( SXElement )

import Casadi.SX ( SX )
import Casadi.DMatrix ( DMatrix )

type Sx a = J a SX
type Sxe = SXElement

class OcpPhaseClass a where
  type X a :: * -> *
  type Z a :: * -> *
  type U a :: * -> *
  type P a :: * -> *
  type R a :: * -> *
  type O a :: * -> *
  type C a :: * -> *
  type H a :: * -> *

instance OcpPhaseClass (OcpPhase x z u p r o c h) where
  type X (OcpPhase x z u p r o c h) = x
  type Z (OcpPhase x z u p r o c h) = z
  type U (OcpPhase x z u p r o c h) = u
  type P (OcpPhase x z u p r o c h) = p
  type R (OcpPhase x z u p r o c h) = r
  type O (OcpPhase x z u p r o c h) = o
  type C (OcpPhase x z u p r o c h) = c
  type H (OcpPhase x z u p r o c h) = h

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
  OcpPhase
  { -- | the Mayer term @Jm(T, x(0), x(T))@
    ocpMayer :: Sxe -> x Sxe -> x Sxe -> Sxe
    -- | the Lagrange term @Jl(x(t),z(t),u(t),p,o,t,T)@
  , ocpLagrange :: x Sxe -> z Sxe -> u Sxe -> p Sxe -> o Sxe -> Sxe -> Sxe -> Sxe
    -- | fully implicit differential-algebraic equation of the form:
    --
    -- > f(x'(t), x(t), z(t), u(t), p, t) == 0
  , ocpDae :: x Sxe -> x Sxe -> z Sxe -> u Sxe -> p Sxe -> Sxe -> (r Sxe, o Sxe)
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
instance (Vectorize x, Vectorize z, Vectorize u, Vectorize p)
         => Default (OcpPhase x z u p r o None None) where
  def =
    OcpPhase
    { ocpMayer = \_ _ _ -> 0
    , ocpLagrange = \_ _ _ _ _ _ _ -> 0
    , ocpDae = error "no default dae in OcpPhase"
    , ocpBc = \_ _ -> None
    , ocpPathC = \_ _ _ _ _ _ -> None
    , ocpBcBnds = None
    , ocpPathCBnds = None
    , ocpXbnd = fill (Nothing, Nothing)
    , ocpZbnd = fill (Nothing, Nothing)
    , ocpUbnd = fill (Nothing, Nothing)
    , ocpPbnd = fill (Nothing, Nothing)
    , ocpTbnd = (Nothing, Nothing)
    , ocpObjScale      = Nothing
    , ocpTScale        = Nothing
    , ocpXScale        = Nothing
    , ocpZScale        = Nothing
    , ocpUScale        = Nothing
    , ocpPScale        = Nothing
    , ocpResidualScale = Nothing
    , ocpBcScale       = Nothing
    , ocpPathCScale    = Nothing
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
