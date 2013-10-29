{-# OPTIONS_GHC -Wall #-}

module Ocp ( OcpPhase(..) ) where

import Dae

data OcpPhase x z u p r bc pc a =
  OcpPhase { --ocpMeyer :: x a -> a -> x a -> a -> a
             ocpMeyer :: x a -> a
--           , ocpLagrange :: x a -> z a -> u a -> p a -> a -> a
           , ocpLagrange :: x a -> z a -> u a -> p a -> a
           , ocpDae :: Dae x z u p r a
--           , ocpBc :: x a -> a -> x a -> a -> bc a
           , ocpBc :: x a -> x a -> bc a
--           , ocpPathC :: x a -> z a -> u a -> p a -> a -> pc a
           , ocpPathC :: x a -> z a -> u a -> p a -> pc a
           , ocpPathCBnds :: pc (Maybe Double, Maybe Double)
           , ocpXbnd :: x (Maybe Double, Maybe Double)
           , ocpZbnd :: z (Maybe Double, Maybe Double)
           , ocpUbnd :: u (Maybe Double, Maybe Double)
           , ocpPbnd :: p (Maybe Double, Maybe Double)
           , ocpTbnd :: (a,a)
           }
