{-# OPTIONS_GHC -Wall #-}

module Hascm.Ocp ( OcpPhase(..), Dae ) where

type Dae x z u p r a = x a -> x a -> z a -> u a -> p a -> a -> r a

data OcpPhase x z u p r bc pc a =
  OcpPhase { --ocpMeyer :: x a -> a -> x a -> a -> a
             ocpMeyer :: x a -> a -> a
           , ocpLagrange :: x a -> z a -> u a -> p a -> a -> a
           , ocpDae :: Dae x z u p r a
--           , ocpBc :: x a -> a -> x a -> a -> bc a
           , ocpBc :: x a -> x a -> bc a
           , ocpPathC :: x a -> z a -> u a -> p a -> a -> pc a
           , ocpPathCBnds :: pc (Maybe Double, Maybe Double)
           , ocpXbnd :: x (Maybe Double, Maybe Double)
           , ocpZbnd :: z (Maybe Double, Maybe Double)
           , ocpUbnd :: u (Maybe Double, Maybe Double)
           , ocpPbnd :: p (Maybe Double, Maybe Double)
           , ocpTbnd :: (Maybe Double, Maybe Double)
           }
