{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language DeriveFunctor #-}

module Spring where

import qualified Data.Vector as V
import Data.TypeLevel.Num.Reps

import TypeVecs ( Vec(..) )

import Vectorize
import Dae

data Xyz a = Xyz a a a deriving (Functor)

instance Vectorize Xyz D3 where
  vectorize (Xyz x y z) = Vec $ V.fromList [x,y,z]
  devectorize (Vec v) = case V.toList v of
    [x,y,z] -> Xyz x y z
    _ -> error "Vectorize Xyz: unvectorize error"
  empty = Xyz () () ()

data Spring a = Spring { pos :: Xyz a
                       , vel :: Xyz a
                       }
data SpringForce a = SpringForce a

ddtSpring :: Fractional a => Spring a -> SpringForce a -> Spring a
ddtSpring (Spring (Xyz x y z) (Xyz x' y' z')) (SpringForce u) =
  Spring (Xyz x' y' z') (Xyz x'' y'' z'')
  where
    x'' = -k*x - b*x'
    y'' = -k*y - b*y'
    z'' = -k*z - b*z' + u

    k = 2.6
    b = 0.2

data None a = None

springOde :: Fractional a => ExplicitOde Spring SpringForce a
springOde = ddtSpring
