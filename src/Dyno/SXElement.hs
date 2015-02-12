{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Dyno.SXElement
       ( SXElement(..)
       , sxElementSym
       , sxToSXElement
       , sxElementToSX
       ) where

import Linear.Conjugate ( Conjugate(..) )

import Casadi.SX
import qualified Casadi.CMatrix as CM
import Casadi.Overloading

newtype SXElement =
  SXElement SX
  deriving ( Num, Fractional, Floating
           , Fmod, ArcTan2, SymOrd
           , Show, Eq, Conjugate
           )

sxElementSym :: String -> IO SXElement
sxElementSym = fmap SXElement . ssym

sxToSXElement :: SX -> SXElement
sxToSXElement x
  | (1,1) == sizes = SXElement x
  | otherwise = error $ "sxToSXElement: got non-scalar of size " ++ show sizes
  where
    sizes = (CM.size1 x, CM.size2 x)

sxElementToSX :: SXElement -> SX
sxElementToSX (SXElement x)
  | (1,1) == sizes = x
  | otherwise = error $ "sxElementToSX: got non-scalar of size " ++ show sizes
  where
    sizes = (CM.size1 x, CM.size2 x)
