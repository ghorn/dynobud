{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Dyno.SXElement
       ( SXElement(..)
       , sxSplitJV
       , sxCatJV
         -- todo: remove this completely after NlpMonad/OcpMonad are done with it
       , sxElementSym
         -- todo: remove the next two exports after NlpMonad/OcpMonad are done with it
       , sxElementToSX
       , sxToSXElement
       ) where

import Linear.Conjugate ( Conjugate(..) )

import Casadi.SX ( SX, ssym )
import qualified Casadi.CMatrix as CM
import Casadi.Overloading ( Fmod, ArcTan2, SymOrd )

import Dyno.View.Unsafe.View ( mkJ, unJ )

import Dyno.View.JV ( JV, splitJV', catJV' )
import Dyno.View.View ( J )
import Dyno.Vectorize ( Vectorize, Id )

newtype SXElement =
  SXElement SX
  deriving ( Num, Fractional, Floating
           , Fmod, ArcTan2, SymOrd
           , Show, Eq, Conjugate
           )

-- todo: take this out after NlpMonad/OcpMonad are done with it
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


sxSplitJV :: Vectorize f => J (JV f) SX -> f SXElement
sxSplitJV v = fmap f (splitJV' v)
  where
    f :: J (JV Id) SX -> SXElement
    f = sxToSXElement . unJ

sxCatJV :: Vectorize f => f SXElement -> J (JV f) SX
sxCatJV v = catJV' (fmap f v)
  where
    f :: SXElement -> J (JV Id) SX
    f x = mkJ (sxElementToSX x)
