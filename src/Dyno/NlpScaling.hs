{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Dyno.NlpScaling
       ( ScaleFuns(..)
       , scaledFG
       , mkScaleFuns
       ) where

import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V

import Dyno.Vectorize ( Id )
import Dyno.View.View
import Dyno.View.JV
import Dyno.View.Viewable ( Viewable )
import Dyno.View.CasadiMat ( CasadiMat(..), fromDVector )

data ScaleFuns x g a =
  ScaleFuns
  { fToFBar :: J (JV Id) a -> J (JV Id) a
  , fbarToF :: J (JV Id) a -> J (JV Id) a
  , xToXBar :: J x a -> J x a
  , xbarToX :: J x a -> J x a
  , gToGBar :: J g a -> J g a
  , gbarToG :: J g a -> J g a
  , lamXToLamXBar :: J x a -> J x a
  , lamXBarToLamX :: J x a -> J x a
  , lamGToLamGBar :: J g a -> J g a
  , lamGBarToLamG :: J g a -> J g a
  }

scaledFG ::
  forall x p g a .
  (View x, View g, CasadiMat a, Viewable a)
  => ScaleFuns x g a
  -> (J x a -> J p a -> (J (JV Id) a, J g a))
  -> J x a
  -> J p a
  -> (J (JV Id) a, J g a)
scaledFG scaleFuns fg x p = (fToFBar scaleFuns f, gToGBar scaleFuns g)
  where
    (f, g) = fg (xbarToX scaleFuns x) p

allPositive :: Maybe (V.Vector Double) -> Bool
allPositive = all (> 0) . fromMaybe [] . fmap V.toList

mkScaleFuns ::
  forall x g a .
  (View x, View g, CasadiMat a, Viewable a)
  => Maybe (J x (V.Vector Double))
  -> Maybe (J g (V.Vector Double))
  -> Maybe Double
  -> ScaleFuns x g a
mkScaleFuns mx mg mf
  | any (not . allPositive)
    [ fmap unJ mx
    , fmap unJ mg
    , fmap V.singleton mf
    ] = error "all scaling factors must be positive"
  | otherwise =
    ScaleFuns { fToFBar = divByFScale
              , fbarToF = mulByFScale
              , xToXBar = divByXScale
              , xbarToX = mulByXScale
              , gToGBar = divByGScale
              , gbarToG = mulByGScale
              , lamXToLamXBar = lamXToLamXBar'
              , lamXBarToLamX = lamXBarToLamX'
              , lamGToLamGBar = lamGToLamGBar'
              , lamGBarToLamG = lamGBarToLamG'
              }
  where
    (lamXToLamXBar', lamXBarToLamX') = case mf of
      Nothing -> (mulByXScale, divByXScale)
      Just fscl -> ( \lamx -> mkJ ((unJ (mulByXScale lamx)) / fs)
                   , \lamx -> mkJ ((unJ (divByXScale lamx)) * fs)
                   )
        where
          fs :: a
          fs = fromDVector (V.singleton fscl)
    
    (lamGToLamGBar', lamGBarToLamG') = case mf of
      Nothing -> (mulByGScale, divByGScale)
      Just fscl -> ( \lamg -> mkJ ((unJ (mulByGScale lamg)) / fs)
                   , \lamg -> mkJ ((unJ (divByGScale lamg)) * fs)
                   )
        where
          fs :: a
          fs = fromDVector (V.singleton fscl)
    
    mulByXScale :: J x a -> J x a
    divByXScale :: J x a -> J x a
    (mulByXScale, divByXScale) = case mx of
      Nothing -> (id, id)
      Just xscl -> ( \(UnsafeJ x') -> mkJ (x' * s)
                   , \(UnsafeJ x') -> mkJ (x' / s)
                   )
        where
          s :: a
          s = fromDVector (unJ xscl)

    mulByGScale :: J g a -> J g a
    divByGScale :: J g a -> J g a
    (mulByGScale, divByGScale) = case mg of
      Nothing -> (id, id)
      Just gscl -> ( \(UnsafeJ g') -> mkJ (g' * s)
                   , \(UnsafeJ g') -> mkJ (g' / s)
                   )
        where
          s :: a
          s = fromDVector (unJ gscl)

    mulByFScale :: J (JV Id) a -> J (JV Id) a
    divByFScale :: J (JV Id) a -> J (JV Id) a
    (mulByFScale, divByFScale) = case mf of
      Nothing -> (id, id)
      Just fscl -> ( \(UnsafeJ f') -> mkJ (f' * s)
                   , \(UnsafeJ f') -> mkJ (f' / s)
                   )
        where
          s :: a
          s = fromDVector (V.singleton fscl)
