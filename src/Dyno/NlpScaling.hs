{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.NlpScaling
       ( ScaleFuns(..)
       , scaledFG
       , mkScaleFuns
       ) where

import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V

import Casadi.CMatrix ( CMatrix, fromDVector )

import Dyno.View.Unsafe ( mkM, unM )
import Dyno.View.M ( M )
import qualified Dyno.View.M as M
import Dyno.View.Vectorize ( Id(..) )
import Dyno.View.View ( View, J, S, v2d )

data ScaleFuns x g a =
  ScaleFuns
  { fToFBar :: S a -> S a
  , fbarToF :: S a -> S a
  , xToXBar :: J x a -> J x a
  , xbarToX :: J x a -> J x a
  , gToGBar :: J g a -> J g a
  , gbarToG :: J g a -> J g a
  , lamXToLamXBar :: J x a -> J x a
  , lamXBarToLamX :: J x a -> J x a
  , lamGToLamGBar :: J g a -> J g a
  , lamGBarToLamG :: J g a -> J g a
  , gradFBarToGradF :: J x a -> J x a
  , jacGBarToJacG :: M g x a -> M g x a
  , hessFBarToHessF :: M x x a -> M x x a
  , hessLamGBarToHessLamG :: M x x a -> M x x a
  , hessLagBarToHessLag :: M x x a -> M x x a
  }

scaledFG ::
  forall x p g a .
  ScaleFuns x g a
  -> (J x a -> J p a -> (S a, J g a))
  -> J x a
  -> J p a
  -> (S a, J g a)
scaledFG scaleFuns fg x p = (fToFBar scaleFuns f, gToGBar scaleFuns g)
  where
    (f, g) = fg (xbarToX scaleFuns x) p

allPositive :: Maybe (V.Vector Double) -> Bool
allPositive = all (> 0) . fromMaybe [] . fmap V.toList

-- todo:
-- Could make this return casadi Functions for better performance.
-- Doesn't seem to be a bottleneck
mkScaleFuns ::
  forall x g a .
  (View x, View g, CMatrix a)
  => Maybe (J x (V.Vector Double))
  -> Maybe (J g (V.Vector Double))
  -> Maybe Double
  -> ScaleFuns x g a
mkScaleFuns mx mg mf
  | any (not . allPositive)
    [ fmap unM mx
    , fmap unM mg
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
              , gradFBarToGradF = gradFBarToGradF'
              , jacGBarToJacG = jacGBarToJacG'
              , hessFBarToHessF = hessFBarToHessF'
              , hessLamGBarToHessLamG = hessFBarToHessF' -- only valid at the solution
              , hessLagBarToHessLag = hessFBarToHessF' -- only valid at the solution
              }
  where
    xdiaginv :: Maybe (M x x a)
    xdiaginv = fmap (\scl -> M.diag (M.fromDM (1.0 / (v2d scl)))) mx

    gdiag :: Maybe (M g g a)
    gdiag = fmap (\scl -> M.diag (M.fromDM (v2d scl))) mg

    jacGBarToJacG' :: M g x a -> M g x a
    jacGBarToJacG' g0 = gg0x
      where
        gg0x = case gdiag of
          Nothing -> g0x
          Just gd -> gd `M.mm` g0x
        g0x = case xdiaginv of
          Nothing -> g0
          Just xdi -> g0 `M.mm` xdi

    gradFBarToGradF' :: J x a -> J x a
    gradFBarToGradF' = lamXBarToLamX'

    hessFBarToHessF' :: M x x a -> M x x a
    hessFBarToHessF' h0 = case mf of
      Nothing -> h1
      Just fscl -> h1 `M.ms` (M.vcat (Id (realToFrac fscl)))
      where
        h1 = case xdiaginv of
          Nothing -> h0
          Just xdi -> xdi `M.mm` h0 `M.mm` xdi

    (lamXToLamXBar', lamXBarToLamX') = case mf of
      Nothing -> (mulByXScale, divByXScale)
      Just fscl -> ( \lamx -> mkM ((unM (mulByXScale lamx)) / fs)
                   , \lamx -> mkM ((unM (divByXScale lamx)) * fs)
                   )
        where
          fs :: a
          fs = fromDVector (V.singleton fscl)
    
    (lamGToLamGBar', lamGBarToLamG') = case mf of
      Nothing -> (mulByGScale, divByGScale)
      Just fscl -> ( \lamg -> mkM ((unM (mulByGScale lamg)) / fs)
                   , \lamg -> mkM ((unM (divByGScale lamg)) * fs)
                   )
        where
          fs :: a
          fs = fromDVector (V.singleton fscl)
    
    mulByXScale :: J x a -> J x a
    divByXScale :: J x a -> J x a
    (mulByXScale, divByXScale) = case mx of
      Nothing -> (id, id)
      Just xscl -> ( mkM . (* s) . unM
                   , mkM . (/ s) . unM
                   )
        where
          s :: a
          s = fromDVector (unM xscl)

    mulByGScale :: J g a -> J g a
    divByGScale :: J g a -> J g a
    (mulByGScale, divByGScale) = case mg of
      Nothing -> (id, id)
      Just gscl -> ( mkM . (* s) . unM
                   , mkM . (/ s) . unM
                   )
        where
          s :: a
          s = fromDVector (unM gscl)

    mulByFScale :: S a -> S a
    divByFScale :: S a -> S a
    (mulByFScale, divByFScale) = case mf of
      Nothing -> (id, id)
      Just fscl -> ( mkM . (* s) . unM
                   , mkM . (/ s) . unM
                   )
        where
          s :: a
          s = fromDVector (V.singleton fscl)
