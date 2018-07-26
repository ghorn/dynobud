{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.NlpScaling
       ( ScaleFuns(..)
       , scaledFG
       , mkScaleFuns
       ) where

import Data.Maybe ( fromMaybe )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Casadi.Matrix ( CMatrix, fromDVector )

import Dyno.View.Unsafe ( mkM, unM )
import Dyno.View.M ( M )
import qualified Dyno.View.M as M
import Dyno.Vectorize ( Id(..) )
import Dyno.View.View ( View, J, S, v2d )

data ScaleFuns x g a =
  ScaleFuns
  { fToFBar :: S (Vector Double) -> S (Vector Double)
  , fbarToF :: S (Vector Double) -> S (Vector Double)
  , xToXBar :: J x (Vector Double) -> J x (Vector Double)
  , xbarToX :: J x (Vector Double) -> J x (Vector Double)
  , gToGBar :: J g (Vector Double) -> J g (Vector Double)
  , gbarToG :: J g (Vector Double) -> J g (Vector Double)
  , lamXToLamXBar :: J x (Vector Double) -> J x (Vector Double)
  , lamXBarToLamX :: J x (Vector Double) -> J x (Vector Double)
  , lamGToLamGBar :: J g (Vector Double) -> J g (Vector Double)
  , lamGBarToLamG :: J g (Vector Double) -> J g (Vector Double)
  , gradFBarToGradF :: J x (Vector Double) -> J x (Vector Double)

  , fToFBar' :: S a -> S a
  , gToGBar' :: J g a -> J g a
  , xbarToX' :: J x a -> J x a
  , jacGBarToJacG :: M g x a -> M g x a
  , hessFBarToHessF :: M x x a -> M x x a
  , hessLamGBarToHessLamG :: M x x a -> M x x a
  , hessLagBarToHessLag :: M x x a -> M x x a
  }

scaledFG ::
  forall x p g a
  . ScaleFuns x g a
  -> (J x a -> J p a -> (S a, J g a))
  -> J x a
  -> J p a
  -> (S a, J g a)
scaledFG scaleFuns fg x p = (fToFBar' scaleFuns f, gToGBar' scaleFuns g)
  where
    (f, g) = fg (xbarToX' scaleFuns x) p

allPositive :: Maybe (Vector Double) -> Bool
allPositive = all (> 0) . fromMaybe [] . fmap V.toList

mkScaleFuns ::
  forall x g a .
  (View x, View g, CMatrix a)
  => Maybe (J x (Vector Double))
  -> Maybe (J g (Vector Double))
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
              , fToFBar' = divByFScale'
              , fbarToF = mulByFScale
              , xToXBar = divByXScale
              , xbarToX = mulByXScale
              , xbarToX' = mulByXScale'
              , gToGBar = divByGScale
              , gToGBar' = divByGScale'
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

    gradFBarToGradF' :: J x (Vector Double) -> J x (Vector Double)
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
      Just fscl -> ( mkM . fmap (/ fscl) . unM . mulByXScale
                   , mkM . fmap (* fscl) . unM . divByXScale
                   )

    (lamGToLamGBar', lamGBarToLamG') = case mf of
      Nothing -> (mulByGScale, divByGScale)
      Just fscl -> ( mkM . fmap (/ fscl) . unM . mulByGScale
                   , mkM . fmap (* fscl) . unM . divByGScale
                   )

    mulByXScale :: J x (Vector Double) -> J x (Vector Double)
    divByXScale :: J x (Vector Double) -> J x (Vector Double)
    (mulByXScale, divByXScale) = case mx of
      Nothing -> (id, id)
      Just xscl -> ( mkM . V.zipWith (*) s . unM
                   , mkM . V.zipWith (flip (/)) s . unM
                   )
        where
          s :: Vector Double
          s = unM xscl

    mulByXScale' :: J x a -> J x a
    mulByXScale' = case mx of
      Nothing -> id
      Just xscl -> mkM . (* s) . unM
        where
          s :: a
          s = fromDVector (unM xscl)

    mulByGScale :: J g (Vector Double) -> J g (Vector Double)
    divByGScale :: J g (Vector Double) -> J g (Vector Double)
    (mulByGScale, divByGScale) = case mg of
      Nothing -> (id, id)
      Just gscl -> ( mkM . V.zipWith (*) s . unM
                   , mkM . V.zipWith (flip (/)) s . unM
                   )
        where
          s :: Vector Double
          s = unM gscl

    divByGScale' :: J g a -> J g a
    divByGScale' = case mg of
      Nothing -> id
      Just gscl -> mkM . (/ s) . unM
        where
          s :: a
          s = fromDVector (unM gscl)

    mulByFScale :: S (Vector Double) -> S (Vector Double)
    divByFScale :: S (Vector Double) -> S (Vector Double)
    (mulByFScale, divByFScale) = case mf of
      Nothing -> (id, id)
      Just fscl -> ( mkM . V.zipWith (*) s . unM
                   , mkM . V.zipWith (flip (/)) s . unM
                   )
        where
          s :: Vector Double
          s = V.singleton fscl

    divByFScale' :: S a -> S a
    divByFScale' = case mf of
      Nothing -> id
      Just fscl -> mkM . (/ realToFrac fscl) . unM
