{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}

module Hascm.MultipleShooting
       ( MsTraj(..)
       , MsPoint(..)
       , MsConstraints(..)
       , makeNlp
       ) where

import TypeVecs ( Vec, (|>), tvzipWith, tvhead, tvtail, tvlength )
import Vectorize
import Nlp
import TypeNats
import Ocp
import Dae

-- multiple shooting
data MsPoint x u a = MsPoint (x a) (u a) deriving (Functor, Generic1)
instance (Vectorize x, Vectorize u) => Vectorize (MsPoint x u)
data MsTraj x u p n a = MsTraj (p a) (Vec n (MsPoint x u a)) (x a) a deriving (Functor, Generic1)
instance (Vectorize x, Vectorize u, Vectorize p, NaturalT n) => Vectorize (MsTraj x u p n)

data MsConstraints x n bc pc a =
  MsConstraints
  { mscBc :: bc a
  , mscPathc :: Vec n (pc a)
  , mscDynamics :: Vec n (x a)
  } deriving (Functor, Generic1, Show)

instance (Vectorize x, Vectorize pc, Vectorize bc, NaturalT nu) =>
         Vectorize (MsConstraints x nu bc pc)

getDvBnds :: forall x u p bc pc n . NaturalT n =>
  OcpPhase x None u p x bc pc Double -> -- Double here supresses warning in makeNlp, Double is never used
  MsTraj x u p n (Maybe Double, Maybe Double)
getDvBnds ocp = MsTraj p xus x (Just lbt, Just ubt)
  where
    xus = fill (MsPoint (ocpXbnd ocp) (ocpUbnd ocp))
    x = ocpXbnd ocp
    p = ocpPbnd ocp
    (lbt, ubt) = ocpTbnd ocp

getGBnds :: (Vectorize x, Vectorize bc, NaturalT n) =>
            OcpPhase x None u p x bc pc Double ->  -- Double here supresses warning in makeNlp, Double is never used
            MsConstraints x n bc pc (Maybe Double, Maybe Double)
getGBnds ocp =
  MsConstraints
  { mscBc = fill (Just 0, Just 0)
  , mscPathc = fill (ocpPathCBnds ocp)
  , mscDynamics = fill (fill (Just 0, Just 0))
  }

getFg :: (Floating a, Vectorize x, NaturalT n, PositiveT n, (n ~ Succ (Pred n))) =>
         OcpPhase x None u p x bc pc a ->
         Integrator x None u p x a ->
         MsTraj x u p n a ->
         NlpFun (MsConstraints x n bc pc) a
getFg ocp integrator (MsTraj p xus xf tf) = NlpFun objective constraints
  where
    xs = fmap (\(MsPoint x _) -> x) xus
    us = fmap (\(MsPoint _ u) -> u) xus
    n = tvlength us
    ts = tf / (fromIntegral n)
    constraints =
      MsConstraints
      { mscBc = (ocpBc ocp) (tvhead xs) xf
      , mscPathc = tvzipWith (\x u -> ocpPathC ocp x None u p) xs us
      , mscDynamics = vzipWith3 integrator' xs us x1s
      }
      where
        integrator' x0 u0 x1 = vzipWith (-) x1' x1
          where
            x1' = integrator (ocpDae ocp) ts x0 u0 p
        x1s = tvtail xs |> xf

    objective = (ocpMayer ocp) xf
--    objectiveL = V.foldl' f 0 (vectorize xus)
--      where
--        f acc (MsPoint x u) = integrator dae' ts x u p + acc
--          where
--            dae' = Dae (\x None u p' -> ocpLagrange ocp x None u p')
--            ddtL l = ocpLagrange ocp x None u p
--        (ssum (tvzipWith (\x u -> ocpLagrange ocp x None u p) xs us)) / fromIntegral (tvlength us)
--ssum :: Num a => Vec n a -> a
--ssum = F.foldl' (+) 0

makeNlp :: (Vectorize x, NaturalT n, PositiveT n, n ~ (Succ (Pred n)), Vectorize bc) =>
           (forall a. Floating a => OcpPhase x None u p x bc pc a) ->
           (forall a. Floating a => Integrator x None u p x a) ->
           Nlp (MsTraj x u p n) (MsConstraints x n bc pc)
makeNlp ocp integrator = Nlp (getFg ocp integrator) (getDvBnds ocp) (getGBnds ocp)
