{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

-- -- for Show:
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Dyno.TestMX where

import GHC.Generics ( Generic )
import Data.Proxy
import qualified Data.Vector as V
import Dyno.Casadi.MXV
import Dyno.Casadi.MXVData

data Id m = Id (m S) deriving ( Generic  )
data Xy m = Xy (m S) (m S) deriving ( Generic )
data Fctr f m = Fctr (m f) deriving ( Generic, Show )
data Tuple f g m = Tuple (m f) (m g) deriving ( Generic, Show )

instance View Id
instance View Xy
instance View f => View (Fctr f)
instance (View f, View g) => View (Tuple f g)







--deriving instance Show (m S) => Show (Id m)
--deriving instance Show (m S) => Show (Xy m)

proxy :: a -> Proxy a
proxy = const Proxy

vertsplit' :: MX -> [Int] -> [MX]
vertsplit' x ks = V.toList (vertsplit x (V.fromList ks))

--anTuple :: Tuple (Fctr Xy) (Fctr Id) MXV
--anTuple = Tuple x y
--  where
--    x :: MXV (Fctr Xy)
--    x = MXV $ ssymV' "x" 2
--    y :: MXV (Fctr Id)
--    y = MXV $ ssymV' "y" 1
--
--anFunctor :: Fctr Xy MXV
--anFunctor = Fctr x
--  where
--    x :: MXV Xy
--    x = MXV $ veccat (V.fromList [0,1])

--go2 :: MXV (Fctr Xy)
--go2 = cat anFunctor
--
--go :: MXV (Tuple (Fctr Xy) (Fctr Id))
--go = cat anTuple
--
--og2 :: (Fctr Xy) MXV
--og2 = split $ MXV (ssymV' "x" 2)
--
--og :: Tuple (Fctr Xy) (Fctr Id) MXV
--og = split $ MXV (ssymV' "x" 3)

go :: IO ()
go = do
  woo <- msym "x" :: IO (MXV (Tuple (Fctr Xy) (Fctr Id)))
  print woo
  let oow = split woo
  print oow
  let wootoo = cat oow
  print wootoo
  return ()
