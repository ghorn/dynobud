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

import GHC.Generics
import Data.Proxy
import qualified Data.Vector as V
import Dyno.Casadi.MXVClasses
import Dyno.Casadi.MXVData
import Dyno.Casadi.SX ( ssymV' )
--import Dyno.Casadi.MX

data Id m = Id (m Sc) deriving ( Generic  )
data Xy m = Xy (m Sc) (m Sc) deriving ( Generic )
data Fctr f m = Fctr (m f) deriving ( Generic, Show )
data Tuple f g m = Tuple (m f) (m g) deriving ( Generic, Show )
--deriving instance Show (m Sc) => Show (Id m)
--deriving instance Show (m Sc) => Show (Xy m)

proxy :: a -> Proxy a
proxy = const Proxy

instance Cat (Id MXV) where
instance Cat (Xy MXV) where
instance Cat (Fctr f MXV) where
instance Cat (Tuple f g MXV) where

instance Split (Id MXV) where
instance Split (Xy MXV) where
instance Split (Fctr f MXV) where
instance (Size (f MXV), Size (g MXV)) => Split (Tuple f g MXV) where

instance Size (Id MXV) where
instance Size (Xy MXV) where
instance Size (f MXV) => Size (Fctr f MXV) where
instance (Size (f MXV), Size (g MXV)) => Size (Tuple f g MXV) where

vertsplit' :: MX -> [Int] -> [MX]
vertsplit' x ks = V.toList (vertsplit x (V.fromList ks))

anTuple :: Tuple (Fctr Xy) (Fctr Id) MXV
anTuple = Tuple x y
  where
    x :: MXV (Fctr Xy)
    x = MXV $ ssymV' "x" 2
    y :: MXV (Fctr Id)
    y = MXV $ ssymV' "y" 1

anFunctor :: Fctr Xy MXV
anFunctor = Fctr x
  where
    x :: MXV Xy
    x = MXV $ veccat (V.fromList [0,1])

go2 :: MX
go2 = cat anFunctor

go :: MX
go = cat anTuple

og2 :: (Fctr Xy) MXV
og2 = split (ssymV' "x" 2)

og :: Tuple (Fctr Xy) (Fctr Id) MXV
og = split (ssymV' "x" 3)
