{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PolyKinds #-}

module Utils
       ( reproxy
       , reproxy2
       , reproxy3
       ) where

import Data.Proxy

reproxy :: Proxy f -> Proxy g -> Proxy (f g)
reproxy _ _ = Proxy

reproxy2 :: Proxy f -> Proxy g -> Proxy h -> Proxy (f g h)
reproxy2 _ _ _ = Proxy

reproxy3 :: Proxy f -> Proxy g -> Proxy h -> Proxy j -> Proxy (f g h j)
reproxy3 _ _ _ _ = Proxy

