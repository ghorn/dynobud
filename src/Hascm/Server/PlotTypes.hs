{-# OPTIONS_GHC -Wall #-}
--{-# Language ExistentialQuantification #-}
--{-# Language GADTs #-}

module Hascm.Server.PlotTypes
       ( Channel(..)
       , PlotReal
--       , PbPrim(..)
--       , PbTree(..)
--       , PbTree'(..)
--       , XAxisType(..)
--       , toGetterTree
--       , pbTreeToTree
                 ) where

import Control.Concurrent ( MVar, ThreadId )
--import Data.Sequence ( Seq )
import Data.Time ( NominalDiffTime )
import Data.Tree ( Tree(..) )

--import GAccessors

type PlotReal = Double

--toGetterTree :: AccessorTree a -> Tree (String, String, Maybe (a -> [(PlotReal,PlotReal)]))
--toGetterTree = toGetterTree' "" ""
--
--toGetterTree' :: String -> String -> AccessorTree a ->
--                 Tree (String, String, Maybe (a -> [(PlotReal,PlotReal)]))
--toGetterTree' msg name (Getter f) = Node (name, msg,Just f) []
--toGetterTree' msg name (Data (_,name') children) =
--  Node (msg, name, Nothing) $ map (\(n,t) -> toGetterTree' (msg ++ name) n t) children

--data XAxisType a = XAxisTime
--                 | XAxisCounter
--                 | XAxisStaticCounter
--                 | XAxisFun (String, a -> PlotReal)

data Channel a =
  Channel { chanName :: String
          , chanTraj :: MVar (a, Int, NominalDiffTime)
          , chanServerThreadId :: ThreadId
          , chanGetters :: Tree (String, String, Maybe (a -> [[(PlotReal,PlotReal)]]))
          }
