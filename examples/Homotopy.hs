{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Text.Printf ( printf )

import Casadi.MX ( MX )

import Dyno.View
import Dyno.Vectorize ( Vectorize )
import Dyno.Nlp ( Nlp(..), Bounds )
import Dyno.NlpUtils ( HomotopyParams(..), solveNlpHomotopy )
import Dyno.Solvers

hp :: HomotopyParams
hp = HomotopyParams
   { reduction = 0.6
   , increase = 2
   , iterIncrease = 10
   , iterDecrease = 20
   }

data P a = P a a deriving (Functor, Generic, Generic1, Show)
data X a = X a a deriving (Functor, Generic, Generic1, Show)
data G a = G a -- (S a)
         deriving (Functor, Generic, Generic1, Show)

instance Vectorize X
instance Vectorize G
instance Vectorize P

myNlp :: Nlp (JV X) (JV P) (JV G) MX
myNlp = Nlp { nlpFG = fg
            , nlpBX = bx
            , nlpBG = bg
            , nlpX0 = x0
            , nlpP = catJV $ P (-2) 0
            , nlpLamX0 = Nothing
            , nlpLamG0 = Nothing
            , nlpScaleF = Nothing
            , nlpScaleX = Nothing
            , nlpScaleG = Nothing
            }
  where
    x0 :: J (JV X) (V.Vector Double)
    x0 = catJV $ X (-8) (-8)

    bx :: J (JV X) (Vector Bounds)
    bx = catJV $ X (Just (-3), Just 3) (Just (-3), Just 3)
    bg :: J (JV G) (Vector Bounds)
    bg = catJV (G (Nothing, Just 0))

    fg :: J (JV X) MX -> J (JV P) MX -> (S MX, J (JV G) MX)
    fg xy pxy = (f, vcat g)
      where
        X  x  y = vsplit  xy
        P px  _ = vsplit pxy
        f = (1-x)**2 + 100*(y - x**2)**2
--        g = G x
--        f = (x - px)**2 + (y - py)**2

        g = G (x - px)

solver :: Solver
solver = ipoptSolver {options = [ --("max_iter", Opt (5 :: Int))
                                  ("print_level", Opt (0 :: Int))
                                , ("print_time", Opt False)
                                ]}
--solver = snoptSolver {options = [ ("print_time", Opt False)
----                                , ("_isumm", Opt (0 :: Int))
----                                , ("max_iter", Opt (5 :: Int))
----                                , ("_start", Opt "Warm")
--                                ]}
main :: IO ()
main = do
  let cbp :: J (JV X) (Vector Double) -> J (JV P) (Vector Double) -> Double -> IO ()
      cbp xy pxy alpha = do
        let X x y = splitJV xy
            P px py = splitJV pxy
        printf "X: (%.3f,%.3f), P: (%.3f, %.3f), a: %.4f\n" x y px py alpha
        return ()
      pfs = [catJV (P 2 0), catJV (P 3 0)]
  opt <- solveNlpHomotopy 1e-3 hp solver myNlp pfs Nothing (Just cbp)
  print opt
