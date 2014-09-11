-- | Minimize the Rosenbrock function (plus a trivial constraint) using
-- the more complicated NLP' interface.

{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Text.Printf ( printf )

import Dyno.View
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Solvers


data P a = P (J S a) (J S a) deriving (Generic, Show)
data X a = X (J S a) (J S a) deriving (Generic, Show)
data G a = G (J S a)-- (J S a)
         deriving (Generic, Show)

instance View X
instance View G
instance View P

myNlp :: Nlp' X P G MX
myNlp = Nlp' { nlpFG' = fg
             , nlpBX' = bx
             , nlpBG' = bg
             , nlpX0' = x0
             , nlpP' = cat $ P (-2) 0
             }
  where
    x0 :: J X (V.Vector Double)
    x0 = cat $ X (-8) (-8)

    bx :: J X (Vector Bounds)
    bx = mkJ $
         V.fromList [ (Just (-3), Just 3)
                    , (Just (-3), Just 3)
                    ]
    bg :: J G (Vector Bounds)
    bg = mkJ $ (V.singleton (Nothing, Just 0))

    fg :: J X MX -> J P MX -> (J S MX, J G MX)
    fg xy pxy = (f, cat g)
      where
        X  x  y = split  xy
        P px py = split pxy
        f = (1-x)**2 + 100*(y - x**2)**2
--        g = G x
--        f = (x - px)**2 + (y - py)**2

        g = G (x - px)

--solver = ipoptSolver {options = [ --("max_iter", Opt (5 :: Int))
--                                  ("print_level", Opt (0 :: Int))
--                                , ("print_time", Opt False)
--                                ]}
solver = snoptSolver {options = [ ("print_time", Opt False)
                                , ("_isumm", Opt (0 :: Int))
--                                , ("max_iter", Opt (5 :: Int))
--                                , ("_start", Opt "Warm")
                                ]}
main :: IO ()
main = do
  let cbp :: J X (Vector Double) -> J P (Vector Double) -> Double -> IO ()
      cbp xy pxy alpha = do
        let X x y = split xy
            P px py = split pxy
        --printf "X: (%.3f,%.3f), P: (%.3f, %.3f), a: %.4f\n"
        -- (V.head (unJ x)) (V.head (unJ y)) (V.head (unJ px)) (V.head (unJ py)) alpha
        return ()
  opt <- solveNlpHomotopy' solver myNlp (cat (P (2) (0))) Nothing (Just cbp)
  print opt
