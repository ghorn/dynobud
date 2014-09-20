-- | Minimize the Rosenbrock function (plus a trivial constraint) using
-- the more complicated NLP' interface.

{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dyno.View
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Solvers


data X a = X (J S a) (J S a) deriving (Generic, Show)
data G a = G (J S a) deriving (Generic, Show)

instance View X
instance View G

myNlp :: Nlp' X JNone G MX
myNlp = Nlp' { nlpFG' = fg
             , nlpBX' = bx
             , nlpBG' = bg
             , nlpX0' = x0
             , nlpP' = cat JNone
             , nlpLamX0' = Nothing
             , nlpLamG0' = Nothing
             , nlpScaleF' = Nothing
             , nlpScaleX' = Nothing
             , nlpScaleG' = Nothing
             }
  where
    x0 :: J X (V.Vector Double)
    x0 = cat $ X (-8) (-8)

    bx :: J X (Vector Bounds)
    bx = mkJ $
         V.fromList [ (Just (-21), Just 0.5)
                    , (Just (-2), Just 2)
                    ]
    bg :: J G (Vector Bounds)
    bg = mkJ $ (V.singleton (Just (-10), Just 10))

    fg :: J X MX -> J JNone MX -> (J S MX, J G MX)
    fg xy _ = (f, cat g)
      where
        f = (1-x)**2 + 100*(y - x**2)**2
        g = G x

        X x y = split xy

main :: IO ()
main = do
  opt <- solveNlp' ipoptSolver myNlp Nothing
  print opt
