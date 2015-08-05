-- | Minimize the Rosenbrock function (plus a trivial constraint) using
-- the View-based NLP interface.
-- Unfortunately, at the moment there only types here are (JV ) compound types
-- so the use of Views aren't fully illustrated.
-- todo: comment up the multiple shooting code as an example

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic, Generic1 )

import Data.Vector ( Vector )
import qualified Data.Vector as V

import Casadi.MX ( MX )
import Dyno.View.View
import Dyno.View.JV ( JV, catJV, catJV', splitJV' )
import Dyno.Vectorize
import Dyno.Nlp
import Dyno.NlpUtils
import Dyno.Solvers


data X a = X a a deriving (Functor, Generic, Generic1, Show)
data G a = G a deriving (Functor, Generic, Generic1, Show)

instance Vectorize X
instance Vectorize G

myNlp :: Nlp (JV X) JNone (JV G) MX
myNlp = Nlp { nlpFG = fg
            , nlpBX = bx
            , nlpBG = bg
            , nlpX0 = x0
            , nlpP = cat JNone
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
    bx = catJV $
         X (Just (-21), Just 0.5)
           (Just (-2), Just 2)

    bg :: J (JV G) (Vector Bounds)
    bg = catJV $ G (Just (-10), Just 10)

    fg :: J (JV X) MX -> J JNone MX -> (J (JV Id) MX, J (JV G) MX)
    fg xy _ = (f, catJV' g)
      where
        f = (1-x)**2 + 100*(y - x**2)**2
        g = G x

        X x y = splitJV' xy

main :: IO ()
main = do
  opt <- solveNlp ipoptSolver myNlp Nothing
  print opt
