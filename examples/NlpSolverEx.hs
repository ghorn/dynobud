-- | Example of NlpSolver

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic1 )

import Text.Printf ( printf )

import Casadi.MX ( MX )

import Dyno.View.Vectorize ( Vectorize, Id(..), None(..), vpure, vapply )
import Dyno.View.View
import Dyno.View.M ( vcat, vsplit )
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.NlpUtils
import Dyno.Solvers

data X a = X a a deriving (Functor, Generic1, Show)
data G a = G a deriving (Functor, Generic1, Show)

instance Applicative X where {pure = vpure; (<*>) = vapply}
instance Applicative G where {pure = vpure; (<*>) = vapply}

instance Vectorize X
instance Vectorize G

myNlp :: Nlp (JV X) (JV None) (JV G) MX
myNlp = Nlp { nlpFG = fg
            , nlpIn =
              NlpIn
              { nlpBX = catJV bx
              , nlpBG = catJV bg
              , nlpX0 = catJV x0
              , nlpP = catJV None
              , nlpLamX0 = Nothing
              , nlpLamG0 = Nothing
              }
            , nlpScaleF = Just 9.86
            , nlpScaleX = Just $ catJV $ (X (4.7e-3) (4.7e4))
            , nlpScaleG = Just $ catJV $ (G 4.7)
--            , nlpScaleF = Just 1
--            , nlpScaleX = Just $ catJV (X 1 1)
--            , nlpScaleG = Just $ catJV (G 1) -- 1)
            }
  where
    x0 :: X Double
    x0 = X 0 0

    bx :: X Bounds
    bx = pure (Nothing, Nothing)

    bg :: G Bounds
    bg = G (Just 2, Nothing)

    fg :: J (JV X) MX -> J (JV None) MX -> (S MX, J (JV G) MX)
    fg xy _ = (f, vcat g)
      where
        X x y = vsplit xy
        x' = 1e3*x
        y' = 1e-4*y
        f = x'**2 + y'**2 + 0.1*x' * y'
        g = G (x' + y')

solver :: Solver
solver = ipoptSolver { options = [ ("print_time", GBool False)
                                 , ("ipopt.linear_solver", GString "ma86")
                                 --, ("print_level", GInt 0)
                                 ] }

main :: IO ()
main = do
  (_, eopt) <- solveNlp solver myNlp Nothing
  let opt = case eopt of
        Left msg -> error msg
        Right r -> r
      Id obj = splitJV (fOpt opt)
      x = splitJV (xOpt opt)
      g = splitJV (gOpt opt)
--      Sdv obj' x' g' = split (fmapJ exp xopt)
--      Id obj = splitJV obj'
--      x = splitJV x'
--      g = splitJV g'
  putStrLn "***********************************************************"
  putStrLn "solution:"
  print opt
  putStrLn "***********************************************************"
  putStrLn "scaling:"
  putStrLn $ "f: " ++ (printf "%.2e" obj)
  putStrLn $ "x: " ++ show (fmap (printf "%.2e" :: Double -> String) x)
  putStrLn $ "g: " ++ show (fmap (printf "%.2e" :: Double -> String) g)
  putStrLn "***********************************************************"
