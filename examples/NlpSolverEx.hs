-- | Example of NlpSolver monad and autoscaling

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic, Generic1 )

import Text.Printf ( printf )

import Casadi.MX ( MX )

import Dyno.Vectorize ( Vectorize, Id(..), None(..), fill )
import Dyno.View.View
import Dyno.View.Viewable
import Dyno.View.JV -- ( JV )
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.NlpUtils
import Dyno.Solvers
import Dyno.AutoScaling

data X a = X a a deriving (Functor, Generic1, Show)
data G a = G a deriving (Functor, Generic1, Show)

instance Vectorize X
instance Vectorize G

myNlp :: Nlp (JV X) (JV None) (JV G) MX
myNlp = Nlp { nlpFG = fg
            , nlpBX = catJV bx
            , nlpBG = catJV bg
            , nlpX0 = catJV x0
            , nlpP = catJV None
            , nlpLamX0 = Nothing
            , nlpLamG0 = Nothing
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
    bx = fill (Nothing, Nothing)

    bg :: G Bounds
    bg = G (Just 2, Nothing)

    fg :: J (JV X) MX -> J (JV None) MX -> (J (JV Id) MX, J (JV G) MX)
    fg xy _ = (f, catJV' g)
      where
        X x y = splitJV' xy
        x' = 1e3*x
        y' = 1e-4*y
        f = x'**2 + y'**2 + 0.1*x' * y'
        g = G (x' + y')

solver :: Solver
solver = ipoptSolver { options = [ ("print_time", Opt False)
                                 , ("linear_solver", Opt "ma86")
                                 --, ("print_level", Opt (0 :: Int))
                                 ] }

quietSolver :: Solver
quietSolver = ipoptSolver { options = [ ("print_time", Opt False)
                                      , ("print_level", Opt (0 :: Int))
                                      , ("linear_solver", Opt "ma86")
                                      ] }

computeKKTs :: NlpSolver (JV X) (JV None) (JV G)
               (KKT (JV X) (JV G), KKT (JV X) (JV G))
computeKKTs = do
  kktU <- evalKKT
  kktS <- evalScaledKKT
  return (kktU, kktS)


runMe :: NlpSolver (JV X) (JV None) (JV G) ((Double, X Double, G Double), (KKT (JV X) (JV G), KKT (JV X) (JV G)))
runMe = do
  (msg,opt') <- solve'
  let opt = case msg of
        Left m -> error m
        Right _ -> opt'
      f = fOpt opt
      x = xOpt opt
      g = gOpt opt
  getX >>= setX0
  getLamX >>= setLamX0
  getLamG >>= setLamG0
  kkts <- computeKKTs
  return ((unId (splitJV f), splitJV x, splitJV g), kkts)

data Sdv a = Sdv (J (JV Id) a) (J (JV X) a) (J (JV G) a) deriving (Generic)
instance View Sdv

expand :: Viewable a => J Sdv a -> (J (JV Id) a, J (JV X) a, J (JV G) a)
expand sdv = (f, x, g)
  where
    Sdv f x g = split sdv

main :: IO ()
main = do
  (opt, (kktU, kktS)) <- runNlp solver myNlp Nothing runMe
  putStrLn "***********************************************************"
  putStrLn "unscaled kkt:"
  putStrLn $ kktScalingInfo kktU
  putStrLn "\nscaled kkt:"
  putStrLn $ kktScalingInfo kktS
  putStrLn "***********************************************************"
  putStrLn $ "unscaled gradF: " ++ show (kktGradF kktU)
  putStrLn $ "scaled gradF:   " ++ show (kktGradF kktS)
  putStrLn ""
  putStrLn $ "unscaled jacG: " ++ show (kktJacG kktU)
  putStrLn $ "scaled jacG:   " ++ show (kktJacG kktS)
  putStrLn ""
  putStrLn $ "unscaled hessLag: " ++ show (kktHessLag kktU)
  putStrLn $ "scaled hessLag:   " ++ show (kktHessLag kktS)

  let snlp = scalingNlp kktU expand
  (msg,opt') <- solveNlp quietSolver snlp Nothing
  let xopt = case msg of
        Left m -> error m
        Right _ -> xOpt opt'
      Sdv obj' x' g' = split (fmapJ exp xopt)
      Id obj = splitJV obj'
      x = splitJV x'
      g = splitJV g'
  putStrLn "***********************************************************"
  putStrLn "solution:"
  print opt
  putStrLn "***********************************************************"
  putStrLn "scaling:"
  putStrLn $ "f: " ++ (printf "%.2e" obj)
  putStrLn $ "x: " ++ show (fmap (printf "%.2e" :: Double -> String) x)
  putStrLn $ "g: " ++ show (fmap (printf "%.2e" :: Double -> String) g)
  putStrLn "***********************************************************"
  putStrLn "before and after"
  putStrLn $ beforeAndAfter kktU expand xopt
  return ()
