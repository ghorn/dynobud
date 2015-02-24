-- | Example of NlpSolver monad testing scaling

{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic1 )

import Casadi.MX ( MX )

import Dyno.Vectorize ( Vectorize, Id(..), None(..) )
import Dyno.View.View
import Dyno.View.JV -- ( JV )
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.NlpUtils
import Dyno.Solvers

data X a = X a a deriving (Functor, Generic1, Show)
data G a = G a deriving (Functor, Generic1, Show)

instance Vectorize X
instance Vectorize G

myNlp :: Nlp' (JV X) (JV None) (JV G) MX
myNlp = Nlp' { nlpFG' = fg
             , nlpBX' = catJV bx
             , nlpBG' = catJV bg
             , nlpX0' = catJV x0
             , nlpP' = catJV None
             , nlpLamX0' = Nothing
             , nlpLamG0' = Nothing
             , nlpScaleF' = Just 2.235
             , nlpScaleX' = Just $ catJV (X 11 77)
             , nlpScaleG' = Just $ catJV (G 666)
             }
  where
    x0 :: X Double
    x0 = X (-8) (-8)

    bx :: X Bounds
    bx = X (Just (-10), Just 10)
           (Just (-10), Just 10)

    bg :: G Bounds
    bg = G (Just (2), Nothing)

    fg :: J (JV X) MX -> J (JV None) MX -> (J (JV Id) MX, J (JV G) MX)
    fg xy _ = (f, catJV' g)
      where
        X x y = splitJV' xy
        f = x**2 + 2*y**2 + 0.1 * x*y
        g = G (0.3*x + 0.4*y)

solver :: Solver
solver = ipoptSolver { options = [ ("print_time", Opt False)
                                 , ("print_level", Opt (0 :: Int))
                                 ] }

runMe' :: NlpSolver (JV X) (JV None) (JV G) ()
runMe' = do
  liftIO $ putStrLn "running it!!"
  getX0 >>= (\x -> liftIO (putStrLn ("x0: " ++ show (splitJV x))))
  getLamX0 >>= (\x -> liftIO (putStrLn ("lam_x0: " ++ show (splitJV x))))
  getLamG0 >>= (\x -> liftIO (putStrLn ("lam_g0: " ++ show (splitJV x))))
  (gradF,f) <- evalGradF
  (jacG, g) <- evalJacG
  hessL <- evalHessLag

  liftIO $ do
    putStrLn $ "f: " ++ show (unId (splitJV (d2v f)))
    putStrLn $ "gradF: " ++ show (splitJV (d2v gradF))
    putStrLn $ "g: " ++ show (splitJV (d2v g))
    putStrLn $ "jacG: " ++ show jacG
    putStrLn $ "hessLag: " ++ show hessL


runMe :: NlpSolver (JV X) (JV None) (JV G) ()
runMe = do
  runMe'
  liftIO $ putStrLn "========================="
  out <- solve'
  liftIO $ print out
  liftIO $ putStrLn "========================="
  getX >>= setX0
  getLamX >>= setLamX0
  getLamG >>= setLamG0
  runMe'
  

main :: IO ()
main = runNlp solver myNlp Nothing runMe
