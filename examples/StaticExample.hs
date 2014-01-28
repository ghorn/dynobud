{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Vector as V

import Hascm.Nlp
import Hascm.NlpMonad
import Hascm.Ipopt.Ipopt
--import Hascm.Sqp.Sqp
--import Hascm.Sqp.LineSearch
--import Hascm.Snopt

buildRosen :: IO (Nlp V.Vector V.Vector V.Vector)
buildRosen = fmap fst $ buildNlp' $ do
  
  x <- designVar "x"
  y <- designVar "y"

--  1.2 <== x
--  0 === y
  
  minimize $ (1-x)**2 + 100 * (y - x**2)**2


main :: IO ()
main = do
  rosen0 <- buildRosen

  let rosen = rosen0 {nlpX0 = V.fromList [-8,8]}

  ret <- solveStaticNlpIpopt rosen
  print ret
--  ret' <- solveNlpSnopt rosen Nothing Nothing
--  print ret'
--  (xopt , kktInf) <- solveSqp rosen armilloSearch
--  print xopt
--  print kktInf
