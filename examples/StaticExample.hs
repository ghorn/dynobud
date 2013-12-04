{-# OPTIONS_GHC -Wall #-}

module Main where

import Hascm.Vectorize
import Hascm.Nats
import Hascm.TypeVecs ( Vec(..), mkVec' )
import Hascm.Nlp
import Hascm.StaticNlp
import Hascm.Ipopt
--import Hascm.Sqp.Sqp
--import Hascm.Sqp.LineSearch
--import Hascm.Snopt

buildRosen :: IO (Nlp (Vec D2) None (Vec D0))
buildRosen = fmap fst $ buildNlp $ do
  
  x <- designVar "position"
  y <- designVar "y"

--  1.2 <== x
  
  minimize $ (1-x)**2 + 100 * (y - x**2)**2


main :: IO ()
main = do
  rosen <- buildRosen

  let x0' = [-8,8]
      x0 = mkVec' x0' :: Vec D2 Double

  ret <- solveNlpIpopt rosen x0 None Nothing
  --ret <- solveNlpSnopt rosen Nothing x0 None Nothing
  print ret
  --(xopt , kktInf) <- solveSqp rosen armilloSearch x0 None
  --print xopt
  --print kktInf
