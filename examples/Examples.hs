{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

--import GHC.Generics ( Generic1 )
import qualified Data.Vector as V
import Data.TypeLevel.Num.Reps
--import Data.TypeLevel.Num.Aliases
import Vectorize
import TypeVecs ( Vec(..), mkVec )

import Nlp

myNlp :: Nlp (Vec D2) (Vec D0)
myNlp = Nlp fg bx bg
  where
    bx = mkVec (V.fromList [(Just (-21), Just 2),(Just (-2), Just 2)])
    bg = mkVec (V.fromList [])
    
    --fg :: Vec D2 a -> NlpFun D1 a
    fg xs' = NlpFun f g
      where
        f = (1-x)**2 + 100*(y - x**2)**2
        g = mkVec $ V.fromList []
        
        xs = vectorize xs'
        x = xs V.! 0
        y = xs V.! 1

main :: IO ()
main = do
  ret <- solveNlp myNlp
  print ret
