{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dyno.Nats
import Dyno.View
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Ipopt
--import Dyno.Snopt
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch


data X a = X (J S a) (J S a) deriving (Generic, Show)
data G a = G (J S a) deriving (Generic, Show)

instance View X
instance View G

myNlp :: Nlp X JNone G MX
myNlp = Nlp { nlpFG = fg
            , nlpBX = bx
            , nlpBG = bg
            , nlpX0 = x0
            , nlpP = cat JNone
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

    fg :: (J X MX, J JNone MX) -> (J S MX, J G MX)
    fg (xy, _) = (f, cat g)
      where
        f = (1-x)**2 + 100*(y - x**2)**2
        g = G x

        X x y = split xy

main :: IO ()
main = do
  opt <- solveNlp ipoptSolver myNlp Nothing
  print opt
--  opt2 <- solveNlpSnopt myNlp Nothing guess JNone Nothing
--  print opt2
--  (x0, kktInf) <- solveSqp myNlp armilloSearch guess NNone
--  print x0
--  print kktInf
