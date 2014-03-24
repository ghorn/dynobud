{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )
import qualified Data.Vector as V

import Dyno.Nats
import Dyno.Vectorize
import Dyno.TypeVecs -- ( Vec(..), unVec, mkVec' )
import Dyno.View.View
import Dyno.View.Symbolic
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Ipopt
--import Dyno.Snopt
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch


data X a = X a a deriving (Functor, Generic, Generic1, Show)
data G a = G a deriving (Functor, Generic, Generic1, Show)

instance Vectorize X
instance Vectorize G

myNlp :: Nlp (JV X) JNone (JV G) MX
myNlp = Nlp { nlpFG = fg
            , nlpBX = cat bx
            , nlpBG = cat bg
            , nlpX0 = cat x0
            , nlpP = cat JNone
            }
  where
    x0 :: JV X (V.Vector Double)
    x0 = JV $ fmap V.singleton $ X ((-8)) ((-8))

    bx = JV $ fmap V.singleton $ X (Just (-21), Just 0.5) (Just (-2), Just 2)
    bg = JV $ fmap V.singleton $ G (Just (-10), Just 10)

    fg :: NlpInputs (JV X) JNone MX -> NlpFun (JV G) MX
    fg (NlpInputs xs' _) = NlpFun (cat (S f)) (cat g)
      where
        f = (1-x)**2 + 100*(y - x**2)**2
        g = JV $ G x

        xy :: X MX
        xy = unJV $ split xs'
        X x y = xy

main :: IO ()
main = do
  opt <- solveNlp ipoptSolver myNlp Nothing
  print opt
--  opt2 <- solveNlpSnopt myNlp Nothing guess JNone Nothing
--  print opt2
--  (x0, kktInf) <- solveSqp myNlp armilloSearch guess NNone
--  print x0
--  print kktInf
