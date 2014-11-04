{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}

module Main where

import GHC.Generics ( Generic )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dyno.TypeVecs
import Dyno.View
import Dyno.Vectorize
import Dyno.Nlp
import Dyno.Nats
import Dyno.NlpSolver
import Dyno.Solvers

import qualified Data.Foldable as F

import Control.Applicative ( Applicative(..) )
import Linear

instance Applicative X where
 pure = fill
 x0 <*> x1 = devectorize (V.zipWith id (vectorize x0) (vectorize x1))
instance Applicative U where
 pure = fill
 x0 <*> x1 = devectorize (V.zipWith id (vectorize x0) (vectorize x1))

instance Additive X where
 zero = fill 0

instance Additive U where
 zero = fill 0

data X a = X a a deriving (Functor, Generic, Generic1, Show)
data U a = U a deriving (Functor, Generic, Generic1, Show)
instance Vectorize X
instance Vectorize U

data G n a = G (J (JVec n (JV X)) a) deriving (Generic, Generic1, Show)

data Dvs n a = Dvs
               (J (JVec n (JTuple (JV X) (JV U))) a)
               (J (JV X) a)
             deriving (Generic, Generic1, Show)

instance Dim n => View (Dvs n)
instance Dim n => View (G n)

data IntegratorIn a = IntegratorIn (J (JV X) a) (J (JV U) a)
                    deriving (Generic, Generic1)
data IntegratorOut a = IntegratorOut (J (JV X) a)
                    deriving (Generic, Generic1)
instance Scheme IntegratorIn
instance Scheme IntegratorOut

dt :: Floating a => a
dt = 0.1

type Ode a =  X a -> U a -> a -> X a

ode :: Floating a => X a -> U a -> a -> X a
ode (X x v) (U u) _t = X v (-x -0.1*v + u)

rk4 :: Floating a => Ode a -> X a -> U a -> a -> a -> X a
rk4 ode x u t h =  x ^+^ h/6*^(k1 ^+^ 2 *^ k2 ^+^ 2 *^ k3 ^+^ k4)
    where
      k1 = ode x u t
      k2 = ode (x ^+^ h/2 *^ k1) u (t+h/2)
      k3 = ode (x ^+^ h/2 *^ k2) u (t+h/2)
      k4 = ode (x ^+^ h *^ k2) u (t+h)

simulate :: Floating a => Int -> Ode a -> X a -> U a -> a -> a -> X a
simulate  n  ode x0 u t h = xf

    where
      dt = h/ fromIntegral n

      xf = foldl sim x0 [ t+fromIntegral i*dt | i <- [0..(n-1)] ]

      sim x0 t' = rk4 ode x0 u t' dt



makeNlp :: IO (Nlp' (Dvs D20) JNone (G D20) MX)
makeNlp = do
  integrator <- toMXFun "my integrator" $ \(IntegratorIn x0 u) -> IntegratorOut (catJV' (simulate 20 ode (splitJV' x0) (splitJV' u) 0 dt))
  let _ = integrator :: MXFun IntegratorIn IntegratorOut -- just for type signature

  let nlp =
        Nlp'
        { nlpFG' = fg
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

      x0 :: J (Dvs D20) (V.Vector Double)
      x0 = jfill 0

      boundsx = (Just (-1), Just 1)  :: Bounds
      boundsv = (Just (-1), Just 1) :: Bounds
      boundsu = (Just (-1), Just 1) :: Bounds

      initial = (Just 1, Just 1) :: Bounds
      initialX = X (Just 1, Just 1) (Just 0, Just 0) :: X Bounds

      boundsX = X boundsx boundsx :: X Bounds
      jboundsX =  catJV boundsX :: J (JV X) (Vector Bounds)
      jboundsU =  catJV (U boundsu) :: J (JV U) (Vector Bounds)


      bounds_xu = JTuple jboundsX jboundsU :: JTuple (JV X) (JV U) (Vector Bounds)
      initial_xu = JTuple (catJV initialX) jboundsU :: JTuple (JV X) (JV U) (Vector Bounds)

      bounds_xus :: (J (JVec D20 (JTuple (JV X) (JV U))) (Vector Bounds))
      --test2 = jreplicate $ cat $ test
      --test2 = cat $  JVec $ mkVec' $ replicate 20 $ cat $ test
      bounds_xus = cat $  JVec $ mkVec'  ( cat initial_xu : replicate 19 (cat bounds_xu))

      bounds_dvs = Dvs bounds_xus jboundsX

      bx :: J (Dvs D20) (Vector Bounds)
      bx = cat bounds_dvs

      bg :: J (G D20) (Vector Bounds)
      bg = jfill (Just 0, Just 0)

      fg :: J (Dvs D20) MX -> J JNone MX -> (J S MX, J (G D20) MX)
      fg dvs _ = (f, cat g)
        where
          Dvs xus xf = split dvs
          x1s :: Vec D20 (J (JV X) MX)
          x1s = fmap (integrate . split) $ unJVec $ split xus
          integrate (JTuple x0 u) = x1
            where
              IntegratorOut x1 = call integrator (IntegratorIn x0 u)


          us = fmap (extractU . split) $ unJVec $ split xus :: Vec D20 (J (JV U) MX)
          extractU (JTuple x0 u) = u

          reg_U = fmap square us
             where
               square a = cat $ S $ extractU $ unJV $ split $ a *a
               extractU (U a) = a

          reg_X = fmap square x1s
           where
             square a = cat $ S $ sumX $ unJV $ split $ a * a
             sumX (X a b) = a+b

          f :: J S MX
          f = F.sum reg_U + F.sum reg_X


          x0s' = fmap (extractx . split) $ unJVec $ split xus :: Vec D20 (J (JV X) MX)
          extractx (JTuple x0 u) = x0

          x0s = tvtail (x0s' |> xf)  :: Vec D20 (J (JV X) MX)

          gaps:: Vec D20 (J (JV X) MX)
          gaps = tvzipWith (-) x1s x0s

          g :: G D20 MX
          g = G $ cat $ JVec gaps

  return nlp

main :: IO ()
main = do
  myNlp <- makeNlp
  opt <- solveNlp' ipoptSolver myNlp Nothing
  print opt
