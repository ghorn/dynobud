{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}

module Dyno.MultipleShooting
       ( MsOcp(..)
       , MsDvs(..)
       , MsConstraints(..)
       , makeMsNlp
       ) where

import GHC.Generics ( Generic )
import Data.Vector ( Vector )
import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import Linear
import qualified Data.Foldable as F

import Dyno.TypeVecs
import Dyno.View
import Dyno.View.Scheme
import Dyno.Vectorize
import Dyno.Nlp


data IntegratorIn x u p a = IntegratorIn (J (JV x) a) (J (JV u) a) (J (JV p) a)
                          deriving (Generic, Generic1)
data IntegratorOut x a = IntegratorOut (J (JV x) a)
                       deriving (Generic, Generic1)
instance (Vectorize x, Vectorize u, Vectorize p) => Scheme (IntegratorIn x u p)
instance Vectorize x => Scheme (IntegratorOut x)

type Ode x u p a = x a -> u a -> p a -> a -> x a

-- problem specification
data MsOcp x u p =
  MsOcp
  { msOde :: Ode x u p (J (JV Id) MX)
  , msMayer :: x (J (JV Id) MX) -> J (JV Id) MX
  , msLagrangeSum :: x (J (JV Id) MX) -> u (J (JV Id) MX) -> J (JV Id) MX
  , msX0 :: x (Maybe Double)
  , msXF :: x (Maybe Double)
  , msXBnds :: x Bounds
  , msUBnds :: u Bounds
  , msPBnds :: p Bounds
  , msEndTime :: Double
  , msNumRk4Steps :: Maybe Int
  }

-- design variables
data MsDvs x u p n a =
  MsDvs
  { dvXus :: J (JVec n (JTuple (JV x) (JV u))) a
  , dvXf :: J (JV x) a
  , dvP :: J (JV p) a
  } deriving (Generic, Generic1)
instance (Vectorize x, Vectorize u, Vectorize p, Dim n) => View (MsDvs x u p n)

-- constraints
data MsConstraints x n a =
  MsConstraints
  { gContinuity :: J (JVec n (JV x)) a
  } deriving (Generic, Generic1)
instance (Vectorize x, Dim n) => View (MsConstraints x n)

rk4 :: (Floating a, Additive x) => (x a -> u a -> p a -> a -> x a) -> x a -> u a -> p a -> a -> a -> x a
rk4 f x0 u p t h =  x0 ^+^ h/6*^(k1 ^+^ 2 *^ k2 ^+^ 2 *^ k3 ^+^ k4)
    where
      k1 = f x0 u p t
      k2 = f (x0 ^+^ h/2 *^ k1) u p (t+h/2)
      k3 = f (x0 ^+^ h/2 *^ k2) u p (t+h/2)
      k4 = f (x0 ^+^ h *^ k2) u p (t+h)

simulate :: (Floating a, Additive x) => Int -> Ode x u p a -> x a -> u a -> p a -> a -> a -> x a
simulate n ode x0' u p t h = xf
    where
      dt' = h/ fromIntegral n

      xf = foldl sim x0' [ t+fromIntegral i*dt' | i <- [0..(n-1)] ]

      sim x0'' t' = rk4 ode x0'' u p t' dt'

makeMsNlp ::
  forall x u p n
  . (Dim n, Vectorize x, Vectorize u, Vectorize p, Additive x)
  => MsOcp x u p -> IO (Nlp' (MsDvs x u p n) JNone (MsConstraints x n) MX)
makeMsNlp msOcp = do
  let n = reflectDim (Proxy :: Proxy n)
      integrate (IntegratorIn x0 u p) = IntegratorOut (catJV' (simulate nsteps ode x0' u' p' 0 dt))
        where
          endTime = msEndTime msOcp
          dt = (realToFrac endTime) / fromIntegral n
          ode = msOde msOcp
          nsteps = fromMaybe 1 (msNumRk4Steps msOcp)
          x0' = splitJV' x0
          u' = splitJV' u
          p' = splitJV' p
  integrator <- toMXFun "my integrator" integrate
  let _ = integrator :: MXFun (IntegratorIn x u p) (IntegratorOut x) -- just for type signature

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

      x0 :: J (MsDvs x u p n) (V.Vector Double)
      x0 = jfill 0

      boundsX0 = catJV (fmap (\x -> (x,x)) (msX0 msOcp)) :: J (JV x) (Vector Bounds)

      boundsX =  catJV (msXBnds msOcp) :: J (JV x) (Vector Bounds)
      boundsU =  catJV (msUBnds msOcp) :: J (JV u) (Vector Bounds)

      boundsX0u = JTuple boundsX0 boundsU :: JTuple (JV x) (JV u) (Vector Bounds)
      boundsXu  = JTuple boundsX  boundsU :: JTuple (JV x) (JV u) (Vector Bounds)
      boundsXF = catJV (fmap (\x -> (x,x)) (msXF msOcp)) :: J (JV x) (Vector Bounds)

      boundsXus :: (J (JVec n (JTuple (JV x) (JV u))) (Vector Bounds))
      boundsXus = cat $  JVec $ mkVec'  ( cat boundsX0u : replicate (n-1) (cat boundsXu))

      bx :: J (MsDvs x u p n) (Vector Bounds)
      bx = cat MsDvs
               { dvXus = boundsXus
               , dvXf = boundsXF
               , dvP = catJV (msPBnds msOcp)
               }

      bg :: J (MsConstraints x n) (Vector Bounds)
      bg = cat MsConstraints { gContinuity = jfill (Just 0, Just 0) }

      fg :: J (MsDvs x u p n) MX -> J JNone MX -> (J S MX, J (MsConstraints x n) MX)
      fg dvs _ = (f, cat g)
        where
          MsDvs xus xf p = split dvs
          x1s :: Vec n (J (JV x) MX)
          x1s = fmap (callIntegrate . split) $ unJVec $ split xus
          callIntegrate (JTuple x0' u) = x1
            where
              IntegratorOut x1 = call integrator (IntegratorIn x0' u p)

          lagrangeSum = F.sum $ fmap callLagrangeSum (unJVec (split xus))
            where
              callLagrangeSum xu = msLagrangeSum msOcp (splitJV' x) (splitJV' u)
                where
                  JTuple x u = split xu

          mayer = msMayer msOcp (splitJV' xf)

          f :: J S MX
          f = mkJ $ unJ $ mayer + lagrangeSum


          x0s' = fmap (extractx . split) $ unJVec $ split xus :: Vec n (J (JV x) MX)
          extractx (JTuple x0'' _) = x0''

          x0s = tvtail (x0s' |> xf)  :: Vec n (J (JV x) MX)

          gaps:: Vec n (J (JV x) MX)
          gaps = tvzipWith (-) x1s x0s

          g :: MsConstraints x n MX
          g = MsConstraints { gContinuity = cat $ JVec gaps }

  return nlp
