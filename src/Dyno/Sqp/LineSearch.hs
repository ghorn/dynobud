{-# OPTIONS_GHC -Wall #-}

module Dyno.Sqp.LineSearch ( LineSearch, armilloSearch, ArmilloParams(..), fullStep ) where

import qualified Data.Vector as V

type LineSearch m a = (V.Vector a -> m a) -> V.Vector a -> a -> V.Vector a -> m (Either String (V.Vector a, a))

data ArmilloParams a =
  ArmilloParams { apGamma :: a
                , apBeta :: a
                , apMaxIters :: Int
                }

-- | reasonable default parameters for armillo line search
defaultArmilloParams :: (Fractional a, Ord a) => ArmilloParams a
defaultArmilloParams = ArmilloParams 0.01 0.6 1000

-- | armillo line search with reasonable default parameters
armilloSearch :: (Fractional a, Ord a, Monad m) => LineSearch m a
armilloSearch = armilloSearch' defaultArmilloParams

-- | armillo line search where user provides parameters
armilloSearch' :: (Num a, Ord a, Monad m) => ArmilloParams a -> LineSearch m a
armilloSearch' params _ _ projGrad _
  | projGrad >= 0 = error "line search directional gradient >= 0"
  | apGamma params < 0 = error "line search gamma < 0"
  | apGamma params > 1 = error "line search gamma > 1"
  | apBeta params < 0 = error "line search beta < 0"
  | apBeta params > 1 = error "line search beta > 1"
armilloSearch' params f xk projGrad pk = do
  f0 <- f xk
  let gamma = apGamma params
      beta = apBeta params
      
      armilloSearch'' 0 _ = return $ Left "armillo search ran out of iterations"
      armilloSearch'' n t = do
        let x = V.zipWith (+) xk (V.map (t*) pk)
        fx <- f x
        if fx <= f0 + gamma * t * projGrad
          then return $ Right (x, t)
          else armilloSearch'' (n - 1) (beta * t)

  armilloSearch'' (apMaxIters params) 1
      
-- | take a full step in the search direction
fullStep :: (Monad m, Num a) => LineSearch m a
fullStep _ xk _ pk = return $ Right (V.zipWith (+) xk pk, 1)
