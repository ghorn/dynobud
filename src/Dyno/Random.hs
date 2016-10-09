{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.Random
       ( initRandomIO
       ) where

import Control.Monad ( replicateM )
import Data.Proxy ( Proxy(..) )
import Data.Foldable ( toList )
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions
import qualified Numeric.LinearAlgebra.Data as D
import qualified Numeric.LinearAlgebra.HMatrix as HM

import Dyno.View.Vectorize ( Vectorize(..), devectorize, vlength )

initRandomIO :: forall w . (Vectorize w, Foldable w) => w (w Double) -> IO (IO (w Double))
initRandomIO sq = do
  gen0 <- createSystemRandom
  let cov = D.fromLists $ map toList (toList sq) :: HM.Matrix Double
      c = HM.chol (HM.sym cov) :: HM.Matrix Double
      n = vlength (Proxy :: Proxy w)

      mkOne :: IO (w Double)
      mkOne = do
        vs' <- replicateM n (standard gen0)
        let vs = HM.app c (HM.vector vs') :: HM.Vector Double
            w = devectorize $ V.fromList (HM.toList vs)
        return w

  return mkOne
