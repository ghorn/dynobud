{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

module Hascm.Ipopt.Ipopt
       ( solveNlpIpopt
       , solveStaticNlpIpopt
       , solveStaticOcpIpopt
       ) where

import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import Data.Proxy

import Hascm.Vectorize
import Hascm.Nlp
import Hascm.NlpMonad ( reifyNlp )
import Hascm.Ocp ( OcpPhase )
import Hascm.OcpMonad ( reifyOcp )
import Hascm.DirectCollocation ( CollTraj, makeCollNlp )
import Hascm.DirectCollocation.Dynamic ( DynCollTraj, ctToDynamic )
import qualified Hascm.TypeVecs as TV
import Hascm.Ipopt.Monad

inf :: Double
inf = read "Infinity"

toBnds :: Vectorize f => f (Maybe Double, Maybe Double) -> (f Double, f Double)
toBnds vs = (devectorize (V.map (fromMaybe (-inf)) lb), devectorize (V.map (fromMaybe inf) ub))
  where
    (lb,ub) = V.unzip (vectorize vs)


-- | convenience function to solve a pure Nlp
solveNlpIpopt ::
  (Vectorize x, Vectorize p, Vectorize g) =>
  Nlp x p g -> Maybe (x Double -> IO Bool)
  -> IO (Either String (NlpOut x g Double))
solveNlpIpopt nlp callback = do
  runIpopt (nlpFG nlp) callback $ do

    let (lbx,ubx) = toBnds (nlpBX nlp)
        (lbg,ubg) = toBnds (nlpBG nlp)

    setOption "max_iter" (3000 :: Int)
    --setOption "tol" (1e-9 :: Double)
    --setOption "fixed_variable_treatment" "make_constraint" -- causes segfaults?
    --setOption "fixed_variable_treatment" "make_parameter"
    setX0 (nlpX0 nlp)
    setP (nlpP nlp)
    setLbx lbx
    setUbx ubx
    setLbg lbg
    setUbg ubg
    solve'

solveStaticNlpIpopt ::
  Nlp V.Vector V.Vector V.Vector -> Maybe (V.Vector Double -> IO Bool) -> IO (Either String (NlpOut V.Vector V.Vector Double))
solveStaticNlpIpopt nlp callback = reifyNlp nlp callback foo
  where
    foo ::
      (Vectorize x, Vectorize p, Vectorize g) =>
      Nlp x p g -> Maybe (x Double -> IO Bool) ->
      IO (Either String (NlpOut V.Vector V.Vector Double))
    foo nlp' cb' = do
      ret <- solveNlpIpopt nlp' cb'
      return $ case ret of
        Left x -> Left x
        Right (NlpOut { fOpt = fopt
                      , xOpt = xopt
                      , gOpt = gopt
                      , lambdaOpt = Multipliers { lambdaX = lambdax
                                                , lambdaG = lambdag
                                                }}) ->
          Right NlpOut { fOpt = fopt
                       , xOpt = vectorize xopt
                       , gOpt = vectorize gopt
                       , lambdaOpt = Multipliers { lambdaX = vectorize lambdax
                                                 , lambdaG = vectorize lambdag
                                                 }}


--solveOcpIpopt ::
--  forall x z u p r c h n deg .
--  (Dim deg, Dim n, Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize c, Vectorize h)
--  => OcpPhase x z u p r c h -> Maybe (CollTraj x z u p n deg Double) -> IO ()
--solveOcpIpopt ocp guess' = do
--    let guess :: CollTraj x z u p n deg Double
--        guess = case guess' of Nothing -> fill 1
--                               Just g -> g
--    _ <- solveNlpIpopt ((makeCollNlp ocp) {nlpX0 = guess}) Nothing
--    return ()

solveOcpIpopt' ::
  forall x z u p r o c h .
  (Vectorize x, Vectorize z, Vectorize u, Vectorize p, Vectorize r, Vectorize o, Vectorize c, Vectorize h)
  => Int -> Int -> Maybe (DynCollTraj Double -> IO Bool) -> OcpPhase x z u p r o c h -> IO ()
solveOcpIpopt' n deg cb ocp =
  TV.reifyDim n $ \(Proxy :: Proxy n) ->
  TV.reifyDim deg $ \(Proxy :: Proxy deg) -> do
    let guess :: CollTraj x z u p n deg Double
        guess = fill 1
    _ <- solveNlpIpopt ((makeCollNlp ocp) {nlpX0 = guess}) (fmap (. ctToDynamic) cb)
    return ()

solveStaticOcpIpopt ::
  Int -> Int -> Maybe (DynCollTraj Double -> IO Bool) -> OcpPhase V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector V.Vector -> IO ()
solveStaticOcpIpopt n deg cb ocp = reifyOcp ocp (solveOcpIpopt' n deg cb)
