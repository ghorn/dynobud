-- | How to use symbolic map (serial and parallel).

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main ( main ) where

import qualified Data.Map as M
import Data.Proxy ( Proxy(..) )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Linear ( V2(..), V3(..) )
import Text.Printf ( printf )

import Casadi.DMatrix ( DMatrix )
import Casadi.SX ( SX )
import Casadi.MX ( MX )
import Casadi.Option ( Opt(..) )

import qualified Dyno.TypeVecs as TV
import Dyno.Vectorize ( Id(..) )
import Dyno.View.Fun ( FunClass, Fun, SXFun, call, toSXFun, toMXFun, eval )
import Dyno.View.MapFun ( mapFun )
import Dyno.View.M ( M, hcat', hsplit' )
import Dyno.View.JV ( JV, catJV', splitJV' )
import Dyno.View.JVec ( JVec(..) )
import Dyno.View.View ( J )

type N = 300

-- some random function
f0' :: J (JV V2) SX -> J (JV V3) SX
f0' x = catJV' $ V3 (g (100000 :: Int) x0) x1 (2*x1)
  where
    V2 x0 x1 = splitJV' x

    g 0 y = y
    g k y = g (k-1) (sin y)

main :: IO ()
main = do
  let dummyInput :: M (JV V2) (JVec N (JV Id)) DMatrix
      dummyInput = hcat' $ fmap (\x -> catJV' (V2 x (2*x)))
                    (TV.tvlinspace 0 (2*pi))

  show dummyInput `seq` return ()

  -- make a dummy function that's moderately expensive to evaluate
  putStrLn "creating dummy function..."
  f0 <- toSXFun "f0" f0'
        :: IO (SXFun (J (JV V2)) (J (JV V3)))

  let runOne :: FunClass fun
                => String
                -> fun
                   (M (JV V2) (JVec N (JV Id)))
                   (M (JV V3) (JVec N (JV Id)))
                -> IO ()
      runOne name someMap = do
        putStrLn $ "evaluating " ++ name ++ "..."
        t0 <- getCurrentTime
        _ <- eval someMap dummyInput
        t1 <- getCurrentTime
        printf "evaluated %s in %.3f seconds\n"
          name (realToFrac (diffUTCTime t1 t0) :: Double)

  let naiveFun :: M (JV V2) (JVec N (JV Id)) MX -> M (JV V3) (JVec N (JV Id)) MX
      naiveFun xs = hcat' ys
        where
          ys :: TV.Vec N (M (JV V3) (JV Id) MX)
          ys = fmap (call f0) xs'

          xs' :: TV.Vec N (M (JV V2) (JV Id) MX)
          xs' = hsplit' xs

  naive <- toMXFun "naive map" naiveFun
  ser <- mapFun (Proxy :: Proxy N) "serial symbolic map" f0
         (M.fromList [("parallelization", Opt "serial")])
         :: IO (Fun
                (M (JV V2) (JVec N (JV Id)))
                (M (JV V3) (JVec N (JV Id))))
  par <- mapFun (Proxy :: Proxy N) "parallel symbolic map" f0
         (M.fromList [("parallelization", Opt "openmp")])

  runOne "naive map" naive
  runOne "serial symbolic map" ser
  runOne "parallel symbolic map" par
