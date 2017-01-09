-- | How to use symbolic map (serial and parallel).

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main ( main ) where

import Data.Proxy ( Proxy(..) )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Linear ( V2(..), V3(..) )
import Text.Printf ( printf )

import Casadi.DM ( DM )
import Casadi.SX ( SX )
import Casadi.MX ( MX )

import qualified Dyno.TypeVecs as TV
import Dyno.View.Fun ( Fun, Symbolic(..), callMX, callDM )
import Dyno.View.MapFun ( MapStrategy(..), mapFun )
import Dyno.View.M ( M, hcat', hsplit', vcat, vsplit )
import Dyno.View.JVec ( JVec(..) )
import Dyno.View.Vectorize ( Id(..) )
import Dyno.View.View ( J, JV )

type N = 300

-- some random function
f0' :: J (JV V2) SX -> J (JV V3) SX
f0' x = vcat $ V3 (g (100000 :: Int) x0) x1 (2*x1)
  where
    V2 x0 x1 = vsplit x

    g 0 y = y
    g k y = g (k-1) (sin y)

main :: IO ()
main = do
  let dummyInput :: M (JV V2) (JVec N (JV Id)) DM
      dummyInput = hcat' $ fmap (\x -> vcat (V2 x (2*x)))
                    (TV.tvlinspace 0 (2*pi))

  show dummyInput `seq` return ()

  -- make a dummy function that's moderately expensive to evaluate
  putStrLn "creating dummy function..."
  f0 <- toFun "f0" f0' mempty
        :: IO (Fun (J (JV V2)) (J (JV V3)))

  let runOne :: String
                -> Fun
                   (M (JV V2) (JVec N (JV Id)))
                   (M (JV V3) (JVec N (JV Id)))
                -> IO ()
      runOne name someMap = do
        putStrLn $ "evaluating " ++ name ++ "..."
        t0 <- getCurrentTime
        _ <- callDM someMap dummyInput
        t1 <- getCurrentTime
        printf "evaluated %s in %.3f seconds\n"
          name (realToFrac (diffUTCTime t1 t0) :: Double)

  let naiveFun :: M (JV V2) (JVec N (JV Id)) MX -> M (JV V3) (JVec N (JV Id)) MX
      naiveFun xs = hcat' ys
        where
          ys :: TV.Vec N (M (JV V3) (JV Id) MX)
          ys = fmap (callMX f0) xs'

          xs' :: TV.Vec N (M (JV V2) (JV Id) MX)
          xs' = hsplit' xs

  naive <- toFun "naive_map" naiveFun mempty
  unroll <- mapFun (Proxy :: Proxy N) f0 "unrolled_symbolic_map" Unroll mempty
         :: IO (Fun
                (M (JV V2) (JVec N (JV Id)))
                (M (JV V3) (JVec N (JV Id))))
  ser <- mapFun (Proxy :: Proxy N) f0 "serial_symbolic_map" Serial mempty
         :: IO (Fun
                (M (JV V2) (JVec N (JV Id)))
                (M (JV V3) (JVec N (JV Id))))
  par <- mapFun (Proxy :: Proxy N) f0 "parallel_symbolic_map" OpenMP mempty

  runOne "naive map" naive
  runOne "unrolled symbolic_map" unroll
  runOne "serial symbolic map" ser
  runOne "parallel symbolic map" par
