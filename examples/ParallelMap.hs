-- | How to use symbolic map (serial and parallel).

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main ( main ) where

import qualified Data.Map as M
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Text.Printf ( printf )

import Casadi.DMatrix ( DMatrix )
import Casadi.SX ( SX )
import Casadi.Option ( Opt(..) )

import qualified Dyno.TypeVecs as TV
import Dyno.Vectorize ( Id(..) )
import Dyno.View.Fun ( call, funMap, toSXFun, toMXFun, eval )
import Dyno.View.M ( M, row )
import Dyno.View.JV ( JV, catJV )
import Dyno.View.JVec ( JVec(..) )
import Dyno.View.View ( J, View(..), v2d )

type N = 300

-- todo(greg): one with different sized input/output and non-scalar input/output
-- some random function
f0' :: J (JV Id) SX -> J (JV Id) SX
f0' x = g (100000 :: Int) x
  where
    g 0 y = y
    g k y = g (k-1) (sin y)

main :: IO ()
main = do
  let dummyInput :: J (JVec N (JV Id)) DMatrix
      dummyInput = v2d $ cat $ JVec $ fmap (catJV . Id) (TV.tvlinspace 0 (2*pi))
      dummyInput' :: M (JV Id) (JVec N (JV Id)) DMatrix
      dummyInput' = row dummyInput
  show dummyInput `seq` return ()
  show dummyInput' `seq` return ()

  -- make a dummy function that's moderately expensive to evaluate
  putStrLn "creating dummy function..."
  f0 <- toSXFun "f0" f0'

  let runOne name someMap input = do
        putStrLn $ "evaluating " ++ name ++ "..."
        t0 <- getCurrentTime
        _ <- eval someMap input
        t1 <- getCurrentTime
        printf "evaluated %s in %.3f seconds\n"
          name (realToFrac (diffUTCTime t1 t0) :: Double)

  naive <- toMXFun "naive map" $
           \xs -> cat $ JVec $ fmap (call f0) (unJVec (split xs))
  ser <- funMap "serial symbolic map" f0
         (M.fromList [("parallelization", Opt "serial")])
  par <- funMap "parallel symbolic map" f0
         (M.fromList [("parallelization", Opt "openmp")])

  runOne "naive map" naive dummyInput
  runOne "serial symbolic map" ser dummyInput'
  runOne "parallel symbolic map" par dummyInput'
