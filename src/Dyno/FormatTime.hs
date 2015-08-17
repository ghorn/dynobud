{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Dyno.FormatTime
       ( formatSeconds
       ) where

import Text.Printf ( printf )

-- | format seconds in a more human readable way
--
-- >>> formatSeconds 59
-- "59.0 seconds"
--
-- >>> formatSeconds 59.999
-- "1 minute, 0.0 seconds"
--
-- >>> formatSeconds 1000
-- "16 minutes, 40.0 seconds"
--
-- >>> formatSeconds 1019.99
-- "17 minutes, 0.0 seconds"
--
-- >>> formatSeconds 3599.9
-- "59 minutes, 59.9 seconds"
--
-- >>> formatSeconds 3599.99
-- "1 hour, 0 minutes, 0.0 seconds"
--
-- >>> formatSeconds 3600
-- "1 hour, 0 minutes, 0.0 seconds"
--
-- >>> formatSeconds 123456
-- "34 hours, 17 minutes, 36.0 seconds"
formatSeconds :: Double -> String
formatSeconds seconds' = formatHMCS hours minutes (round $ 10 * seconds)
  where
    hours :: Int
    hours = floor $ seconds' / (60 * 60)

    minutes :: Int
    minutes = floor $ seconds' / 60 - (fromIntegral hours) * 60

    seconds :: Double
    seconds = seconds' - (fromIntegral hours)*60*60 - (fromIntegral minutes)*60

    formatHMCS :: Int -> Int -> Int -> String
    -- handle printf rounding up
    formatHMCS h m 600 = formatHMCS h (m + 1) 0
    formatHMCS h 60 cs = formatHMCS (h+1) 0 cs
    -- format bumped numbers
    formatHMCS 0 0 cs = printf "%.1f seconds" (fromCS cs)
    formatHMCS 0 1 cs = printf "1 minute, %.1f seconds" (fromCS cs)
    formatHMCS 0 m cs = printf "%d minutes, %.1f seconds" m (fromCS cs)
    formatHMCS 1 1 cs = printf "1 hour, 1 minute, %.1f seconds" (fromCS cs)
    formatHMCS 1 m cs = printf "1 hour, %d minutes, %.1f seconds" m (fromCS cs)
    formatHMCS h m cs = printf "%d hours, %d minutes, %.1f seconds" h m (fromCS cs)

    fromCS :: Int -> Double
    fromCS = (0.1 *) . fromIntegral
