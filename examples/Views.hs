-- | Example for using View
-- This takes a composite type Pose and breaks it into finer and finer pieces until
-- scalars are reached. Then it concatenates them into bigger pieces until the original
-- vector is reconstructed

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics ( Generic )

import Dyno.View
import Dyno.View.Symbolic

data Xyz a = Xyz (J S a) (J S a) (J S a) deriving (Generic, Show)
data Quat a = Quat (J S a) (J S a) (J S a) (J S a) deriving (Generic, Show)
data Pose a = Pose (J Xyz a) (J Quat a) deriving (Generic, Show)

instance View Xyz
instance View Quat
instance View Pose

data Proxy a = Proxy

prnt :: Show a => String -> a -> IO ()
prnt blah x = putStrLn $ blah ++ ": " ++ show x

go :: forall a . Symbolic a => Proxy a -> IO ()
go _ = do
  poseJ <- sym "x" :: IO (J Pose a)
  let pose :: Pose a
      xyzJ :: J Xyz a
      quatJ :: J Quat a
      pose@(Pose xyzJ quatJ) = split poseJ

  prnt "poseJ" poseJ
  prnt "pose"  pose 
  prnt "xyzJ"  xyzJ 
  prnt "quatJ" quatJ

  let xyz :: Xyz a
      xJ,yJ,zJ :: J S a -- really just 3 scalars
      xyz@(Xyz xJ yJ zJ) = split xyzJ
      quat :: Quat a
      q0J,q1J,q2J,q3J :: J S a -- really just 4 scalars
      quat@(Quat q0J q1J q2J q3J) = split quatJ

  prnt "[xJ,yJ,zJ]" [xJ,yJ,zJ]
  prnt "[q0J,q1J,q2J,q3J]" [q0J,q1J,q2J,q3J]

  -- now concatenate them again
  let xyzJ' :: J Xyz a
      xyzJ' = cat (Xyz xJ yJ zJ)

      quatJ' :: J Quat a
      quatJ' = cat (Quat q0J q1J q2J q3J)

      pose' :: Pose a
      pose' = Pose xyzJ' quatJ'

      poseJ' = cat pose'
  prnt "xyzJ'"  xyzJ' 
  prnt "quatJ'" quatJ'
  prnt "pose'"  pose' 
  prnt "poseJ'" poseJ'

main :: IO ()
main = do
  putStrLn "SX!!"
  go (Proxy :: Proxy SX)

  putStrLn "\nMX!!"
  go (Proxy :: Proxy MX)
