{-# OPTIONS_GHC -Wall #-}
{-# Language PackageImports #-}
{-# Language FlexibleContexts #-}

module Dyno.Interface.LogsAndErrors
       ( ErrorMessage (..)
       , LogMessage (..)
       , countLogs
       , debug
       , warn
       , err
       , impossible
       ) where

import "mtl" Control.Monad.Except ( MonadError, throwError )
import "mtl" Control.Monad.Writer ( MonadWriter, tell )

data LogMessage = Debug String
                | Warning String
                | Error String
                | Impossible String

instance Show LogMessage where
  show (Debug x) = "Debug: " ++ x
  show (Warning x) = "Warning: " ++ x
  show (Error x) = "Error: " ++ x
  show (Impossible x) = "\"Impossible\" Error: " ++ x

countLogs' :: (Int,Int,Int,Int) -> [LogMessage] -> (Int,Int,Int,Int)
countLogs' x [] = x
countLogs' (a,b,c,d) (Debug _:xs)      = countLogs' (a+1,   b,   c,   d) xs
countLogs' (a,b,c,d) (Warning _:xs)    = countLogs' (  a, b+1,   c,   d) xs
countLogs' (a,b,c,d) (Error _:xs)      = countLogs' (  a,   b, c+1,   d) xs
countLogs' (a,b,c,d) (Impossible _:xs) = countLogs' (  a,   b,   c, d+1) xs

countLogs :: [LogMessage] -> (Int,Int,Int,Int)
countLogs = countLogs' (0,0,0,0)

newtype ErrorMessage = ErrorMessage String -- deriving Error
instance Show ErrorMessage where
  show (ErrorMessage msg) = msg

logMessage :: MonadWriter [t] m => t -> m ()
logMessage x = tell [x]

debug :: MonadWriter [LogMessage] m => String -> m ()
debug = logMessage . Debug

warn :: MonadWriter [LogMessage] m => String -> m ()
warn = logMessage . Warning

err :: (MonadError ErrorMessage m, MonadWriter [LogMessage] m) =>
       String -> m a
err x = logMessage (Error x) >> throwError (ErrorMessage x)

impossible :: (MonadError ErrorMessage m, MonadWriter [LogMessage] m) =>
              String -> m b
impossible x = logMessage (Impossible x) >> throwError (ErrorMessage ("\"impossible error\": " ++ x))
