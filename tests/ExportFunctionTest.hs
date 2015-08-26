-- | Turn a haskell function into a C function.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( Bar(..), Foo(..), main ) where

import GHC.Generics ( Generic, Generic1 )

import Control.Exception ( finally )
import System.Exit ( ExitCode(..) )
import System.Directory ( doesFileExist, removeFile )
import System.Process ( showCommandForUser, spawnProcess, waitForProcess )

import Accessors ( Lookup )

import Dyno.View.ExportCFunction ( CExportOptions(..), exportCFunction' )
import Dyno.View.ExportCStruct ( exportCData )
import Dyno.Vectorize ( Vectorize )

data Bar a =
  Bar
  { barHey :: a
  , barYo :: a
  } deriving (Functor, Generic, Generic1)
instance Lookup a => Lookup (Bar a)
instance Vectorize Bar

data Foo a =
  Foo
  { lol :: a
  , blah :: a
  , excellent :: Bar a
  } deriving (Functor, Generic, Generic1)
instance Lookup a => Lookup (Foo a)
instance Vectorize Foo

myfun :: Floating a => Bar a -> Foo a
myfun (Bar x y) = Foo (x*y) (x/(0.1 + y**2)) (Bar (x + y) ((0.1 + y**2) ** (1 + x**2)))

removeFilesAfter :: Bool
removeFilesAfter = False

main :: IO ()
main = do
  let name = "my_awesome_function"
      opts =
        CExportOptions
        { expand = True
        , generateMain = True
        , exportName = name
        }

  (source0, header) <- exportCFunction' myfun opts
  let -- test the data export functionality
      source = source0 ++
        unlines
        [ ""
        , "// put some data here to check the struct export"
        , exportCData (Just "some_random_data") (Foo 1 2 (Bar 3 4) :: Foo Double)
        ]

      writeFile' path txt = do
        putStrLn $ "writing " ++ show path ++ "..."
        writeFile path txt

      myRunProcess :: FilePath ->  [String] -> IO ()
      myRunProcess path args = do
        let cmd = showCommandForUser path args
        putStrLn $ "running " ++ show cmd ++ "..."
        handle <- spawnProcess path args
        exitCode <- waitForProcess handle
        case exitCode of
          ExitSuccess -> return ()
          failure -> error $ "error running " ++ show cmd ++ ": " ++ show failure

      prefix = "dist/test"
      sourceName = prefix ++ "/" ++ name ++ ".c"
      headerName = prefix ++ "/" ++ name ++ ".h"
      binName = prefix ++ "/dynobud_codegen_test"

      action = do
        writeFile' sourceName source
        writeFile' headerName header
        myRunProcess "gcc" [sourceName, "-lm", "-o", binName]
        myRunProcess ("./" ++ binName) []
        putStrLn $ show binName ++ " ran successfully"

      cleanup = do
        let removeFile' path = do
              exist <- doesFileExist path
              if exist
                then putStrLn ("removing " ++ show path ++ "...") >> removeFile path
                else putStrLn ("not removing " ++ show path ++ " because it doesn't exist")
        putStrLn "cleaning up..."
        mapM_ removeFile' [sourceName, headerName, binName]

  if removeFilesAfter
    then finally action cleanup
    else action
