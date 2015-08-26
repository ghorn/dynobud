-- | Turn a haskell function into a C function.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( Bar(..), Foo(..), main ) where

import GHC.Generics ( Generic, Generic1 )
import Accessors ( Lookup )

import Dyno.View.ExportCFunction ( CExportOptions(..), exportCFunction' )
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
myfun (Bar x y) = Foo (x*y) (x/y) (Bar (x + y) (y ** sin x))

main :: IO ()
main = do
  let opts =
        CExportOptions
        { expand = True
        --, generateMain = False
        , generateMain = True
        , exportName = "my_awesome_function"
        }
  (source, header) <- exportCFunction' myfun opts

  putStrLn source
  putStrLn "=================================================================="
  putStrLn header
