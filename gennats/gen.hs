{-# OPTIONS_GHC -Wall #-}

import Data.Reflection
import Language.Haskell.TH

foo :: Int -> Q [String]
foo k = do
  let d = "D" ++ show k
  typ <- fmap pprint (int k)
  return
    [ "data " ++ d
    , "instance Dim " ++ d ++ " where"
    , "  reflectDim _ = reflect (Proxy :: Proxy (" ++ typ ++ "))"
    ]

foos :: Int -> Q String
foos k = fmap (unlines . concat) $ mapM foo ([0..k] ++ [500,1000,1500,2000])

main :: IO ()
main = do
  defs <- runQ (foos 200)
  let file =
        unlines
        [ "{-# OPTIONS_GHC -Wall #-}"
        , ""
        , "module Hascm.Nats where"
        , ""
        , "import qualified Data.Reflection"
        , "import Data.Reflection ( reflect )"
        , "import Linear.V ( Dim(..) )"
        , "import Data.Proxy"
        , ""
        , defs
        ]
  writeFile "Nats.hs" file
