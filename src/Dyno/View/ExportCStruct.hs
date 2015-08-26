{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.View.ExportCStruct
       ( CStructExporter
       , runCStructExporter
       , putStruct
       , exportStruct
       ) where

import Control.Monad ( unless )
import Control.Monad.State.Lazy ( State )
import qualified Control.Monad.State.Lazy as State
import Data.List
import qualified Data.Set as S
import Text.Printf ( printf )

import Accessors ( Lookup, AccessorTree(..), Getter(..), accessors )
--import Dyno.Vectorize ( Vectorize, fill )

runCStructExporter :: State CStructExporter a -> (a, String)
runCStructExporter action =
  case State.runState action (CStructExporter (S.empty, [], [])) of
    (ret, CStructExporter (_, typedefs, [])) -> (ret, unlines (intercalate [""] (reverse typedefs)))
    (_, CStructExporter (_, _, stack)) ->
      error $ "runCStructExporter: stack is not empty!!:\n" ++ unlines stack

data CStructExporter = CStructExporter (S.Set String, [[String]], [String])

write :: String -> State CStructExporter ()
write str =
  State.modify $
  \(CStructExporter (set, typedefs, outs)) -> CStructExporter (set, typedefs, str: outs)

typedefStruct :: String -> [(String, AccessorTree a)] -> State CStructExporter ()
typedefStruct typeName fields = do
--  write $ "// " ++ typeName
  write "typedef struct {"
  mapM_ (uncurry writeCField) fields
  write $ "} " ++ typeName ++ ";"

  CStructExporter (set, typedefs, currentStack) <- State.get
  State.put $ CStructExporter (set, (reverse currentStack) : typedefs, [])

typedefStructIfMissing :: String -> [(String, AccessorTree a)] -> State CStructExporter ()
typedefStructIfMissing typeName fields = do
  CStructExporter (set0, typedefs, currentStack) <- State.get
  unless (S.member typeName set0) $ do
    State.put (CStructExporter (S.insert typeName set0, typedefs, []))
    typedefStruct typeName fields
    State.modify $
      \(CStructExporter (set, structs, _)) -> CStructExporter (set, structs, currentStack)

writeCField :: String -> AccessorTree a -> State CStructExporter ()
writeCField fieldName (Data (typeName, _) fields) = do
  typedefStructIfMissing typeName fields
  write (printf "  %s %s;" typeName fieldName)
writeCField fieldName (ATGetter (get,_)) = case get of
  GetDouble _ -> write (printf "  double %s;" fieldName)
  GetInt _    -> write (printf "  int64_t %s;" fieldName)
  GetFloat _  -> write (printf "  float %s;" fieldName)
  GetString _ -> error "writeCField: strings can't be struct fields :("
  GetBool _   -> error "writeCField: bools can't be struct fields :("
  GetSorry    -> error "writeCField: found a GetSorry (generic-accessors doesn't support a type)"


-- | convenience function to export only one struct
putStruct :: Lookup a => a -> State CStructExporter String
putStruct x =
  case accessors x of
    (Data (typeName, _) fields) -> typedefStructIfMissing typeName fields >> return typeName
    (ATGetter _) -> error "putStruct: accessors got ATGetter instead of Data"

-- | convenience function to export only one struct
exportStruct :: Lookup a => a -> String
exportStruct = snd . runCStructExporter . putStruct
