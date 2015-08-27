{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno.View.ExportCStruct
       ( CStructExporter
       , runCStructExporter
       , putTypedef
       , exportTypedef
       , exportCData
       ) where

import Control.Monad ( unless )
import Control.Monad.State.Lazy ( State )
import qualified Control.Monad.State.Lazy as State
import Data.List
import qualified Data.Set as S
import Text.Printf ( printf )
import Text.Read ( readMaybe )

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

parseVecName :: String -> Maybe Int
parseVecName ('V':'e':'c':' ':k) = readMaybe k
parseVecName _ = Nothing

writeCField :: String -> AccessorTree a -> State CStructExporter ()
writeCField fieldName (ATGetter (get,_)) =
  write $ printf "  %s %s;" (primitiveName get) fieldName
writeCField fieldName (Data (typeName, _) fields) = case parseVecName typeName of
  Nothing -> do
    typedefStructIfMissing typeName fields
    write $ printf "  %s %s;" typeName fieldName
  Just k -> do -- handle Vecs as arrays
    childtype <- case fields of
      [] -> error "writeCField: Vec child has no children"
      ((_, ATGetter (get,_)):_) -> return (primitiveName get)
      ((_, Data (typename,_) childfields):_) -> do
        typedefStructIfMissing typename childfields
        return typename
    write $ printf "  %s %s[%d];" childtype fieldName k


primitiveName :: Getter a -> String
primitiveName (GetDouble _) = "double"
primitiveName (GetInt _   ) = "int64_t"
primitiveName (GetFloat _ ) = "float"
primitiveName (GetString _) = error "writeCField: strings can't be struct fields :("
primitiveName (GetBool _  ) = error "writeCField: bools can't be struct fields :("
primitiveName GetSorry    =
  error "writeCField: found a GetSorry (generic-accessors doesn't support a type)"



-- | convenience function to export only one struct
putTypedef :: Lookup a => a -> State CStructExporter String
putTypedef x =
  case accessors x of
    (Data (typeName, _) fields) -> typedefStructIfMissing typeName fields >> return typeName
    (ATGetter _) -> error "putStruct: accessors got ATGetter instead of Data"

-- | convenience function to export only one struct
exportTypedef :: Lookup a => a -> String
exportTypedef = snd . runCStructExporter . putTypedef

-- | Export data as a C struct.
-- If a string with a variable name is given, the variable is declared.
exportCData :: forall a . Lookup a => Maybe String -> a -> String
exportCData maybeVarName theData = case (accessors theData, maybeVarName) of
  (Data _ fields, Nothing) -> exportStructData fields
  (Data (typeName,_) fields, Just varName) ->
    printf "%s %s = %s;" typeName varName (exportStructData fields)
  (ATGetter _, _) -> error "exportStructData: accessors got ATGetter instead of Data"
  where
    exportStructData :: [(String, AccessorTree a)] -> String
    exportStructData fields = "{ " ++ intercalate ", " (map (uncurry exportField) fields) ++ " }"

    toString (GetDouble get) = show (get theData)
    toString (GetFloat get) = show (get theData)
    toString (GetInt get) = show (get theData)
    toString (GetBool _) = "NAN"
    toString (GetString _) = "NAN"
    toString GetSorry = "NAN"

    exportField :: String -> AccessorTree a -> String
    exportField fieldName (ATGetter (get, _)) = printf ".%s = %s" fieldName (toString get)
    exportField fieldName (Data _ subfields) =
      printf ".%s = %s" fieldName (exportStructData subfields)
