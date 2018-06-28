-- | todo(greg):
-- a better name for this module would be Dyno.TechnicalDebt
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Dyno.ExportCStruct
       ( CStructExporter
       , runCStructExporter
       , putTypedef
       , exportTypedef
       , exportCData
       , exportNames
       ) where

import Control.Lens ( (^.) )
import Control.Monad.State.Lazy ( State )
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map as M
import Data.List
import Data.Proxy ( Proxy(..) )
import Text.Printf ( printf )
import Text.Read ( readMaybe )

import Accessors
       ( Lookup, AccessorTree
       , GAData(..), GAConstructor(..), GASimpleEnum(..), GAField(..)
       , accessors, flatten, sameFieldType )
import Dyno.View.Vectorize ( Vectorize, vlength )

runCStructExporter :: State CStructExporter a -> (a, String)
runCStructExporter action =
  case State.runState action (CStructExporter (M.empty, [], [])) of
    (ret, CStructExporter (_, typedefs, [])) -> (ret, unlines (intercalate [""] (reverse typedefs)))
    (_, CStructExporter (_, _, stack)) ->
      error $ "runCStructExporter: stack is not empty!!:\n" ++ unlines stack

data CStructExporter = CStructExporter (M.Map String Constructor, [[String]], [String])

data Constructor where
  Constructor :: GAConstructor a -> Constructor

write :: String -> State CStructExporter ()
write str =
  State.modify $
  \(CStructExporter (set, typedefs, outs)) -> CStructExporter (set, typedefs, str: outs)

typedefStruct :: String -> GAConstructor a -> State CStructExporter ()
typedefStruct typeName (GAConstructor _ fields) = do
  write "typedef struct {"
  mapM_ (uncurry writeCField) fields
  write $ "} " ++ typeName ++ ";"

  CStructExporter (set, typedefs, currentStack) <- State.get
  State.put $ CStructExporter (set, reverse currentStack : typedefs, [])
-- special case Bool:
typedefStruct typeName (GASum (GASimpleEnum {eConstructors = ["False", "True"]})) = do
  write $ "/* C99, requires <stdbool.h> */"
  write $ "typedef bool " ++ typeName ++ ";"
  CStructExporter (set, typedefs, currentStack) <- State.get
  State.put $ CStructExporter (set, reverse currentStack : typedefs, [])
typedefStruct typeName (GASum simpleEnum) = do
  write "typedef enum {"
  let writeEntry opt = write $ "    " ++ opt ++ ","
  mapM_ writeEntry (eConstructors simpleEnum)
  write $ "} " ++ typeName ++ ";"

  CStructExporter (set, typedefs, currentStack) <- State.get
  State.put $ CStructExporter (set, reverse currentStack : typedefs, [])

sameFields :: [(Maybe String, AccessorTree a)] -> [(Maybe String, AccessorTree b)] -> Bool
sameFields xs ys
  | length xs /= length ys = False
  | otherwise = all (uncurry same) (zip xs ys)
  where
    same (nx, fx) (ny, fy) = (nx == ny) && sameTree fx fy

sameTree :: AccessorTree a -> AccessorTree b -> Bool
sameTree (Right (GAData dn0 con0)) (Right (GAData dn1 con1)) =
  dn0 == dn1 && sameConstructor con0 con1
sameTree (Left fx) (Left fy) = sameFieldType fx fy
sameTree _ _ = False

sameConstructor :: GAConstructor a -> GAConstructor b -> Bool
sameConstructor (GAConstructor cn0 fields0) (GAConstructor cn1 fields1) =
  cn0 == cn1 && sameFields fields0 fields1
sameConstructor (GASum simpleEnum0) (GASum simpleEnum1) =
  eConstructors simpleEnum0 == eConstructors simpleEnum1
sameConstructor _ _ = False


typedefStructIfMissing :: String -> GAConstructor a -> State CStructExporter String
typedefStructIfMissing typeName con = do
  CStructExporter (set0, typedefs, currentStack) <- State.get
  case M.lookup typeName set0 of
   -- haven't seen this type name yet, use it
   Nothing -> do
     State.put (CStructExporter (M.insert typeName (Constructor con) set0, typedefs, []))
     typedefStruct typeName con
     State.modify $
       \(CStructExporter (set, structs, _)) -> CStructExporter (set, structs, currentStack)
     return typeName
   -- have seen this type name already, check if it's the one we have seen already
   Just (Constructor con0)
     -- yeah it's the one we have seen already
     | sameConstructor con0 con -> return typeName
     -- uh oh, same name but different type, lets modify the name
     | otherwise -> typedefStructIfMissing newTypeName con
     where
       -- ideally this would sort types by complexity and give simple ones simple names
       newTypeName = typeName ++ "_"
--       newTypeName =
--         error $
--         printf "got two types with the same name: %s\nnew type:\n%s\nold type:\n%s\n"
--         (show typeName)
--         (show fields)
--         (show fields0)

parseVecName :: String -> Maybe Int
parseVecName ('V':'e':'c':' ':k) = readMaybe k
parseVecName _ = Nothing

writeCField :: Maybe String -> AccessorTree a -> State CStructExporter ()
writeCField (Just fieldName) x = writeCField' fieldName x
writeCField Nothing _ = error "writeCField: got a non-record field"

writeCField' :: String -> AccessorTree a -> State CStructExporter ()
writeCField' fieldName (Left f) =
  write $ printf "  %s %s;" (primitiveName f) fieldName
writeCField' fieldName (Right (GAData typeName0 con)) = case parseVecName typeName0 of
  Nothing -> do
    typeName <- typedefStructIfMissing typeName0 con
    write $ printf "  %s %s;" typeName fieldName
  Just k -> do -- handle Vecs as arrays
    childtype <- case con of
      (GASum _) -> error "writeCField: Vec is an Enum"
      (GAConstructor _ []) -> error "writeCField: Vec child has no children"
      (GAConstructor _ ((_, Left f):_)) -> return (primitiveName f)
      (GAConstructor _ ((_, Right (GAData typeName0' con')):_)) ->
        typedefStructIfMissing typeName0' con'
    write $ printf "  %s %s[%d];" childtype fieldName k


primitiveName :: GAField a -> String
primitiveName (FieldDouble _) = "double"
primitiveName (FieldInt64 _   ) = "int64_t"
primitiveName (FieldInt32 _   ) = "int32_t"
primitiveName (FieldInt16 _   ) = "int16_t"
primitiveName (FieldInt8 _   ) = "int8_t"
primitiveName (FieldWord64 _   ) = "uint64_t"
primitiveName (FieldWord32 _   ) = "uint32_t"
primitiveName (FieldWord16 _   ) = "uint16_t"
primitiveName (FieldWord8 _   ) = "uint8_t"
primitiveName (FieldFloat _ ) = "float"
primitiveName (FieldString _) = error "writeCField: strings can't be struct fields :("
primitiveName FieldSorry    =
  error "writeCField: found a FieldSorry (generic-accessors doesn't support a type)"



-- | convenience function to export only one struct
putTypedef :: forall a . Lookup a => Proxy a -> State CStructExporter String
putTypedef _ =
  case handleM33 (accessors :: AccessorTree a) of
    (Right (GAData typeName con)) -> typedefStructIfMissing typeName con
    (Left _) -> error "putStruct: accessors got Field instead of Data"

-- | convenience function to export only one struct
exportTypedef :: Lookup a => Proxy a -> String
exportTypedef = snd . runCStructExporter . putTypedef

-- | Export data as a C struct.
-- If a string with a variable name is given, the variable is declared.
exportCData :: forall a . Lookup a => Int -> Maybe String -> a -> String
exportCData spaces0 maybeVarName theData = case (acc, maybeVarName) of
  (Right (GAData typeName con), Nothing) -> exportStructData theData typeName spaces con
  (Right (GAData typeName con), Just varName) ->
    printf "%s%s %s = {\n%s;" spaces typeName varName
    (exportStructData theData typeName (spaces ++ "  ") con)
  (Left _, _) -> error "exportStructData: accessors got Field instead of Data"
  where
    spaces = replicate spaces0 ' '
    acc = handleM33 accessors

handleM33 :: AccessorTree a -> AccessorTree a
handleM33 r@(Left _) = r
handleM33 r@(Right (GAData _ (GASum _))) = r
handleM33 (Right (GAData "V3" (GAConstructor "V3"
           [ ( Just "x"
             , Right (GAData "V3"
                      (GAConstructor "V3"
                       [ (Just "x", Left field0)
                       , (Just "y", Left field1)
                       , (Just "z", Left field2)
                       ])
                     )
             )
           , ( Just "y"
             , Right (GAData "V3"
                      (GAConstructor "V3"
                       [ (Just "x", Left field3)
                       , (Just "y", Left field4)
                       , (Just "z", Left field5)
                       ])
                     )
             )
           , ( Just "z"
             , Right (GAData "V3"
                      (GAConstructor "V3"
                       [ (Just "x", Left field6)
                       , (Just "y", Left field7)
                       , (Just "z", Left field8)
                       ])
                     )
             )
           ]))) = r
  where
    r = Right (GAData "M33" (GAConstructor "M33"
        [ (Just "xx", Left field0)
        , (Just "xy", Left field1)
        , (Just "xz", Left field2)
        , (Just "yx", Left field3)
        , (Just "yy", Left field4)
        , (Just "yz", Left field5)
        , (Just "zx", Left field6)
        , (Just "zy", Left field7)
        , (Just "zz", Left field8)
        ]))
handleM33 (Right (GAData dname (GAConstructor cname fields))) =
  Right $ GAData dname $ GAConstructor cname $ map (\(n,at) -> (n, handleM33 at)) fields

exportStructData :: forall a . a -> String -> String -> GAConstructor a -> String
exportStructData theData comment spaces (GAConstructor _ fields) =
  spaces ++ intercalate (",\n"++spaces) (map exportFieldData' fields)
  ++ "\n"++ spaces ++ "} /* " ++ comment ++ " */"
  where
    exportFieldData' :: (Maybe String, AccessorTree a) -> String
    exportFieldData' (n,t) = exportFieldData theData n spaces t
exportStructData theData comment spaces (GASum simpleEnum) =
  spaces ++ eToString simpleEnum theData ++ "/* " ++ comment ++ "*/"

toString :: a -> GAField a -> String
toString theData (FieldDouble f) = case show (theData ^. f) of
  "Infinity" -> "INFINITY"
  "-Infinity" -> "-INFINITY"
  r -> r
toString theData (FieldFloat f) = case show (theData ^. f) of
  "Infinity" -> "INFINITY"
  "-Infinity" -> "-INFINITY"
  r -> r
toString theData (FieldInt64  f) = show (theData ^. f)
toString theData (FieldInt32  f) = show (theData ^. f)
toString theData (FieldInt16  f) = show (theData ^. f)
toString theData (FieldInt8   f) = show (theData ^. f)
toString theData (FieldWord64 f) = show (theData ^. f)
toString theData (FieldWord32 f) = show (theData ^. f)
toString theData (FieldWord16 f) = show (theData ^. f)
toString theData (FieldWord8  f) = show (theData ^. f)
toString _ (FieldString _) = "NAN"
toString _ FieldSorry = "NAN"

exportFieldData :: a -> Maybe String -> String -> AccessorTree a -> String
exportFieldData theData (Just fieldName) _ (Left f) =
  printf ".%s = %s" fieldName (toString theData f)
exportFieldData theData Nothing _ (Left f) = toString theData f
exportFieldData theData mfieldName spaces (Right (GAData typeName con)) =
  nameEq ++ "\n" ++ fields
  where
    nameEq = case mfieldName of
      Just fieldName -> printf ".%s = {" fieldName
      Nothing -> "{"

    comment = case mfieldName of
      Just fieldName -> fieldName ++ " (" ++ typeName ++ ")"
      Nothing -> typeName

    fields :: String
    fields = case (parseVecName typeName, con) of
      (Nothing, _) -> exportStructData theData comment (spaces ++ "  ") con
      (Just _, GAConstructor _ subfields) ->
        spaces ++ "  "
        ++ intercalate (",\n"++(spaces++"  "))
           (map (exportFieldData theData Nothing (spaces++"  ") . snd) subfields)
        ++ "\n"++spaces++"  } /* " ++ comment ++ " */"
      (Just _, GASum _) -> error "exportFieldData: field: %s, type %s: Vec is GASum" (show mfieldName) typeName

exportNames :: forall f . (Vectorize f, Lookup (f ())) => Proxy f -> String -> (String, String)
exportNames _ functionName = (src, prototype)
  where
    src =
      unlines
      [ prototype ++ " {"
      , printf "  static const char names[%d][%d] = {%s};"
        n maxLen (intercalate "," (map show names))
      , "  return names[k];"
      , "}"
      ]
    prototype = printf "const char * %s(const int k)" functionName
    maxLen = 1 + maximum (map length names)
    n
      | length names == vlength (Proxy :: Proxy f) = vlength (Proxy :: Proxy f)
      | otherwise = error "exportNames: length mismatch"
    names :: [String]
    names = map (\(name, _) -> name) $ flatten $
            handleM33 (accessors :: AccessorTree (f ()))
