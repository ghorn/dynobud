-- todo(greg):
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
       ( Lookup, AccessorTree(..), Field(..)
       , accessors, flatten, sameFieldType )
import Dyno.Vectorize ( Vectorize, vlength )

runCStructExporter :: State CStructExporter a -> (a, String)
runCStructExporter action =
  case State.runState action (CStructExporter (M.empty, [], [])) of
    (ret, CStructExporter (_, typedefs, [])) -> (ret, unlines (intercalate [""] (reverse typedefs)))
    (_, CStructExporter (_, _, stack)) ->
      error $ "runCStructExporter: stack is not empty!!:\n" ++ unlines stack

data CStructExporter = CStructExporter (M.Map String Fields, [[String]], [String])

data Fields where
  Fields :: [(String, AccessorTree a)] -> Fields

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
  State.put $ CStructExporter (set, reverse currentStack : typedefs, [])

sameFields :: [(String, AccessorTree a)] -> [(String, AccessorTree b)] -> Bool
sameFields xs ys
  | length xs /= length ys = False
  | otherwise = all (uncurry same) (zip xs ys)
  where
    same (nx, fx) (ny, fy) = (nx == ny) && sameTree fx fy

sameTree :: AccessorTree a -> AccessorTree b -> Bool
sameTree (Data (x0, x1) fx) (Data (y0, y1) fy) = x0 == y0 && x1 == y1 && sameFields fx fy
sameTree (Field fx) (Field fy) = sameFieldType fx fy
sameTree _ _ = False

typedefStructIfMissing :: String -> [(String, AccessorTree a)] -> State CStructExporter String
typedefStructIfMissing typeName fields = do
  CStructExporter (set0, typedefs, currentStack) <- State.get
  case M.lookup typeName set0 of
   -- haven't seen this type name yet, use it
   Nothing -> do
     State.put (CStructExporter (M.insert typeName (Fields fields) set0, typedefs, []))
     typedefStruct typeName fields
     State.modify $
       \(CStructExporter (set, structs, _)) -> CStructExporter (set, structs, currentStack)
     return typeName
   -- have seen this type name already, check if it's the one we have seen already
   Just (Fields fields0)
     -- yeah it's the one we have seen already
     | sameFields fields0 fields -> return typeName
     -- uh oh, same name but different type, lets modify the name
     | otherwise -> typedefStructIfMissing newTypeName fields
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

writeCField :: String -> AccessorTree a -> State CStructExporter ()
writeCField fieldName (Field f) =
  write $ printf "  %s %s;" (primitiveName f) fieldName
writeCField fieldName (Data (typeName0, _) fields) = case parseVecName typeName0 of
  Nothing -> do
    typeName <- typedefStructIfMissing typeName0 fields
    write $ printf "  %s %s;" typeName fieldName
  Just k -> do -- handle Vecs as arrays
    childtype <- case fields of
      [] -> error "writeCField: Vec child has no children"
      ((_, Field f):_) -> return (primitiveName f)
      ((_, Data (typeName0',_) childfields):_) ->
        typedefStructIfMissing typeName0' childfields
    write $ printf "  %s %s[%d];" childtype fieldName k


primitiveName :: Field a -> String
primitiveName (FieldDouble _) = "double"
primitiveName (FieldInt _   ) = "int64_t"
primitiveName (FieldFloat _ ) = "float"
primitiveName (FieldString _) = error "writeCField: strings can't be struct fields :("
primitiveName (FieldBool _  ) = error "writeCField: bools can't be struct fields :("
primitiveName FieldSorry    =
  error "writeCField: found a GetSorry (generic-accessors doesn't support a type)"



-- | convenience function to export only one struct
putTypedef :: forall a . Lookup a => Proxy a -> State CStructExporter String
putTypedef _ =
  case handleM33 (accessors :: AccessorTree a) of
    (Data (typeName, _) fields) -> typedefStructIfMissing typeName fields
    (Field _) -> error "putStruct: accessors got Field instead of Data"

-- | convenience function to export only one struct
exportTypedef :: Lookup a => Proxy a -> String
exportTypedef = snd . runCStructExporter . putTypedef

-- | Export data as a C struct.
-- If a string with a variable name is given, the variable is declared.
exportCData :: forall a . Lookup a => Int -> Maybe String -> a -> String
exportCData spaces0 maybeVarName theData = case (acc, maybeVarName) of
  (Data (typeName,_) fields, Nothing) -> exportStructData theData typeName spaces fields
  (Data (typeName,_) fields, Just varName) ->
    printf "%s%s %s = {\n%s;" spaces typeName varName
    (exportStructData theData typeName (spaces ++ "  ") fields)
  (Field _, _) -> error "exportStructData: accessors got Field instead of Data"
  where
    spaces = replicate spaces0 ' '
    acc = handleM33 accessors

handleM33 :: AccessorTree a -> AccessorTree a
handleM33 r@(Field _) = r
handleM33 (Data ("V3","V3")
           [ ("x", Data ("V3","V3") [ ("x", Field field0)
                                    , ("y", Field field1)
                                    , ("z", Field field2)
                                    ])
           , ("y", Data ("V3","V3") [ ("x", Field field3)
                                    , ("y", Field field4)
                                    , ("z", Field field5)
                                    ])
           , ("z", Data ("V3","V3") [ ("x", Field field6)
                                    , ("y", Field field7)
                                    , ("z", Field field8)
                                    ])
           ]) = r
  where
    r = Data ("M33","M33")
        [ ("xx", Field field0)
        , ("xy", Field field1)
        , ("xz", Field field2)
        , ("yx", Field field3)
        , ("yy", Field field4)
        , ("yz", Field field5)
        , ("zx", Field field6)
        , ("zy", Field field7)
        , ("zz", Field field8)
        ]
handleM33 (Data name fields) = Data name $ map (\(n,at) -> (n, handleM33 at)) fields

exportStructData :: forall a . a -> String -> String -> [(String, AccessorTree a)] -> String
exportStructData theData comment spaces fields =
  spaces ++ intercalate (",\n"++spaces) (map exportField' fields)
  ++ "\n"++ spaces ++ "} /* " ++ comment ++ " */"
  where
    exportField' :: (String, AccessorTree a) -> String
    exportField' (n,t) = exportField theData (Just n) spaces t

toString :: a -> Field a -> String
toString theData (FieldDouble f) = case show (theData ^. f) of
  "Infinity" -> "INFINITY"
  "-Infinity" -> "-INFINITY"
  r -> r
toString theData (FieldFloat f) = show (theData ^. f)
toString theData (FieldInt f) = show (theData ^. f)
toString theData (FieldBool f) = show (fromEnum (theData ^. f))
toString _ (FieldString _) = "NAN"
toString _ FieldSorry = "NAN"

exportField :: a -> Maybe String -> String -> AccessorTree a -> String
exportField theData (Just fieldName) _ (Field f) =
  printf ".%s = %s" fieldName (toString theData f)
exportField theData Nothing _ (Field f) = toString theData f
exportField theData mfieldName spaces (Data (typeName,_) subfields) =
  nameEq ++ "\n" ++ fields
  where
    nameEq = case mfieldName of
      Just fieldName -> printf ".%s = {" fieldName
      Nothing -> "{"

    comment = case mfieldName of
      Just fieldName -> fieldName ++ " (" ++ typeName ++ ")"
      Nothing -> typeName

    fields :: String
    fields = case parseVecName typeName of
      Nothing -> exportStructData theData comment (spaces ++ "  ") subfields
      Just _ ->
        spaces ++ "  "
        ++ intercalate (",\n"++(spaces++"  "))
           (map (exportField theData Nothing (spaces++"  ") . snd) subfields)
        ++ "\n"++spaces++"  } /* " ++ comment ++ " */"

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
