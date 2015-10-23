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

import Control.Monad.State.Lazy ( State )
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map as M
import Data.List
import Data.Proxy ( Proxy(..) )
import Text.Printf ( printf )
import Text.Read ( readMaybe )

import Accessors ( Lookup, AccessorTree(..), Getter(..), Setter(..), accessors, flatten )
import Dyno.Vectorize ( Vectorize, fill, vlength )

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
    same (nx,fx) (ny,fy) = (nx == ny) && sameTree fx fy

sameTree :: AccessorTree a -> AccessorTree b -> Bool
sameTree (Data (x0, x1) fx) (Data (y0, y1) fy) = x0 == y0 && x1 == y1 && sameFields fx fy
sameTree (ATGetter (getx, setx)) (ATGetter (gety, sety)) = sameGet getx gety && sameSet setx sety
  where
    -- todo(greghorn): this will fail if new fields are added, move this to Accessors.hs
    sameGet (GetBool _) (GetBool _) = True
    sameGet (GetDouble _) (GetDouble _) = True
    sameGet (GetFloat _) (GetFloat _) = True
    sameGet (GetInt _) (GetInt _) = True
    sameGet (GetString _) (GetString _) = True
    sameGet _ _ = False

    sameSet (SetBool _) (SetBool _) = True
    sameSet (SetDouble _) (SetDouble _) = True
    sameSet (SetFloat _) (SetFloat _) = True
    sameSet (SetInt _) (SetInt _) = True
    sameSet (SetString _) (SetString _) = True
    sameSet _ _ = False
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
writeCField fieldName (ATGetter (get,_)) =
  write $ printf "  %s %s;" (primitiveName get) fieldName
writeCField fieldName (Data (typeName0, _) fields) = case parseVecName typeName0 of
  Nothing -> do
    typeName <- typedefStructIfMissing typeName0 fields
    write $ printf "  %s %s;" typeName fieldName
  Just k -> do -- handle Vecs as arrays
    childtype <- case fields of
      [] -> error "writeCField: Vec child has no children"
      ((_, ATGetter (get,_)):_) -> return (primitiveName get)
      ((_, Data (typeName0',_) childfields):_) ->
        typedefStructIfMissing typeName0' childfields
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
  case handleM33 (accessors x) of
    (Data (typeName, _) fields) -> typedefStructIfMissing typeName fields
    (ATGetter _) -> error "putStruct: accessors got ATGetter instead of Data"

-- | convenience function to export only one struct
exportTypedef :: Lookup a => a -> String
exportTypedef = snd . runCStructExporter . putTypedef

-- | Export data as a C struct.
-- If a string with a variable name is given, the variable is declared.
exportCData :: forall a . Lookup a => Int -> Maybe String -> a -> String
exportCData spaces0 maybeVarName theData = case (acc, maybeVarName) of
  (Data (typeName,_) fields, Nothing) -> exportStructData theData typeName spaces fields
  (Data (typeName,_) fields, Just varName) ->
    printf "%s%s %s = {\n%s;" spaces typeName varName
    (exportStructData theData typeName (spaces ++ "  ") fields)
  (ATGetter _, _) -> error "exportStructData: accessors got ATGetter instead of Data"
  where
    spaces = replicate spaces0 ' '
    acc = handleM33 (accessors theData)

handleM33 :: AccessorTree a -> AccessorTree a
handleM33 r@(ATGetter (_,_)) = r
handleM33 (Data ("V3","V3")
           [ ("x", Data ("V3","V3") [ ("x", ATGetter (get0,set0))
                                    , ("y", ATGetter (get1,set1))
                                    , ("z", ATGetter (get2,set2))
                                    ])
           , ("y", Data ("V3","V3") [ ("x", ATGetter (get3,set3))
                                    , ("y", ATGetter (get4,set4))
                                    , ("z", ATGetter (get5,set5))
                                    ])
           , ("z", Data ("V3","V3") [ ("x", ATGetter (get6,set6))
                                    , ("y", ATGetter (get7,set7))
                                    , ("z", ATGetter (get8,set8))
                                    ])
           ]) = r
  where
    r = Data ("M33","M33")
        [ ("xx", ATGetter (get0,set0))
        , ("xy", ATGetter (get1,set1))
        , ("xz", ATGetter (get2,set2))
        , ("yx", ATGetter (get3,set3))
        , ("yy", ATGetter (get4,set4))
        , ("yz", ATGetter (get5,set5))
        , ("zx", ATGetter (get6,set6))
        , ("zy", ATGetter (get7,set7))
        , ("zz", ATGetter (get8,set8))
        ]
handleM33 (Data name fields) = Data name $ map (\(n,at) -> (n, handleM33 at)) fields

exportStructData :: forall a . a -> String -> String -> [(String, AccessorTree a)] -> String
exportStructData theData comment spaces fields =
  spaces ++ intercalate (",\n"++spaces) (map exportField' fields)
  ++ "\n"++ spaces ++ "} /* " ++ comment ++ " */"
  where
    exportField' :: (String, AccessorTree a) -> String
    exportField' (n,t) = exportField theData (Just n) spaces t

toString :: a -> Getter a -> String
toString theData (GetDouble get) = case show (get theData) of
  "Infinity" -> "INFINITY"
  "-Infinity" -> "-INFINITY"
  r -> r
toString theData (GetFloat get) = show (get theData)
toString theData (GetInt get) = show (get theData)
toString theData (GetBool get) = show (fromEnum (get theData))
toString _ (GetString _) = "NAN"
toString _ GetSorry = "NAN"

exportField :: a -> Maybe String -> String -> AccessorTree a -> String
exportField theData (Just fieldName) _ (ATGetter (get, _)) =
  printf ".%s = %s" fieldName (toString theData get)
exportField theData Nothing _ (ATGetter (get, _)) = toString theData get
exportField theData mfieldName spaces (Data (typeName,_) subfields) = nameEq ++ "\n" ++ fields
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
    names = map (\(name,_,_) -> name) $ flatten $ handleM33 $ accessors (fill () :: f ())
