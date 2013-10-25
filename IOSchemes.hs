{-# OPTIONS_GHC -Wall #-}

module IOSchemes where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Vector as V

import Casadi.Wrappers.Classes.SXMatrix
import Casadi.Wrappers.Classes.IOScheme
import Casadi.Wrappers.Enums ( InputOutputScheme(..) )

mkSchemeSXMatrix :: InputOutputScheme -> [(String,SXMatrix)] -> IO (V.Vector SXMatrix)
mkSchemeSXMatrix schemeEnum userVals = do
  sch <- ioScheme' schemeEnum
  size <- ioScheme_size sch
  let ks = take size [(0::Int)..]
  entries <- mapM (ioScheme_entry sch) ks
  let entrySet :: S.Set String
      entrySet = S.fromList entries

      -- map of user input {String:SXMatrix}, already checked for duplicates
      userMap :: M.Map String SXMatrix
      userMap = M.fromListWithKey err userVals
      err k _ _ = error $ "mkScheme: " ++ show schemeEnum ++ ": you gave key \"" ++ k ++ "\" more than once"

      userSet :: S.Set String
      userSet = M.keysSet userMap

  case F.toList (S.difference userSet entrySet) of
    [] -> return ()
    xs ->  error $ "mkScheme: " ++ show schemeEnum ++ ": you gave keys " ++ show xs ++ " which aren't valid, valid entries:\n" ++ show entries

  let getSXMat str = case M.lookup str userMap of
        Nothing -> sxMatrix -- new empty sxmatrix
        Just x -> return x
  sxMatVec <- fmap V.fromList $ mapM getSXMat entries

--  name <- ioScheme_name sch
--  print size
--  print name
--  ioScheme_entryNames sch >>= putStrLn
--  mapM (ioScheme_entry sch) ks >>= print
--  mapM (ioScheme_entryEnum sch) ks >>= print
--  mapM (ioScheme_entryLabel sch) ks >>= print

  return sxMatVec
