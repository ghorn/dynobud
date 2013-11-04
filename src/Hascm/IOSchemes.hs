{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Hascm.IOSchemes
       ( mkSchemeSXMatrix
       , mkSchemeCRSSparsity
       ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Vector as V

import Casadi.Wrappers.Classes.CRSSparsity ( CRSSparsity, crsSparsity' )
import Casadi.Wrappers.Classes.SXMatrix ( SXMatrix, sxMatrix )
import Casadi.Wrappers.Classes.IOScheme ( ioScheme', ioScheme_size, ioScheme_entry )
import Casadi.Wrappers.Enums ( InputOutputScheme(..) )

mkScheme :: forall a . IO a -> InputOutputScheme -> [(String,a)] -> IO (V.Vector a)
mkScheme newEmpty schemeEnum userVals = do
  sch <- ioScheme' schemeEnum
  size <- ioScheme_size sch
  let ks = take size [(0::Int)..]
  entries <- mapM (ioScheme_entry sch) ks
  let entrySet :: S.Set String
      entrySet = S.fromList entries

      -- map of user input {String:a}, already checked for duplicates
      userMap :: M.Map String a
      userMap = M.fromListWithKey err userVals
      err k _ _ = error $ "mkScheme: " ++ show schemeEnum ++ ": you gave key \"" ++ k ++ "\" more than once"

      userSet :: S.Set String
      userSet = M.keysSet userMap

  case F.toList (S.difference userSet entrySet) of
    [] -> return ()
    xs ->  error $ "mkScheme: " ++ show schemeEnum ++ ": you gave keys " ++ show xs ++ " which aren't valid, valid entries:\n" ++ show entries

  let getElem str = case M.lookup str userMap of
        Nothing -> newEmpty -- new empty
        Just x -> return x
  vec <- fmap V.fromList $ mapM getElem entries

--  name <- ioScheme_name sch
--  print size
--  print name
--  ioScheme_entryNames sch >>= putStrLn
--  mapM (ioScheme_entry sch) ks >>= print
--  mapM (ioScheme_entryEnum sch) ks >>= print
--  mapM (ioScheme_entryLabel sch) ks >>= print

  return vec

mkSchemeSXMatrix :: InputOutputScheme -> [(String,SXMatrix)] -> IO (V.Vector SXMatrix)
mkSchemeSXMatrix = mkScheme sxMatrix

mkSchemeCRSSparsity :: InputOutputScheme -> [(String,CRSSparsity)] -> IO (V.Vector CRSSparsity)
mkSchemeCRSSparsity = mkScheme crsSparsity'
