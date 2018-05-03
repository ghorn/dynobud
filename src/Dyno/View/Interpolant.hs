{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Dyno.View.Interpolant
       ( interpolant1, interpolant2, interpolant3, interpolant4
       , makeInterpolant1, makeInterpolant2, makeInterpolant3, makeInterpolant4

       , Interps(..)
       , casadiInterps
       , ffiInterps
       ) where


import Control.Compose ( Id(..) )
import Control.Monad ( void, when )
import Data.List ( intercalate )
import Data.Map ( Map )
import Data.Int ( Int32 )
import qualified Data.Vector as V
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.Ptr ( FunPtr, Ptr )
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array ( mallocArray, newArray, peekArray, withArray )
import Linear ( V2(..), V3(..), V4(..) )

import qualified Casadi.Interpolant as C
import Casadi.GenericType ( GType )

import Dyno.View.Fun ( Fun(..), checkFunDimensions )
import Dyno.View.Vectorize ( Vectorize(vectorize) )
import Dyno.View.View ( J, JV, S )

data RawInterpolant
foreign import ccall unsafe "interpolant.hpp new_interpolant" c_newInterpolant
  :: Ptr (Ptr CDouble) -> Int32 -> Ptr Int32 -> Ptr CDouble -> Int32 -> CString -> IO (Ptr RawInterpolant)

foreign import ccall unsafe "interpolant.hpp eval_interpolant" c_evalInterpolant
  :: Ptr RawInterpolant -> Ptr CDouble -> Ptr CDouble -> IO ()

foreign import ccall unsafe "interpolant.hpp &delete_interpolant" c_deleteInterpolant
  :: FunPtr (Ptr RawInterpolant-> IO ())

data Interpolant = Interpolant Int Int (ForeignPtr RawInterpolant)

newInterpolant :: String -> [[Double]] -> [Double] -> IO Interpolant
newInterpolant lookupMode grid values = do
  let ndims = length grid
      gridLengths = map length grid
      nvalues = length values

      gridLengthProd = product gridLengths

      noutputs :: Int
      noutputs = nvalues `div` gridLengthProd

  void $ when (noutputs * gridLengthProd /= nvalues) $
    error $
    intercalate "\n"
    [ "newInterpolant:"
    , "grid lengths: " ++ show gridLengths
    , "# values: " ++ show nvalues
    , "noutputs: " ++ show noutputs
    ]

  gridPtrs <- mapM (newArray . map realToFrac) grid :: IO [Ptr CDouble]
  gridPtr <- newArray gridPtrs :: IO (Ptr (Ptr CDouble))
  gridLengthsPtr <- newArray (map fromIntegral gridLengths) :: IO (Ptr Int32)
  valuesPtr <- newArray (map realToFrac values) :: IO (Ptr CDouble)
  raw <- withCString lookupMode $
         c_newInterpolant gridPtr (fromIntegral ndims) gridLengthsPtr valuesPtr (fromIntegral nvalues)
  free gridLengthsPtr
  free valuesPtr
  free gridPtr
  mapM_ free gridPtrs

  Interpolant ndims noutputs <$> newForeignPtr c_deleteInterpolant raw

evalInterpolant :: Interpolant -> [Double] -> IO [Double]
evalInterpolant (Interpolant ndims noutputs raw) inputs
  | length inputs /= ndims =
      error $
      intercalate "\n"
      [ "interpolant called with wrong number of values"
      , "expected dimension: " ++ show ndims
      , "given dimensions: " ++ show (length inputs)
      ]
  | otherwise = do
      outputPtr <- mallocArray noutputs :: IO (Ptr CDouble)
      withArray (map realToFrac inputs) $ \inputPtr ->
        withForeignPtr raw $ \obj ->
        c_evalInterpolant obj inputPtr outputPtr
      ret <- peekArray noutputs outputPtr :: IO [CDouble]
      free outputPtr
      return (map realToFrac ret)


arrangeValues2 ::
  forall f0 f1
  . ( Vectorize f0, Traversable f0
    , Vectorize f1
    )
  => f0 Double -> f1 Double
  -> f0 (f1 Double)
  -> (V.Vector (V.Vector Double), V.Vector Double)
arrangeValues2 grid0 grid1 values0 = (grid, vectorizedValues)
  where
    -- transpose values
    values :: f1 (f0 Double)
    values = sequenceA values0

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize (vectorize values)

    grid :: V.Vector (V.Vector Double)
    grid = V.fromList [vectorize grid0, vectorize grid1]

arrangeValues3 ::
  forall f0 f1 f2
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Traversable f1
    , Vectorize f2
    )
  => f0 Double -> f1 Double -> f2 Double
  -> f0 (f1 (f2 Double))
  -> (V.Vector (V.Vector Double), V.Vector Double)
arrangeValues3 grid0 grid1 grid2 values0 = (grid, vectorizedValues)
  where
    -- transpose values
    values :: f2 (f1 (f0 Double))
    values = v3
      where
        v0 :: f0 (f1 (f2 Double))
        v0 = values0

        v1 :: f1 (f0 (f2 Double))
        v1 = sequenceA v0

        v2 :: f1 (f2 (f0 Double))
        v2 = fmap sequenceA v1

        v3 :: f2 (f1 (f0 Double))
        v3 = sequenceA v2

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize $ V.concatMap vectorize (vectorize values)

    grid :: V.Vector (V.Vector Double)
    grid = V.fromList [vectorize grid0, vectorize grid1, vectorize grid2]


arrangeValues4 ::
  forall f0 f1 f2 f3
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Traversable f1
    , Vectorize f2, Traversable f2
    , Vectorize f3
    )
  => f0 Double -> f1 Double -> f2 Double -> f3 Double
  -> f0 (f1 (f2 (f3 Double)))
  -> (V.Vector (V.Vector Double), V.Vector Double)
arrangeValues4 grid0 grid1 grid2 grid3 values0 = (grid, vectorizedValues)
  where
    -- transpose values
    values :: f3 (f2 (f1 (f0 Double)))
    values = v6
      where
        v0 :: f0 (f1 (f2 (f3 Double)))
        v0 = values0

        v1 :: f1 (f0 (f2 (f3 Double)))
        v1 = sequenceA v0

        v2 :: f1 (f2 (f0 (f3 Double)))
        v2 = fmap sequenceA v1

        v3 :: f1 (f2 (f3 (f0 Double)))
        v3 = fmap (fmap sequenceA) v2

        v4 :: f2 (f1 (f3 (f0 Double)))
        v4 = sequenceA v3

        v5 :: f2 (f3 (f1 (f0 Double)))
        v5 = fmap sequenceA v4

        v6 :: f3 (f2 (f1 (f0 Double)))
        v6 = sequenceA v5

    vectorizedValues :: V.Vector Double
    vectorizedValues = V.concatMap vectorize $ V.concatMap vectorize $ V.concatMap vectorize (vectorize values)

    grid :: V.Vector (V.Vector Double)
    grid = V.fromList [vectorize grid0, vectorize grid1, vectorize grid2, vectorize grid3]


interpolant1 :: String -> String -> V.Vector (Double, Double) -> Map String GType -> Fun S S
interpolant1 name solver gridAndValues opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant1 " ++ name ++ ": " ++ err
  where
    fun :: Fun S S
    fun = Fun $ C.interpolant name solver (V.singleton grid) values opts
    (grid, values) = V.unzip gridAndValues


interpolant2 ::
  forall f0 f1
  . ( Vectorize f0, Traversable f0
    , Vectorize f1
    )
  => String -> String
  -> f0 Double -> f1 Double
  -> f0 (f1 Double)
  -> Map String GType
  -> Fun (J (JV V2)) S
interpolant2 name solver grid0 grid1 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant2 " ++ name ++ ": " ++ err
  where
    fun :: Fun (J (JV V2)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts

    (grid, vectorizedValues) = arrangeValues2 grid0 grid1 values0

interpolant3 ::
  forall f0 f1 f2
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Traversable f1
    , Vectorize f2
    )
  => String -> String
  -> f0 Double -> f1 Double -> f2 Double
  -> f0 (f1 (f2 Double))
  -> Map String GType
  -> Fun (J (JV V3)) S
interpolant3 name solver grid0 grid1 grid2 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant3 " ++ name ++ ": " ++ err
  where
    grid :: V.Vector (V.Vector Double)
    vectorizedValues :: V.Vector Double
    (grid, vectorizedValues) = arrangeValues3 grid0 grid1 grid2 values0

    fun :: Fun (J (JV V3)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts


interpolant4 ::
  forall f0 f1 f2 f3
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Traversable f1
    , Vectorize f2, Traversable f2
    , Vectorize f3
    )
  => String -> String
  -> f0 Double -> f1 Double -> f2 Double -> f3 Double
  -> f0 (f1 (f2 (f3 Double)))
  -> Map String GType
  -> Fun (J (JV V4)) S
interpolant4 name solver grid0 grid1 grid2 grid3 values0 opts = case checkFunDimensions fun of
  Right _ -> fun
  Left err -> error $ "error making interpolant4 " ++ name ++ ": " ++ err
  where
    grid :: V.Vector (V.Vector Double)
    vectorizedValues :: V.Vector Double
    (grid, vectorizedValues) = arrangeValues4 grid0 grid1 grid2 grid3 values0

    fun :: Fun (J (JV V4)) S
    fun = Fun $ C.interpolant name solver grid vectorizedValues opts


makeInterpolant1 :: String -> V.Vector (Double, Double) -> IO (Double -> IO Double)
makeInterpolant1 lookupName gridAndValues = do
  let (grid, values) = V.unzip gridAndValues

  interpolant <- newInterpolant lookupName [V.toList grid] (V.toList values)
  let interpolate :: Double -> IO Double
      interpolate x = do
        ret <- evalInterpolant interpolant [x]
        case ret of
          [r] -> return r
          _ -> error $ "interpolant1 expected outputs of length 1, got length " ++ show (length ret)

  return interpolate


makeInterpolant2 ::
  forall f0 f1
  . ( Vectorize f0, Traversable f0
    , Vectorize f1
    )
  => String
  -> f0 Double -> f1 Double
  -> f0 (f1 Double)
  -> IO (V2 Double -> IO Double)
makeInterpolant2 lookupName grid0 grid1 values0 = do
  let grid :: V.Vector (V.Vector Double)
      vectorizedValues :: V.Vector Double
      (grid, vectorizedValues) = arrangeValues2 grid0 grid1 values0
  interpolant <- newInterpolant lookupName (map V.toList (V.toList grid)) (V.toList vectorizedValues)
  let interpolate :: V2 Double -> IO Double
      interpolate (V2 x y) = do
        ret <- evalInterpolant interpolant [x, y]
        case ret of
          [r] -> return r
          _ -> error $ "interpolant2 expected outputs of length 1, got length " ++ show (length ret)

  return interpolate

makeInterpolant3 ::
  forall f0 f1 f2
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Traversable f1
    , Vectorize f2
    )
  => String
  -> f0 Double -> f1 Double -> f2 Double
  -> f0 (f1 (f2 Double))
  -> IO (V3 Double -> IO Double)
makeInterpolant3 lookupName grid0 grid1 grid2 values0 = do
  let grid :: V.Vector (V.Vector Double)
      vectorizedValues :: V.Vector Double
      (grid, vectorizedValues) = arrangeValues3 grid0 grid1 grid2 values0
  interpolant <- newInterpolant lookupName (map V.toList (V.toList grid)) (V.toList vectorizedValues)
  let interpolate :: V3 Double -> IO Double
      interpolate (V3 x y z) = do
        ret <- evalInterpolant interpolant [x, y, z]
        case ret of
          [r] -> return r
          _ -> error $ "interpolant3 expected outputs of length 1, got length " ++ show (length ret)

  return interpolate


makeInterpolant4 ::
  forall f0 f1 f2 f3
  . ( Vectorize f0, Traversable f0
    , Vectorize f1, Traversable f1
    , Vectorize f2, Traversable f2
    , Vectorize f3
    )
  => String
  -> f0 Double -> f1 Double -> f2 Double -> f3 Double
  -> f0 (f1 (f2 (f3 Double)))
  -> IO (V4 Double -> IO Double)
makeInterpolant4 lookupName grid0 grid1 grid2 grid3 values0 = do
  let grid :: V.Vector (V.Vector Double)
      vectorizedValues :: V.Vector Double
      (grid, vectorizedValues) = arrangeValues4 grid0 grid1 grid2 grid3 values0
  interpolant <- newInterpolant lookupName (map V.toList (V.toList grid)) (V.toList vectorizedValues)
  let interpolate :: V4 Double -> IO Double
      interpolate (V4 x y z w) = do
        ret <- evalInterpolant interpolant [x, y, z, w]
        case ret of
          [r] -> return r
          _ -> error $ "interpolant3 expected outputs of length 1, got length " ++ show (length ret)

  return interpolate


data Interps a =
  Interps
  (V.Vector (Double, Double) -> IO (a -> IO a))
  (forall f0 f1 . (Traversable f0, Vectorize f0, Vectorize f1)
    => f0 Double -> f1 Double -> f0 (f1 Double) -> IO (V2 a -> IO a))
  (forall f0 f1 f2 . (Traversable f0, Traversable f1, Vectorize f0, Vectorize f1, Vectorize f2)
    => f0 Double -> f1 Double -> f2 Double -> f0 (f1 (f2 Double)) -> IO (V3 a -> IO a))
  (forall f0 f1 f2 f3 . (Traversable f0, Traversable f1, Traversable f2, Vectorize f0, Vectorize f1, Vectorize f2, Vectorize f3)
    => f0 Double -> f1 Double -> f2 Double -> f3 Double -> f0 (f1 (f2 (f3 Double))) -> IO (V4 a -> IO a))

casadiInterps :: (forall f . Vectorize f => Fun (J (JV f)) S -> f a -> IO a)
              -> Interps a
casadiInterps toCall =
  Interps
  (\gridAndValues -> let f = interpolant1 "woo" "linear" gridAndValues mempty in return (toCall f . Id))
  (\g0 g1       v -> let f = interpolant2 "woo" "linear" g0 g1       v mempty in return (toCall f))
  (\g0 g1 g2    v -> let f = interpolant3 "woo" "linear" g0 g1 g2    v mempty in return (toCall f))
  (\g0 g1 g2 g3 v -> let f = interpolant4 "woo" "linear" g0 g1 g2 g3 v mempty in return (toCall f))

ffiInterps :: Interps Double
ffiInterps =
  Interps
  (makeInterpolant1 "exact")
  (makeInterpolant2 "exact")
  (makeInterpolant3 "exact")
  (makeInterpolant4 "exact")
