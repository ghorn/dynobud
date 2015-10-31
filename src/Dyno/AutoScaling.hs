{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Dyno.AutoScaling
       ( scalingNlp
       , kktScalingInfo
       , beforeAndAfter
       ) where

import Data.List ( minimumBy, maximumBy )
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
--import qualified Numeric.LinearAlgebra.Data as HMat
--import qualified Numeric.LinearAlgebra.HMatrix as HMat
import Text.Printf ( printf )

import qualified Casadi.CMatrix as CM
import Casadi.DMatrix ( DMatrix, dnonzeros )
import Casadi.MX ( MX )
import Casadi.Sparsity ( getRow, getCol )

import Dyno.View.JV ( JV, splitJV )
import Dyno.View.M ( M )
import qualified Dyno.View.M as M
import Dyno.Nlp ( KKT(..), Nlp(..) )
import Dyno.View.Unsafe ( mkJ, unJ, unM )
import Dyno.Vectorize ( Id(..) )
import Dyno.View.View ( View(..), J, JNone(..), v2d, d2v, jfill)


toSparse :: (View f, View g) => String -> M f g DMatrix -> [(Int,Int,Double)]
toSparse name mat0
  | V.length row /= V.length col = error $ name ++ " row/column index mismatch"
  | V.length row /= V.length dat = error $ name ++ " sparsity patter size doesn't match data size"
  | otherwise = V.toList $ V.zip3 row col dat
  where
    mat = unM $ M.sparse mat0

    sp = CM.sparsity mat
    dat = dnonzeros mat
    row = getRow sp
    col = getCol sp

kktScalingInfo :: (View f, View g) => KKT f g -> String
kktScalingInfo kkt =
  init $ unlines
  [ showOne "hessLag  " (kktHessLag kkt)
  , showOne "hessF    " (kktHessF kkt)
  , showOne "hessLamG " (kktHessLambdaG kkt)
  , showOne "jacG     " (kktJacG kkt)
  , showOne "gradF    " (kktGradF kkt)
  ]
  where
    showOne name m =
      printf "%s size (%5d, %5d), nonzeros %7d/%10d (%6.2f %%), min: %s, max: %s, ratio: %s"
      name r c nz (r*c)
      (100 * fromIntegral nz / fromIntegral (r*c) :: Double)
      min' max' ratio
      where
        byAbs x y = compare (abs x) (abs y)
        min' = case d of
          [] -> "      N/A"
          ds -> printf "% 8.2e" (minimumBy byAbs ds)
        max' = case d of
          [] -> "      N/A"
          ds -> printf "% 8.2e" (maximumBy byAbs ds)
        ratio = case d of
          [] -> "      N/A"
          ds -> printf "% 8.2e" (minimumBy byAbs ds / maximumBy byAbs ds)

        nz = length d
        (_,_,d) = unzip3 (toSparse name m)
        r = CM.size1 (unM m)
        c = CM.size2 (unM m)

-- log |aij| + sj + si (+ sf)
data LogScaling a =
  LogScaling
  { lsHessF :: [a]
  , lsHessLambdaG :: [a]
  , lsHessLag :: [a]
  , lsJacG :: [a]
  , lsGradF :: [a]
  } deriving Functor


toObjective :: Floating a => LogScaling a -> a
toObjective (LogScaling hf hlg hl jg gf) = sum (map sqr hf) + sum (map sqr hlg) + 0*sum (map sqr hl) + 2*sum (map sqr jg) + sum (map sqr gf)
  where
    sqr x = x*x

toMatrixCoeffs :: Floating a => LogScaling a -> LogScaling a
toMatrixCoeffs (LogScaling hf hlg hl jg gf) = LogScaling (f hf) (f hlg) (f hl) (f jg) (f gf)
  where
    f = map exp

toLogScaling ::
  forall x g sdv a
  . (View x, View g, View sdv, CM.CMatrix a)
  => KKT x g -> (J sdv a -> (J (JV Id) a, J x a, J g a)) -> J sdv a -> LogScaling (J (JV Id) a)
toLogScaling kkt expand sdvs =
  LogScaling
  { lsJacG = jacGObjValues
  , lsHessF = hessFObjValues
  , lsHessLambdaG = hessLambdaGObjValues
  , lsHessLag = hessLagObjValues
  , lsGradF = gradFObjValues
  }
  where
    jacGMatValues = toSparse "jacG" (kktJacG kkt)
    hessFMatValues = toSparse "hessF" (kktHessF kkt)
    hessLambdaGMatValues = toSparse "hessLamG" (kktHessLambdaG kkt)
    hessLagMatValues = toSparse "hessLag" (kktHessLag kkt)
    gradFMatValues = toSparse "gradF" (kktGradF kkt)

    objScale' :: J (JV Id) a
    x :: J x a
    g' :: J g a
    (objScale', x, g') = expand sdvs
    -- constraints and objective are inverted
    objScale = negate objScale'
    g = negate g'

    reproxy :: J f a -> Proxy f
    reproxy = const Proxy

    nx = size (reproxy x)
    ng = size (reproxy g)
    xs,gs :: V.Vector (J (JV Id) a)
    xs = fmap mkJ $ CM.vertsplit (unJ x) (V.fromList [0..nx])
    gs = fmap mkJ $ CM.vertsplit (unJ g) (V.fromList [0..ng])

    gradFObjValues :: [J (JV Id) a]
    gradFObjValues = map (toSum xs (V.singleton objScale)) gradFMatValues

    jacGObjValues :: [J (JV Id) a]
    jacGObjValues = map (toSum gs xs) jacGMatValues

    hessFObjValues :: [J (JV Id) a]
    hessFObjValues = map ((+ objScale) . toSum xs xs) hessFMatValues

    hessLambdaGObjValues :: [J (JV Id) a]
    hessLambdaGObjValues = map ((+ objScale) . toSum xs xs) hessLambdaGMatValues

    hessLagObjValues :: [J (JV Id) a]
    hessLagObjValues = map ((+ objScale) . toSum xs xs) hessLagMatValues


toSum :: forall a .
         (Fractional a) =>
         V.Vector a -> V.Vector a -> (Int, Int, Double) -> a
toSum rowVec colVec (rowi,colj,value)
  | absValue == 0 = error "toSum: log(0)"
  | logAbsValue' < -1000 = error "really really small value"
  | logAbsValue' >  1000 = error "really really big value"
  | otherwise = logAbsValue + si + sj
  where
    absValue = abs value
    logAbsValue = realToFrac (log absValue)
    logAbsValue' = log absValue

    si,sj :: a
    si = rowVec V.! rowi
    sj = colVec V.! colj



scalingNlp ::
 forall x g sdv
 . (View x, View g, View sdv)
 => KKT x g -> (J sdv MX -> (J (JV Id) MX, J x MX, J g MX))
 -> Nlp sdv JNone JNone MX
scalingNlp kkt expand =
  Nlp
  { nlpBX = jfill (Nothing, Nothing)
  , nlpBG = cat JNone
  , nlpX0 = jfill 0 -- unit scaling, initially
  , nlpP = cat JNone
  , nlpLamX0 = Nothing
  , nlpLamG0 = Nothing
  , nlpScaleF = Nothing
  , nlpScaleX = Nothing
  , nlpScaleG = Nothing
  , nlpFG = fg
  }
  where
    fg :: J sdv MX -> J JNone MX -> (J (JV Id) MX, J JNone MX)
    fg sdvs _ = (obj, cat JNone)
      where
        obj = toObjective $ toLogScaling kkt expand sdvs


beforeAndAfter
  :: (View x, View g, View sdv)
     => KKT x g
     -> (J sdv DMatrix -> (J (JV Id) DMatrix, J x DMatrix, J g DMatrix))
     -> J sdv (V.Vector Double)
     -> String
beforeAndAfter kkts expand scalingSol =
  init $ unlines
  [ minMax "hessF0" hessF0
  , minMax "hessF " hessF
  , ""
  , minMax "hessLamG0" hessLamG0
  , minMax "hessLamG " hessLamG
  , ""
  , minMax "hessLag0" hessLag0
  , minMax "hessLag " hessLag
  , ""
  , minMax "jacG0" jacG0
  , minMax "jacG " jacG
  , ""
  , minMax "gradF0" gradF0
  , minMax "gradF " gradF
  ]
  where
      ls0 = fmap (unId . splitJV . d2v) $ toLogScaling kkts expand (v2d (jfill 0))
      LogScaling hessF0 hessLamG0 hessLag0 jacG0 gradF0 = toMatrixCoeffs ls0 :: LogScaling Double

      ls :: LogScaling Double
      ls = fmap (unId . splitJV . d2v) $ toLogScaling kkts expand (v2d scalingSol)
      LogScaling hessF hessLamG hessLag jacG gradF = toMatrixCoeffs ls :: LogScaling Double
      minMax name xs = printf "%s min: %s, max: %s, ratio: %s" name min' max' ratio
        where
          -- protect against empty list
          min' = case xs of
            [] -> "N/A"
            xs' -> printf "% 8.2e" (minimum xs')
          max' = case xs of
            [] -> "N/A"
            xs' -> printf "% 8.2e" (maximum xs')
          ratio = case xs of
            [] -> "N/A"
            xs' -> printf "% 8.2e" (minimum xs' / maximum xs')



--analyzeSol :: Nlp'
--              (CollTraj AcX None AcU AcP NCollStages CollDeg)
--              JNone
--              (CollOcpConstraints NCollStages CollDeg AcX AcX Bc PathC)
--              MX ->
--              Save
--              (CollTraj AcX None AcU AcP NCollStages CollDeg)
--              (CollOcpConstraints NCollStages CollDeg AcX AcX Bc PathC) ->
--              IO ()
--analyzeSol nlp save = do
--  let sol = savedNlpOut save
--  putStrLn "creating jacobian..."
--  --nj <- nlpJac nlp
--  nj' <- nlpJac' nlp
--  putStrLn "evaluating jacobian..."
--  --(jacFG', fg) <- nj (v2d (xOpt' sol)) (cat JNone)
--  (dgdx, _) <- nj' (v2d (xOpt' sol))
--  putStrLn "finished! analyzing..."
--  let --JTuple f0' g0' = split fg
--      --Id _f0 = splitJV (d2v f0')
--      --_g0 = unJ $ d2v g0'
--      --
--      --dfgdx :: M
--      --         (JTuple (JV Id) (CollOcpConstraints NCollStages CollDeg AcX AcX Bc PathC))
--      --         (CollTraj AcX None AcU AcP NCollStages CollDeg)
--      --         DMatrix
--      --(dfgdx,_) = M.hsplitTup jacFG'
--      --_dfdx :: M (JV Id) (CollTraj AcX None AcU AcP NCollStages CollDeg) DMatrix
--      --dgdx :: M
--      --        (CollOcpConstraints NCollStages CollDeg AcX AcX Bc PathC)
--      --        (CollTraj AcX None AcU AcP NCollStages CollDeg)
--      --        DMatrix
--      --(_dfdx, dgdx) = M.vsplitTup dfgdx
--
--
--      -- todo: this only works for worhp heh
--      isActive :: Double -> Bool
--      isActive lambda = (abs lambda) > 1e-15
----      isActive :: (Double,Double) -> Double -> Double -> Bool
----      isActive (lb, ub) val lambda
----        | val <= lb = True
----        | ub <= val = True
----        | (abs lambda) > 1e-15 = True
----        | otherwise = False
--
--      activeX = V.map isActive (unJ (lambdaXOpt' sol))
--      activeG = V.map isActive (unJ (lambdaGOpt' sol))
--      activeAll = activeX V.++ activeG
--
--      activeXIndices = map fst $ filter snd $ zip [(0::Int)..] (V.toList activeX)
--
--      nx = size (Proxy :: Proxy (CollTraj AcX None AcU AcP NCollStages CollDeg))
--      ng = size (Proxy :: Proxy (CollOcpConstraints NCollStages CollDeg AcX AcX Bc PathC))
--      fullJac = (HMat.ident nx) HMat.=== dgdx'
--      dgdx' = M.toHMat dgdx
--
--      delRows [] [] = []
--      delRows (False:act) (_:gs) =     delRows act gs
--      delRows (True :act) (g:gs) = g : delRows act gs
--      delRows _ _ = error "delRows got length mismatch"
--
--      activeFullJac :: HMat.Matrix Double
--      activeFullJac = HMat.fromRows $ delRows (V.toList activeAll) (HMat.toRows fullJac)
--
--      activeGJac :: HMat.Matrix Double
--      activeGJac = HMat.fromRows $ delRows (V.toList activeG) (HMat.toRows dgdx')
--
--  printf "num x: %5d, active x: %5d\n" nx (V.length (V.filter id activeX))
--  printf "num g: %5d, active g: %5d\n" ng (V.length (V.filter id activeG))
----  putStrLn $ take 100 $ show dgdx
----  putStrLn "===================="
----  putStrLn $ take 100 $ show dgdx'
--  printf "dgdx': (%d, %d)\n" (HMat.rows dgdx') (HMat.cols dgdx')
--  printf "active full jac size: (%d,%d)\n" (HMat.rows activeFullJac) (HMat.cols activeFullJac)
--  printf "active    g jac size: (%d,%d)\n" (HMat.rows activeGJac) (HMat.cols activeGJac)
--  putStrLn $ "active design vars: " ++ take 100 (show activeXIndices)
--  writeFile "/home/ghorn/takeIt.txt" (saveMat (HMat.toLists activeFullJac))
--  printf "   g jac rank: %d\n" (HMat.rank activeGJac)
--  printf "   g jac rcond: %.3e\n" (HMat.rcond activeGJac)
--  printf "full jac rank: %d\n" (HMat.rank activeFullJac)
--  printf "full jac rcond: %.3e\n" (HMat.rcond activeFullJac)
--
--  let CollTraj tf' p' _stages _xf = split $ lambdaXOpt' sol
--      p = splitJV p'
--      Id tf = splitJV tf'
--  print tf
--  print p
--  return ()
