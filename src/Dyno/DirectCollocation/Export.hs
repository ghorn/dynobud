{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Export
       ( Export(..)
       , exportTraj
       , exportTraj'
       ) where

import Control.Monad ( unless )
import Data.List ( unzip6 )
import Data.Proxy ( Proxy(..) )
import Linear.V ( Dim(..) )
import Data.Vector ( Vector )
import qualified Data.Foldable as F
import Control.Monad.State.Lazy ( State )
import qualified Control.Monad.State.Lazy as State
import qualified Data.Set as S

import Accessors ( Lookup, Getter(..), flatten, flatten', accessors )

import Dyno.Nlp ( NlpOut(..) )
import Dyno.TypeVecs ( Vec )
import Dyno.Vectorize ( Vectorize, Id(..), None(..), fill )
import Dyno.View.View ( View(..) )
import Dyno.View.JV ( splitJV, catJV )
import Dyno.DirectCollocation.Formulate ( CollProblem(..) )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollOcpConstraints(..)
                                    , StageOutputs(..), Quadratures(..)
                                    , getXzus'
                                    )
import Dyno.DirectCollocation.Quadratures ( timesFromTaus )

data Export =
  Export
  { exportMatlab :: String
  , exportPython :: String
  }

exportTraj ::
  forall x z u p fp r o c h q qo po n deg
  . ( Lookup (x Double), Vectorize x
    , Lookup (z Double), Vectorize z
    , Lookup (u Double), Vectorize u
    , Lookup (o Double), Vectorize o
    , Lookup (p Double), Vectorize p
    , Lookup (c Double), Vectorize c
    , Vectorize r
    , Lookup (fp Double), Vectorize fp
    , Lookup (h Double), Vectorize h
    , Lookup (q Double), Vectorize q
    , Lookup (po Double), Vectorize po
    , Lookup (qo Double), Vectorize qo
    , Dim n, Dim deg
    )
  => CollProblem x z u p r o c h q qo po fp n deg
  -> fp Double
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO Export
exportTraj = exportTraj' (Nothing :: Maybe (String, None Double))


-- | this version takes optional user data
exportTraj' ::
  forall x z u p fp r o c h q qo po n deg e
  . ( Lookup (x Double), Vectorize x
    , Lookup (z Double), Vectorize z
    , Lookup (u Double), Vectorize u
    , Lookup (o Double), Vectorize o
    , Lookup (p Double), Vectorize p
    , Lookup (c Double), Vectorize c
    , Vectorize r
    , Lookup (fp Double), Vectorize fp
    , Lookup (h Double), Vectorize h
    , Lookup (q Double), Vectorize q
    , Lookup (po Double), Vectorize po
    , Lookup (qo Double), Vectorize qo
    , Dim n, Dim deg
    , Lookup (e Double), Vectorize e
    )
  => Maybe (String, e Double)
  -> CollProblem x z u p r o c h q qo po fp n deg
  -> fp Double
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO Export
exportTraj' mextra cp fp nlpOut = do
  let ct@(CollTraj tf' p' _ _) = split (xOpt nlpOut)
      CollTraj lagTf' lagP' _ _ = split (lambdaXOpt nlpOut)
      lagBc' = coBc $ split (lambdaGOpt nlpOut)

  (_, outs, finalQuads) <- cpHellaOutputs cp (cat ct) (catJV fp)
  let _ = outs :: Vec n (StageOutputs x o h q qo po deg Double)
      _ = finalQuads :: Quadratures q qo Double

  let taus :: Vec deg Double
      taus = cpTaus cp
      Id tf = splitJV tf'

      n = reflectDim (Proxy :: Proxy n)

      times :: Vec n (Double, Vec deg Double)
      times = timesFromTaus 0 taus dt
        where
          dt = tf / fromIntegral n

      xTimes = concatMap (\(t0,ts) -> t0 : F.toList ts) (F.toList times) ++ [tf]
      zuoTimes = concatMap (\(_,ts) -> F.toList ts) (F.toList times)

      xss :: Vec n (x Double, Vec deg (x Double))
      xf :: x Double
      zss :: Vec n (Vec deg (z Double))
      uss :: Vec n (Vec deg (u Double))
      ((xss,xf), zss, uss) = getXzus' ct

      fullXs :: [x Double]
      fullXs = concatMap (\(x0, xs') -> x0 : F.toList xs') (F.toList xss) ++ [xf]

      xs :: [x Double]
      xs = concatMap (F.toList . snd) (F.toList xss)

      zs :: [z Double]
      zs = concatMap F.toList (F.toList zss)

      us :: [u Double]
      us = concatMap F.toList (F.toList uss)

      os :: [o Double]
      xdots :: [x Double]
      hs :: [h Double]
      -- drop the interpolated value
      os = map splitJV os'
      xdots = map splitJV xdots'
      hs = map splitJV hs'
      pos = map splitJV pos'
      (os', xdots', hs', pos', _, _) = unzip6 $ F.concatMap (F.toList . soVec) outs
      qsFull :: [Quadratures q qo Double]
      qsFull = fill 0 : F.concatMap toQFull outs
        where
          toQFull :: StageOutputs x o h q qo po deg Double -> [Quadratures q qo Double]
          toQFull stageOutputs = (map (\(_,_,_,_,qs',_) -> qs') (F.toList (soVec stageOutputs))) ++ [soQNext stageOutputs]

      qs :: [Quadratures q qo Double]
      qs = F.concatMap toQ outs
        where
          toQ :: StageOutputs x o h q qo po deg Double -> [Quadratures q qo Double]
          toQ stageOutputs = map (\(_,_,_,_,qs',_) -> qs') (F.toList (soVec stageOutputs))

      toQd :: StageOutputs x o h q qo po deg Double -> [Quadratures q qo Double]
      toQd stageOutputs = (map (\(_,_,_,_,_,qd) -> qd) (F.toList (soVec stageOutputs)))
      qds :: [Quadratures q qo Double]
      qds = F.concatMap toQd outs

      at :: forall xzu . (Vectorize xzu, Lookup (xzu Double)) => [(String, xzu Double -> Double)]
      at = map (\(fn,g,_) -> (fn, toDub g)) $ flatten $ accessors (fill (0 :: Double))

      at' :: forall xzu . (Vectorize xzu, Lookup (xzu Double)) => [([String], xzu Double -> Double)]
      at' = map (\(fn,g,_) -> (fn, toDub g)) $ flatten' $ accessors (fill (0 :: Double))

      toDub :: Getter (xzu Double) -> xzu Double -> Double
      toDub (GetDouble f) = f
      toDub (GetFloat f) = realToFrac . f
      toDub (GetInt f) = realToFrac . f
      toDub (GetBool f) = fromIntegral . fromEnum . f
      toDub GetSorry = const (read "NaN")

      mlArray :: String -> [xzu Double] -> String -> (xzu Double -> Double) -> String
      mlArray topName xzus name get = topName ++ "." ++ name ++ " = " ++ show (map get xzus) ++ ";"

      mlParam :: String -> pq Double -> String -> (pq Double -> Double) -> String
      mlParam topName p name get = topName ++ "." ++ name ++ " = " ++ show (get p) ++ ";"

      matlabOut :: String
      matlabOut = unlines $
        map (uncurry (mlArray "ret.diffStatesFull" fullXs)) at ++
        map (uncurry (mlArray "ret.diffStates" xs)) at ++
        map (uncurry (mlArray "ret.diffStateDerivs" xdots)) at ++
        map (uncurry (mlArray "ret.algVars" zs)) at ++
        map (uncurry (mlArray "ret.controls" us)) at ++
        map (uncurry (mlArray "ret.outputs" os)) at ++
        map (uncurry (mlArray "ret.pathConstraints" hs)) at ++
        map (uncurry (mlArray "ret.plotOutputs" pos)) at ++
        map (uncurry (mlArray "ret.quadratureStatesFull" qsFull)) at ++
        map (uncurry (mlArray "ret.quadratureStates" qs)) at ++
        map (uncurry (mlArray "ret.quadratureStateDerivs" qds)) at ++
        map (uncurry (mlParam "ret.params" (splitJV p'))) at ++
        ( case mextra of
            Nothing -> []
            Just (name,extra) -> map (uncurry (mlParam ("ret."++name) extra)) at
        ) ++
        map (uncurry (mlParam "ret.lagrangeMultipliers.params" (splitJV lagP'))) at ++
        map (uncurry (mlParam "ret.lagrangeMultipliers.bc" (splitJV lagBc'))) at ++
        map (uncurry (mlParam "ret.finalQuadratureStates" finalQuads)) at ++
        [ "ret.lagrangeMultipliers.T = " ++ show (unId (splitJV lagTf')) ++ ";"
        , ""
        , "ret.tx = " ++ show xTimes ++ ";"
        , "ret.tzuo = " ++ show zuoTimes ++ ";"
        , "ret.T = " ++ show tf ++ ";"
        , "ret.N = " ++ show n ++ ";"
        , "ret.deg = " ++ show (reflectDim (Proxy :: Proxy deg)) ++ ";"
        , "ret.collocationRoots = '" ++ show (cpRoots cp) ++ "';"
        ]

      pyArray :: String -> String -> [xzu Double] -> ([String], (xzu Double -> Double))
              -> State PyOutState ()
      pyArray topName otherName xzus (name, get) = putVal topName (otherName : name)
         (npArray (show (map get xzus)))

      npArray str = "numpy.array(" ++ str ++ ")"

      pyParam :: String -> [String] -> pq Double -> ([String], (pq Double -> Double))
              -> State PyOutState ()
      pyParam topName otherNames p (name, get) = putVal topName (otherNames ++ name) (show (get p))

      runRet :: State PyOutState ()
      runRet = do
        write "import numpy"
        write ""
        write "ret = {}"
        mapM_ (pyArray "ret" "diffStatesFull" fullXs) at'
        mapM_ (pyArray "ret" "diffStates" xs) at'
        mapM_ (pyArray "ret" "diffStateDerivs" xdots) at'
        mapM_ (pyArray "ret" "algVars" zs) at'
        mapM_ (pyArray "ret" "controls" us) at'
        mapM_ (pyArray "ret" "outputs" os) at'
        mapM_ (pyArray "ret" "pathConstraints" hs) at'
        mapM_ (pyArray "ret" "plotOutputs" pos) at'
        mapM_ (pyArray "ret" "quadratureStatesFull" qsFull) at'
        mapM_ (pyArray "ret" "quadratureStates" qs) at'
        mapM_ (pyArray "ret" "quadratureStateDerivs" qds) at'
        mapM_ (pyParam "ret" ["params"] (splitJV p')) at'
        case mextra of
          Nothing -> return ()
          Just (name,extra) -> mapM_ (pyParam "ret" [name] extra) at'
        mapM_ (pyParam "ret" ["lagrangeMultipliers","params"] (splitJV lagP')) at'
        mapM_ (pyParam "ret" ["lagrangeMultipliers","bc"] (splitJV lagBc')) at'
        mapM_ (pyParam "ret" ["finalQuadratureStates"] finalQuads) at'
        putVal "ret" ["lagrangeMultipliers","T"] (show (unId (splitJV lagTf')))
        write ""
        putVal "ret" ["tx"] (npArray (show xTimes))
        putVal "ret" ["tzuo"] (npArray (show zuoTimes))
        putVal "ret" ["T"] (show tf)
        putVal "ret" ["N"] (show n)
        putVal "ret" ["deg"] (show (reflectDim (Proxy :: Proxy deg)))
        putVal "ret" ["collocationRoots"] ("'" ++ show (cpRoots cp) ++ "'")

  let PyOutState (_, pythonOut) = State.execState runRet (PyOutState (S.empty, []))
  return $ Export
    { exportMatlab = matlabOut
    , exportPython = unlines (reverse pythonOut)
    }


data PyOutState = PyOutState (S.Set [String], [String])

pyname :: String -> [String] -> String
pyname topName xs = topName ++ concatMap (\x -> "['" ++ x ++ "']") xs

putNameIfMissing :: String -> [String] -> State PyOutState ()
putNameIfMissing _ [] = return ()
putNameIfMissing topName name = do
  PyOutState (set0, _) <- State.get
  unless (S.member name set0) $ do
    putNameIfMissing topName (init name)
    PyOutState (set1, out1) <- State.get
    State.put $ PyOutState (S.insert name set1, (pyname topName name ++ " = {}") : out1)

write :: String -> State PyOutState ()
write str = do
  PyOutState (set0, outs0) <- State.get
  State.put $ PyOutState (set0, str:outs0)

putVal :: String -> [String] -> String -> State PyOutState ()
putVal topName name val = do
  putNameIfMissing topName name
  write (pyname topName name ++ " = " ++ val)
