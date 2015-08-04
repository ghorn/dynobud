{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Dyno.DirectCollocation.Export
       ( Export(..)
       , ExportConfig(..)
       , exportTraj
       , exportTraj'
         -- * matlab specific
       , matlabParam
       , matlabTraj
         -- * python specific
       , PythonExporter
       , runPythonExporter
       , pythonParam
       , pythonTraj
       , write
       ) where

import Control.Monad ( unless )
import Data.List ( unzip6, intercalate )
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

data ExportConfig =
  ExportConfig
  { ecMatlabVariableName :: String
  , ecPythonVariableName :: String
  }

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
  => ExportConfig
  -> CollProblem x z u p r o c h q qo po fp n deg
  -> fp Double
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO Export
exportTraj = exportTraj' (Nothing :: Maybe ([String], None Double))


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
  => Maybe ([String], e Double)
  -> ExportConfig
  -> CollProblem x z u p r o c h q qo po fp n deg
  -> fp Double
  -> NlpOut (CollTraj x z u p n deg) (CollOcpConstraints x r c h n deg) (Vector Double)
  -> IO Export
exportTraj' mextra exportConfig cp fp nlpOut = do
  let matlabRetName = ecMatlabVariableName exportConfig
      pyRetName = ecPythonVariableName exportConfig

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

      matlabOut :: String
      matlabOut = unlines $
        matlabTraj (matlabRetName ++ ".diffStatesFull") fullXs ++
        matlabTraj (matlabRetName ++ ".diffStates") xs ++
        matlabTraj (matlabRetName ++ ".diffStateDerivs") xdots ++
        matlabTraj (matlabRetName ++ ".algVars") zs ++
        matlabTraj (matlabRetName ++ ".controls") us ++
        matlabTraj (matlabRetName ++ ".outputs") os ++
        matlabTraj (matlabRetName ++ ".pathConstraints") hs ++
        matlabTraj (matlabRetName ++ ".plotOutputs") pos ++
        matlabTraj (matlabRetName ++ ".quadratureStatesFull") qsFull ++
        matlabTraj (matlabRetName ++ ".quadratureStates") qs ++
        matlabTraj (matlabRetName ++ ".quadratureStateDerivs") qds ++
        matlabParam (matlabRetName ++ ".params") (splitJV p') ++
        ( case mextra of
            Nothing -> []
            Just (names,extra) -> matlabParam (intercalate "." (matlabRetName : names)) extra
        ) ++
        matlabParam (matlabRetName ++ ".lagrangeMultipliers.params") (splitJV lagP') ++
        matlabParam (matlabRetName ++ ".lagrangeMultipliers.bc") (splitJV lagBc') ++
        matlabParam (matlabRetName ++ ".finalQuadratureStates") finalQuads ++
        [ matlabRetName ++ ".lagrangeMultipliers.T = " ++ show (unId (splitJV lagTf')) ++ ";"
        , ""
        , matlabRetName ++ ".tx = " ++ show xTimes ++ ";"
        , matlabRetName ++ ".tzuo = " ++ show zuoTimes ++ ";"
        , matlabRetName ++ ".T = " ++ show tf ++ ";"
        , matlabRetName ++ ".N = " ++ show n ++ ";"
        , matlabRetName ++ ".deg = " ++ show (reflectDim (Proxy :: Proxy deg)) ++ ";"
        , matlabRetName ++ ".collocationRoots = '" ++ show (cpRoots cp) ++ "';"
        ]

      runRet :: State PythonExporter ()
      runRet = do
        write "import numpy"
        write ""
        write $ pyRetName ++ " = {}"
        pythonTraj pyRetName ["diffStatesFull"] fullXs
        pythonTraj pyRetName ["diffStates"] xs
        pythonTraj pyRetName ["diffStateDerivs"] xdots
        pythonTraj pyRetName ["algVars"] zs
        pythonTraj pyRetName ["controls"] us
        pythonTraj pyRetName ["outputs"] os
        pythonTraj pyRetName ["pathConstraints"] hs
        pythonTraj pyRetName ["plotOutputs"] pos
        pythonTraj pyRetName ["quadratureStatesFull"] qsFull
        pythonTraj pyRetName ["quadratureStates"] qs
        pythonTraj pyRetName ["quadratureStateDerivs"] qds
        pythonParam pyRetName ["params"] (splitJV p')
        case mextra of
          Nothing -> return ()
          Just (names,extra) -> pythonParam pyRetName names extra
        pythonParam pyRetName ["lagrangeMultipliers","params"] (splitJV lagP')
        pythonParam pyRetName ["lagrangeMultipliers","bc"] (splitJV lagBc')
        pythonParam pyRetName ["finalQuadratureStates"] finalQuads
        putVal pyRetName ["lagrangeMultipliers","T"] (show (unId (splitJV lagTf')))
        write ""
        putVal pyRetName ["tx"] (npArray (show xTimes))
        putVal pyRetName ["tzuo"] (npArray (show zuoTimes))
        putVal pyRetName ["T"] (show tf)
        putVal pyRetName ["N"] (show n)
        putVal pyRetName ["deg"] (show (reflectDim (Proxy :: Proxy deg)))
        putVal pyRetName ["collocationRoots"] ("'" ++ show (cpRoots cp) ++ "'")

  return $ Export
    { exportMatlab = matlabOut
    , exportPython = unlines (runPythonExporter runRet)
    }

runPythonExporter :: State PythonExporter () -> [String]
runPythonExporter action = reverse pythonOut
  where
    PythonExporter (_, pythonOut) = State.execState action (PythonExporter (S.empty, []))

npArray :: String -> String
npArray str = "numpy.array(" ++ str ++ ")"

toDub :: Getter (xzu Double) -> xzu Double -> Double
toDub (GetDouble f) = f
toDub (GetFloat f) = realToFrac . f
toDub (GetInt f) = realToFrac . f
toDub (GetBool f) = fromIntegral . fromEnum . f
toDub GetSorry = const (read "NaN")


pythonParam :: forall p . (Vectorize p, Lookup (p Double))
              => String -> [String] -> p Double -> State PythonExporter ()
pythonParam pyRetName topNames p = mapM_ pyParam at'
  where
    pyParam :: ([String], (p Double -> Double)) -> State PythonExporter ()
    pyParam (name, get) = putVal pyRetName (topNames ++ name) (show (get p))

    at' :: [([String], p Double -> Double)]
    at' = map (\(fn,g,_) -> (fn, toDub g)) $ flatten' $ accessors (fill (0 :: Double))

pythonTraj :: forall x . (Vectorize x, Lookup (x Double))
              => String -> [String] -> [x Double] -> State PythonExporter ()
pythonTraj pyRetName topNames xs = mapM_ pyArray at'
  where
    pyArray :: ([String], (x Double -> Double)) -> State PythonExporter ()
    pyArray (name, get) = putVal pyRetName (topNames ++ name) (npArray (show (map get xs)))

    at' :: [([String], x Double -> Double)]
    at' = map (\(fn,g,_) -> (fn, toDub g)) $ flatten' $ accessors (fill (0 :: Double))


matlabParam :: forall p . (Vectorize p, Lookup (p Double)) => String -> p Double -> [String]
matlabParam topName p = map (uncurry mlParam) at
  where
    mlParam :: String -> (p Double -> Double) -> String
    mlParam name get = topName ++ "." ++ name ++ " = " ++ show (get p) ++ ";"

    at :: [(String, p Double -> Double)]
    at = map (\(fn,g,_) -> (fn, toDub g)) $ flatten $ accessors (fill (0 :: Double))

matlabTraj :: forall x . (Vectorize x, Lookup (x Double)) => String -> [x Double] -> [String]
matlabTraj topName xs = map (uncurry mlArray) at
  where
    mlArray :: String -> (x Double -> Double) -> String
    mlArray name get =
      topName ++ "." ++ name ++ " = " ++ show (map get xs) ++ ";"

    at :: [(String, x Double -> Double)]
    at = map (\(fn,g,_) -> (fn, toDub g)) $ flatten $ accessors (fill (0 :: Double))

data PythonExporter = PythonExporter (S.Set [String], [String])

pyname :: String -> [String] -> String
pyname topName xs = topName ++ concatMap (\x -> "['" ++ x ++ "']") xs

putNameIfMissing :: String -> [String] -> State PythonExporter ()
putNameIfMissing _ [] = return ()
putNameIfMissing topName name = do
  PythonExporter (set0, _) <- State.get
  unless (S.member name set0) $ do
    putNameIfMissing topName (init name)
    PythonExporter (set1, out1) <- State.get
    State.put $ PythonExporter (S.insert name set1, (pyname topName name ++ " = {}") : out1)

write :: String -> State PythonExporter ()
write str = do
  PythonExporter (set0, outs0) <- State.get
  State.put $ PythonExporter (set0, str:outs0)

putVal :: String -> [String] -> String -> State PythonExporter ()
putVal topName name val = do
  putNameIfMissing topName name
  write (pyname topName name ++ " = " ++ val)
