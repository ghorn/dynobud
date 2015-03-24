{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language PolyKinds #-}

module Dyno.OcpHomotopy
       ( runOcpHomotopy
       ) where

import Data.Vector ( Vector )
import qualified Data.Traversable as T

import Casadi.MX ( MX )

import Dyno.Ocp
import Dyno.Vectorize ( Vectorize, Id(..), None(..) )
import Dyno.View.View ( View(..), J, JTuple(..), JNone(..) )
import Dyno.View.JV ( JV, catJV, catJV' )
import Dyno.View.Viewable ( Viewable )
import Dyno.TypeVecs ( Dim )
import Dyno.Solvers ( Solver )
import Dyno.Nlp ( Nlp(..), NlpOut(..) )
import Dyno.NlpUtils ( HomotopyParams(..), solveNlp, solveNlpHomotopy )
import Dyno.DirectCollocation.Types ( CollTraj(..), CollOcpConstraints )
import Dyno.DirectCollocation.Formulate ( CollProblem(..), makeCollProblem )
import Dyno.DirectCollocation.Quadratures ( QuadratureRoots )


tupleToCollTraj ::
  forall x z u p n deg a
  . ( Dim deg, Dim n, Viewable a, Vectorize x, Vectorize z, Vectorize u, Vectorize p )
  => JTuple (CollTraj x z u None n deg) (JV p) a
  -> J (CollTraj x z u p n deg) a
tupleToCollTraj (JTuple x0 p) = cat x1
  where
    x1 :: CollTraj x z u p n deg a
    x1 = CollTraj tf p stages0 xf

    CollTraj tf _ stages0 xf = split x0

collTrajToTuple ::
  forall x z u p n deg a .
  ( Viewable a
  , Vectorize x, Vectorize z, Vectorize u, Vectorize p
  , Dim deg, Dim n )
  => J (CollTraj x z u p n deg) a
  -> JTuple (CollTraj x z u None n deg) (JV p) a
collTrajToTuple x0 = JTuple (cat x1) pfp
  where
    x1 :: CollTraj x z u None n deg a
    x1 = CollTraj tf (catJV' None) stages0 xf

    CollTraj tf pfp stages0 xf = split x0


convertNlp ::
  forall x z u p r c h n deg a
  . ( Viewable a
    , Vectorize x, Vectorize z, Vectorize u, Vectorize p
    , Dim deg, Dim n
    )
  => Nlp (CollTraj x z u p    n deg) JNone  (CollOcpConstraints x r c h n deg) a
  -> Nlp (CollTraj x z u None n deg) (JV p) (CollOcpConstraints x r c h n deg) a
convertNlp nlp0 = nlp
  where
    nlp = Nlp { nlpX0 = x0
              , nlpBG = nlpBG nlp0
              , nlpP = fp0
              , nlpFG = fg
              , nlpBX = bx
              , nlpLamX0 = fmap ((\(JTuple ret _) -> ret) . collTrajToTuple) (nlpLamX0 nlp0)
              , nlpLamG0 = nlpLamG0 nlp0
              , nlpScaleF = nlpScaleF nlp0
              , nlpScaleX = fmap ((\(JTuple ret _) -> ret) . collTrajToTuple) (nlpScaleX nlp0)
              , nlpScaleG = nlpScaleG nlp0
              }
    fg :: J (CollTraj x z u None n deg) a -> J (JV p) a
          -> (J (JV Id) a, J (CollOcpConstraints x r c h n deg) a)
    fg x fp = (f,g')
      where
        (f,g') = nlpFG nlp0 x' (cat JNone)
        x' :: J (CollTraj x z u p n deg) a
        x' = tupleToCollTraj (JTuple x fp)

    JTuple x0 fp0 = collTrajToTuple (nlpX0 nlp0)
    JTuple bx   _ = collTrajToTuple (nlpBX nlp0)



runOcpHomotopy ::
  forall x z u p r o c h q n deg t
  . ( Dim n, Dim deg
    , Vectorize x, Vectorize z, Vectorize u, Vectorize p
    , Vectorize r, Vectorize o, Vectorize c, Vectorize h, Vectorize q
    , T.Traversable t )
  => Double -> HomotopyParams
  -> OcpPhase x z u p r o c h q
  -> J (CollTraj x z u p n deg) (Vector Double)
  -> QuadratureRoots -> Bool -> Bool -> Solver -> Solver -> p Double -> t (p Double)
  -> (CollProblem x z u p r o c h q n deg
      -> IO ([String] -> J (CollTraj x z u p n deg) (Vector Double) -> IO Bool)
     )
  -> IO (t (NlpOut (CollTraj x z u p n deg)
                   (CollOcpConstraints x r c h n deg)
                   (Vector Double)))
runOcpHomotopy step0 homotopyParams ocpHomotopy guess roots useStartupCallback useHomotopyCallback
  startupSolver homotopySolver param0 nominalParams makeCallback = do
  cp0 <- makeCollProblem roots ocpHomotopy
  callbackHeh <- makeCallback cp0
  let nlp0 = cpNlp cp0
  let nlpHomotopy :: Nlp
                     (CollTraj x z u None n deg)
                     (JV p)
                     (CollOcpConstraints x r c h n deg)
                     MX
      nlpHomotopy = (convertNlp nlp0) {nlpP = catJV param0}

  let callback :: [String]
                  -> J (CollTraj x z u p n deg) (Vector Double)
                  -> IO Bool
      callback moarMsgs traj = callbackHeh moarMsgs traj

      scb = if useStartupCallback then Just (callback ["homotopy startup solve"]) else Nothing

  putStrLn "running startup solver..."
  (msg0,opt0') <- solveNlp startupSolver (nlp0 { nlpX0 = guess }) scb

  opt0 <- case msg0 of
    Left msg' -> error msg'
    Right _ -> return opt0'

  let homoGuessX :: J (CollTraj x z u None n deg) (Vector Double)
      JTuple homoGuessX _ = collTrajToTuple $ xOpt opt0
      JTuple homoGuessLX _ = collTrajToTuple $ lambdaXOpt opt0
      homoGuessLG :: J (CollOcpConstraints x r c h n deg) (Vector Double)
      homoGuessLG = lambdaGOpt opt0

      pFinals :: t (J (JV p) (Vector Double))
      pFinals = fmap catJV nominalParams

      homoCallback :: J (JTuple (CollTraj x z u None n deg) (JV p)) (Vector Double)
                      -> IO Bool
      homoCallback traj0 = callback [ "homotopy stepping"
                                    ] (tupleToCollTraj (split traj0))

  putStrLn "\ninitial solve done, starting homotopy steps"
  let hcb = if useHomotopyCallback then Just homoCallback else Nothing
      pscale :: Maybe (J (JV p) (Vector Double))
      pscale = fmap catJV (ocpPScale ocpHomotopy)
  opt1s <- solveNlpHomotopy step0 homotopyParams
           homotopySolver
           pscale
           (nlpHomotopy { nlpX0    = homoGuessX
                        , nlpLamX0 = Just homoGuessLX
                        , nlpLamG0 = Just homoGuessLG
                        })
           pFinals
           hcb Nothing

  let f :: NlpOut (JTuple (CollTraj x z u None n deg) (JV p))
                  (CollOcpConstraints x r c h n deg)
                  (Vector Double)
           -> NlpOut (CollTraj x z u p n deg)
                     (CollOcpConstraints x r c h n deg)
                     (Vector Double)
      f nlpOut =
        NlpOut
        { fOpt = fOpt nlpOut
        , xOpt = g (xOpt nlpOut)
        , gOpt = gOpt nlpOut
        , lambdaXOpt = g (lambdaXOpt nlpOut)
        , lambdaGOpt = lambdaGOpt nlpOut
        }
        where
          g :: J (JTuple (CollTraj x z u None n deg) (JV p)) (Vector Double)
               -> J (CollTraj x z u p n deg) (Vector Double)
          g = tupleToCollTraj . split

      ret :: t (NlpOut
               (CollTraj x z u p n deg)
               (CollOcpConstraints x r c h n deg)
               (Vector Double))
      ret = fmap f opt1s

  return ret
