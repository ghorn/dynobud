{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Qps.Qp
       ( Qp(..)
       , matchesGlpkQp
       ) where

import Control.Monad ( unless )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck hiding ( Result, reason )
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic

import qualified Numeric.LinearProgramming as GLPK
import qualified Data.Vector as V
import Data.List ( intersperse )
import Data.Maybe
import Linear hiding ( vector )

import qualified Data.Foldable as F

import Dyno.Vectorize
import Dyno.TypeVecs -- ( Vec(..), mkVec', tvlength )
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Casadi.SXElement
import Dyno.Nats

import Qps.Lp

data Qp nx ng = Qp { qx0 :: Vec nx Double
                   , qbx :: Vec nx (Double, Double)
                   , qbg :: Vec ng (Double, Double)
                   , qgoffset:: Vec ng Double
                   , qobjLCoeffs :: Vec nx (Coef Double)
                   , qobjQCoeffs :: Vec nx (Vec nx (Coef Double))
                   , qjacCoeffs :: Vec ng (Vec nx (Coef Double))
                   } --deriving Show
data FQp nx ng = FQp (Qp nx ng) 
data IQp nx ng = IQp (Qp nx ng)

instance (KnownNat nx, KnownNat ng) => Arbitrary (FQp nx ng) where
  arbitrary = do
    FLp lp <- arbitrary
    objQ <- arbitrary :: Gen (Vec nx (Vec nx (Coef Double)))
    let objQCoeffs = objQ !+! adjoint objQ
    return $ FQp $ Qp { qx0 = px0 lp
                      , qbx = pbx lp
                      , qbg = pbg lp
                      , qgoffset = pgoffset lp
                      , qobjLCoeffs = pobjCoeffs lp
                      , qobjQCoeffs = objQCoeffs
                      , qjacCoeffs = pjacCoeffs lp
                      }

instance (KnownNat nx, KnownNat ng) => Arbitrary (IQp nx ng) where
  arbitrary = do
    ILp lp <- arbitrary
    objQ <- arbitrary :: Gen (Vec nx (Vec nx (Coef Double)))
    let objQCoeffs = objQ !+! adjoint objQ
    return $ IQp $ Qp { qx0 = px0 lp
                      , qbx = pbx lp
                      , qbg = pbg lp
                      , qgoffset = pgoffset lp
                      , qobjLCoeffs = pobjCoeffs lp
                      , qobjQCoeffs = objQCoeffs
                      , qjacCoeffs = pjacCoeffs lp
                      }

instance (KnownNat nx, KnownNat ng) => Show (Qp nx ng) where
  show = prettyPrintQp
instance (KnownNat nx, KnownNat ng) => Show (FQp nx ng) where
  show (FQp qp) = prettyPrintQp qp
instance (KnownNat nx, KnownNat ng) => Show (IQp nx ng) where
  show (IQp qp) = prettyPrintQp qp

solveGlpkFlp :: IO()
solveGlpkFlp = do
  FQp flp <- generate (arbitrary :: Gen(FQp D3 D2))
  let sol = solveWithGlpk flp 
  print sol
  print flp

solveWithGlpk :: (KnownNat nx, KnownNat ng) => Qp nx ng -> GLPK.Solution
solveWithGlpk qp = GLPK.simplex prob constraints bounds
  where
    -- unpack qp
    jacCoeffs :: [[Coef Double]]
    jacCoeffs = F.toList (fmap F.toList (qjacCoeffs qp))

    bgs :: [(Double,Double)]
    bgs = F.toList (qbg qp)

    bxs :: [(Double,Double)]
    bxs = F.toList (qbx qp)

    goffsets :: [Double]
    goffsets = F.toList (qgoffset qp)

    objLCoeffs :: [Double]
    objLCoeffs = map asNumber (F.toList (qobjLCoeffs qp))

    -- set up problem
    prob = GLPK.Minimize objLCoeffs

    constraints = GLPK.Dense $ zipWith3 f jacCoeffs bgs goffsets
      where
        f jcs (lb,ub) (goffset) = (map asNumber jcs) GLPK.:&: (lb-goffset,ub-goffset)

    bounds = zipWith (GLPK.:&:) [0..] bxs

matchesGlpkQp :: (KnownNat nx, KnownNat ng, NLPSolverClass nlp)
               => NlpSolverStuff nlp -> Lp nx ng -> Property
matchesGlpkQp solver lp = monadicIO $ do
  let LpNlp nlp = nlpOfLp lp
  (fopt,xopt) <- case solveWithGlpk lp of
    GLPK.Unbounded -> stop (rejected {reason = "unbounded"})
    GLPK.NoFeasible -> stop (rejected {reason = "NoFeasible"})
    GLPK.Infeasible _ -> stop (rejected {reason = "Infeasible"})
    GLPK.Undefined -> stop (failed {reason = "glpk result Undefined"})
    GLPK.Feasible _ -> stop (failed {reason = "glpk result Feasible, should be optimal"})
    GLPK.Optimal opt -> return opt
  let v@(Valid fe xe ge) = validSol lp (devectorize (V.fromList xopt)) fopt
  unless (and [fe <= 1e-6, xe <= 1e-10, ge <= 1e-10]) $
    stop (failed {reason = "glpk gave invalid solution: " ++ show v})

  (ret,nlpOut) <- run $ solveNlp solver nlp Nothing
  let xerr = maximum (map abs (zipWith (-) xopt (F.toList (xOpt nlpOut))))
      ferr = abs (fopt - fOpt nlpOut)
      v'@(Valid fe' xe' ge') = validSol lp (xOpt nlpOut) (fOpt nlpOut)

      summary = unlines
               [ "design vars"
               , " glpk: " ++ show xopt
               , " nlp: " ++ show (F.toList (xOpt nlpOut))
               , "objective"
               , " glpk: " ++ show fopt
               , " nlp: " ++ show (fOpt nlpOut)
               , ""
               ]
  case ret of
    Left "3" -> stop $ rejected {reason = "nlp solver got code 3"}
    Left code -> do
      --run $ writeFile "counterexample.py" (toPython params)
      stop $ failed {reason = "====== nlp solver failed with code " ++ show code ++ " =====\n"++summary}
    Right _ ->
      if | or [fe' > 1e-5, xe' > 1e-6, ge' > 1e-4] ->
            stop $ failed {reason = "returned invalid solution: " ++ show v'}
         | xerr <= 0.4 && ferr <= 0.4 -> stop $ succeeded {reason = "solutions match"}
         | ferr <= 1e-6 -> stop $ rejected {reason =
             "two valid solutions match objective, don't match decision vars"}
         | otherwise -> do
             --run $ writeFile "counterexample.py" (toPython params)
             stop $ failed { reason = "======== solution doesn't match glpk! ========\n" ++ summary }

prettyPrintQp :: (KnownNat nx, KnownNat ng) => Qp nx ng -> String
prettyPrintQp (Qp x0' bx' bg' goffset' objLCoeffs' objQCoeffs' jacCoeffs') =
  init $ unlines $
  [ "minimize:"
  , " " ++ myShowList (zipWith showQCoeff objQCoeffs kq) ++ " + " ++ myShowList (zipWith showCoeff objLCoeffs ks)
  , "subject to:"
  ] ++ (zipWith3 (\coeffs goff (lb,ub) -> " " ++ show lb ++ " <= " ++ myShowList (zipWith showCoeff coeffs ks) ++ " + " ++ show goff ++ " <= " ++ show ub) jacCoeffs goffset bg) ++
  ["subject to (bounds):"] ++
  zipWith (\k (lb,ub) -> " " ++ show lb ++ " <= " ++ xname k ++ " <= " ++ show ub) ks bx
  where
    --maybeOffset
    x0 = V.toList (vectorize x0')
    bx = V.toList (vectorize bx')
    bg = V.toList (vectorize bg')
    goffset = V.toList (vectorize goffset')
    objLCoeffs = V.toList (vectorize objLCoeffs')
    objQCoeffs = concat $ V.toList(vectorize (fmap (V.toList . vectorize) objQCoeffs'))
    jacCoeffs = V.toList (vectorize (fmap (V.toList . vectorize) jacCoeffs'))

    n = length x0
    --m = length bg
    ks = take n [1..]
    kq = [(i,j) | i <- ks, j <- ks]
    showCoeff (Linear x) k = Just (show x ++ "*" ++ xname k)
    showCoeff (Nonlinear x) k = Just (show x ++ "*{" ++ xname k ++ "}")
    showCoeff Zero _ = Nothing
    showQCoeff (Linear x) k = Just (xname (fst k) ++ "*" ++ show x ++ "*" ++ xname (snd k))
    showQCoeff (Nonlinear x) k = Just ("{" ++ xname (fst k) ++ "}*" ++ show x ++ "*{" ++ xname (snd k) ++ "}")
    showQCoeff Zero _ = Nothing
    fstIndex = 0 :: Int
    xname k = "x" ++ show (k + fstIndex)
    myShowList :: [Maybe String] -> String
    myShowList = concat . (intersperse " + ") . catMaybes
