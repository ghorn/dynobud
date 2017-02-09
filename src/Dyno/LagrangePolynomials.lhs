\documentclass[a4paper,12pt]{article}
%\documentclass[printer]{gOMS2e}
%\usepackage{indentfirst}
%\usepackage[T1]{fontenc}
\usepackage{amsfonts}
\usepackage{amsmath,url}
\usepackage[left=2cm,right=2cm,top=2.5cm]{geometry}
\usepackage{graphicx}
\usepackage{algorithmic}
\usepackage{algorithm}

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }

%\newcommand{\qed}{\hfill \qedbox $\quad\quad$\\[1ex]}%
%\newcommand{\beq}{\begin{equation}}
%\newcommand{\eeq}{\end{equation}}
%\newcommand{\barr}{\begin{array}}
%\newcommand{\earr}{\end{array}}
%\newcommand{\bvec}{ \left[ \!\! \barr{cccccccccccc} }
%\newcommand{\evec}{ \earr \!\! \right] }
%\newcommand{\bmat}{ \left( \!\! \barr{ccccccc} }
%\newcommand{\emat}{ \earr \!\! \right) }
%\newcommand{\e}{\mathbf{e}}
%\renewcommand{\AA}{\mathbb{A}}
%\newcommand{\BB}{\mathbb{B}}
%\newcommand{\R}{\mathbb{R}}
%\newcommand{\N}{\mathbb{N}}
%\newcommand{\EE}{{\mathbb{E}}}
%\newcommand{\LL}{{\mathcal{L}}}
%\newcommand{\FF}{{\mathbb{F}}}
%\newcommand{\XX}{{\mathbb{X}}}
%\newcommand{\Id}{\mathbb{I}}
%\newcommand{\conv}{\mathbf{conv}}
%\newcommand{\nonneg}{\mathbf{nonneg}}
%\newcommand{\quadand}{\quad\mbox{ and }\quad}

\newcommand{\dpartial}[2]{\frac{\partial {#1}}{\partial {#2}}}
\newcommand{\dtotal}[2]{\frac{d {#1}}{d {#2}}}

\newcommand{\myl}{\xi}
\newcommand{\myldot}{\myl'}
%\newcommand{\pos}{\vec{r}}
%\newcommand{\rad}{l}
\newcommand{\matr}[2]{\left[\begin{array}{#1}#2\end{array}\right]}
%\newcommand{\dpartial}[2]{\frac{\partial#1}{\partial #2}}
\newcommand{\refeq}[1]{Eq.~(\ref{#1})}
\newcommand{\refsec}[1]{Sect.~\ref{#1}}
\newcommand{\reffig}[1]{Fig.~\ref{#1}}
%\newcommand{\reftab}[1]{Table \ref{#1}}
%\usepackage{epstopdf}
%\usepackage{amsmath}
%\usepackage{amssymb}

\newcommand{\taus}[3]{\frac{\tau_{#1}-\tau_{#2}}{\tau_{#3}-\tau_{#2}}}
\newcommand{\taud}[3]{\frac{1}{\tau_{#3}-\tau_{#2}}}

\begin{document}
\sffamily
\begin{center}
\begin{LARGE}
{\bf Direct Collocation}\\
\vspace*{0.3cm}
\end{LARGE}\end{center}
\begin{center}
\begin{Large}
Greg Horn
\end{Large}

\end{center}
\vspace*{1cm}

\section{Lagrange Interpolation}
The Lagrange interpolating polynomial of degree $D$, defined by nodes $x_0..x_D$ at timepoints $\tau_0..\tau_D$ is
\begin{equation}
\begin{aligned}
x(\tau) &= \sum_{j=0}^D \myl_j(\tau) x_j
\end{aligned}
\label{eq:lagrange_interp_poly}
\end{equation}
where
\begin{equation}
\myl_j(\tau) = \prod_{k=0,k \neq j}^D\frac{\tau-\tau_k}{\tau_j-\tau_k}
\end{equation}

\begin{code}
{-# OPTIONS_GHC -Wall #-}

module Dyno.LagrangePolynomials
       ( lagrangeDerivCoeffs, lagrangeXis, runComparison
       , interpolate
       ) where

import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Linear ( Additive, (^*), sumV )

import Casadi.SX ( SX )
import Casadi.DM ( DM, dnonzeros )
import Casadi.Matrix ( sym, gradient, densify, toFunction )
import Casadi.Function ( callDM )

import Dyno.TypeVecs


interpolate :: (Additive f, Fractional a) => Vec deg a -> Vec deg (f a) -> a -> f a
interpolate taus0 xs0 tau1 = sumV [x ^* (lagrangeXis taus0' tau1 k) | (k,x) <- zip [0..] xs0']
  where
    taus0' = F.toList taus0
    xs0' = F.toList xs0


lagrangeXis :: Fractional a => [a] -> a -> Int -> a
lagrangeXis taus tau j =
  product [(tau - tk) / (tj - tk) | k <- [0..deg]
                                  , k /= j
                                  , let tk = taus !! k
                                        tj = taus !! j
                                  ]
  where
    deg = length taus - 1

\end{code}

The derivative of this polynomial on an intermediate point is given by
\begin{equation}
x'(\tau) = \sum_{j=0}^D \myldot_j(\tau) x_j
\label{eq:lagrange_interp_poly_deriv}
\end{equation}

Written out for $D=3$, this looks like

\begin{align}
x(\tau) = & x_0 \taus{}{1}{0} \taus{}{2}{0} \taus{}{3}{0} + \\
          & x_1 \taus{}{0}{1} \taus{}{2}{1} \taus{}{3}{1} + \\
          & x_2 \taus{}{0}{2} \taus{}{1}{2} \taus{}{3}{2} + \\
          & x_3 \taus{}{0}{3} \taus{}{1}{3} \taus{}{2}{3}
\end{align}

Evaluating this at an interpolation node $\tau_m$ eliminates all terms except $m=k$, giving
\begin{equation}
x(\tau_m) = x_m
\end{equation}

The derivative for $D=3$ is

\begin{align}
x'(\tau) = &x_0 (\taud{}{1}{0} \taus{}{2}{0} \taus{}{3}{0}
               + \taus{}{1}{0} \taud{}{2}{0} \taus{}{3}{0}
               + \taus{}{1}{0} \taus{}{2}{0} \taud{}{3}{0}) + \\
           &x_1 (\taud{}{0}{1} \taus{}{2}{1} \taus{}{3}{1}
               + \taus{}{0}{1} \taud{}{2}{1} \taus{}{3}{1}
               + \taus{}{0}{1} \taus{}{2}{1} \taud{}{3}{1}) + \\
           &x_2 (\taud{}{0}{2} \taus{}{1}{2} \taus{}{3}{2}
               + \taus{}{0}{2} \taud{}{1}{2} \taus{}{3}{2}
               + \taus{}{0}{2} \taus{}{1}{2} \taud{}{3}{2}) + \\
           &x_3 (\taud{}{0}{3} \taus{}{1}{3} \taus{}{2}{3}
               + \taus{}{0}{3} \taud{}{1}{3} \taus{}{2}{3}
               + \taus{}{0}{3} \taus{}{1}{3} \taud{}{2}{3})
\end{align}

evaluating this at $\tau_0$ gives:

\begin{align}
x'(\tau_0) = &x_0 (\taud{0}{1}{0} \taus{0}{2}{0} \taus{0}{3}{0}
                 + \taus{0}{1}{0} \taud{0}{2}{0} \taus{0}{3}{0}
                 + \taus{0}{1}{0} \taus{0}{2}{0} \taud{0}{3}{0}) +\\
             &x_1 (\taud{0}{0}{1} \taus{0}{2}{1} \taus{0}{3}{1}
                 + \taus{0}{0}{1} \taud{0}{2}{1} \taus{0}{3}{1}
                 + \taus{0}{0}{1} \taus{0}{2}{1} \taud{0}{3}{1}) +\\
             &x_2 (\taud{0}{0}{2} \taus{0}{1}{2} \taus{0}{3}{2}
                 + \taus{0}{0}{2} \taud{0}{1}{2} \taus{0}{3}{2}
                 + \taus{0}{0}{2} \taus{0}{1}{2} \taud{0}{3}{2}) +\\
             &x_3 (\taud{0}{0}{3} \taus{0}{1}{3} \taus{0}{2}{3}
                 + \taus{0}{0}{3} \taud{0}{1}{3} \taus{0}{2}{3}
                 + \taus{0}{0}{3} \taus{0}{1}{3} \taud{0}{2}{3})
\end{align}
which simplifies to
\begin{align}
x'(\tau_0) = &x_0 (\taud{0}{1}{0}
                 + \taud{0}{2}{0}
                 + \taud{0}{3}{0}) +\\
             &x_1 (\taud{0}{0}{1} \taus{0}{2}{1} \taus{0}{3}{1}) +\\
             &x_2 (\taud{0}{0}{2} \taus{0}{1}{2} \taus{0}{3}{2}) +\\
             &x_3 (\taud{0}{0}{3} \taus{0}{1}{3} \taus{0}{2}{3}) \\
= & x_0 C_{0,0} + x_1 C_{0,1} + x_2 C_{0,2} + x_3 C_{0,3}
\end{align}

evaluating this at $\tau_1$ gives:
\begin{align}
x'(\tau_0) = &x_0 (\taud{1}{1}{0} \taus{1}{2}{0} \taus{1}{3}{0}) +\\
             &x_1 (\taud{1}{0}{1}
                 + \taud{1}{2}{1}
                 + \taud{1}{3}{1}) +\\
             &x_2 (\taus{1}{0}{2} \taud{1}{1}{2} \taus{1}{3}{2}) +\\
             &x_3 ( \taus{1}{0}{3} \taud{1}{1}{3} \taus{1}{2}{3}) \\
= & x_0 C_{1,0} + x_1 C_{1,1} + x_2 C_{1,2} + x_3 C_{1,3}
\end{align}

evaluating this at $\tau_2$ gives:
\begin{align}
x'(\tau_2) = &x_0 (\taus{2}{1}{0} \taud{2}{2}{0} \taus{2}{3}{0}) + \\
             &x_1 (\taus{2}{0}{1} \taud{2}{2}{1} \taus{2}{3}{1}) + \\
             &x_2 (\taud{2}{0}{2}
                 + \taud{2}{1}{2}
                 + \taud{2}{3}{2}) + \\
             &x_3 (\taus{2}{0}{3} \taus{2}{1}{3} \taud{2}{2}{3}) \\
= & x_0 C_{2,0} + x_1 C_{2,1} + x_2 C_{2,2} + x_3 C_{2,3}
\end{align}

evaluating this at $\tau_3$ gives:
\begin{align}
x'(\tau_3) = &x_0 (\taus{3}{1}{0} \taus{3}{2}{0} \taud{3}{3}{0}) + \\
             &x_1 (\taus{3}{0}{1} \taus{3}{2}{1} \taud{3}{3}{1}) + \\
             &x_2 (\taus{3}{0}{2} \taus{3}{1}{2} \taud{3}{3}{2}) + \\
             &x_3 (\taud{3}{0}{3} 
                 + \taud{3}{1}{3} 
                 + \taud{3}{2}{3}) \\
= & x_0 C_{3,0} + x_1 C_{3,1} + x_2 C_{3,2} + x_3 C_{3,3}
\end{align}

The general formula for $C_{j,k}$ is
\begin{equation}
C_{j,k} =
\begin{cases}
\sum_{i=0,i\ne k}^D{\frac{1}{\tau_k-\tau_i}} & j=k \\
\frac{1}{\tau_k-\tau_j} \prod_{i=0,i\ne j,i\ne k}^D{\frac{\tau_j-\tau_i}{\tau_k-\tau_i}}  & j \ne k
\end{cases}
\end{equation}

\begin{code}
lagrangeDerivCoeffs :: (Dim deg, Fractional a) => Vec deg a -> Vec deg (Vec deg a)
lagrangeDerivCoeffs taus' = mkVec' [mkVec' [cjk j k | k <- [0..deg]] | j <- [0..deg]]
  where
    taus = unVec taus'
    deg = V.length taus - 1

    cjk j k
      | j == k = sum [ 1/(tau_k - taus V.! i) | i <- [0..deg], i /= k ]
      | otherwise =
        1 / (tau_k - tau_j) *
        product [ (tau_j - tau_i)/(tau_k - tau_i)
                | i <- [0..deg], i /= j, i /= k, let tau_i = taus V.! i
                ]
      where
        tau_k = taus V.! k
        tau_j = taus V.! j
\end{code}

Testing code:
\begin{code}
syms :: String -> Int -> IO [SX]
syms name n = mapM (\k -> sym (name ++ show k) 1 1) $ take n [(0::Int)..]

runComparison :: IO ()
runComparison = do
  let deg = 6
      sampleTaus = take (deg+1) [0.1,0.2..]
      sampleTaus' = map realToFrac sampleTaus

  tau <- sym "t" 1 1
  taus <- syms "t" (deg + 1)

  let zs :: [SX]
      zs = map (lagrangeXis taus tau) [0..deg]
      inputs = tau : taus
      zdot = map (`gradient` tau) zs
  zdotAlg <- toFunction "zdotAlg" (V.fromList inputs) (V.fromList zdot) M.empty

  --mapM_ print zdot'
  
  putStrLn "numeric:"
  vals' <- V.mapM (\tau_i -> callDM zdotAlg (V.fromList (tau_i : sampleTaus'))) (V.fromList sampleTaus')
  let d2d :: DM -> Double
      d2d x = case V.toList (dnonzeros (densify x)) of
        [y] -> y
        ys -> error $ "d2d: need length 1, got length " ++ show (length ys)

      vals = fmap (fmap d2d) vals'
  V.mapM_ print vals
  
  putStrLn "\nnumeric difference:"
  let cmp :: V.Vector Double -> V.Vector Double -> IO ()
      cmp v1s v2s = print $ V.zipWith (-) v1s v2s
      f :: Dim n => Vec n Double -> IO ()
      f st = V.zipWithM_ cmp vals $ fmap unVec $ unVec $ lagrangeDerivCoeffs st
  reifyVector (V.fromList sampleTaus) f
  return ()

\end{code}

\end{document}
