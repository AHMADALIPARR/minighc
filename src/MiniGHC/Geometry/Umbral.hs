{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Hand-rolled Umbral Calculus.
--
-- Core notions: polynomial sequences, the umbral operator,
-- forward difference, and Sheffer sequences (Appell / binomial type).

module MiniGHC.Geometry.Umbral
  ( Poly
  , umbral
  , forwardDiff
  , appell
  , binomialType
  , evalPoly
  ) where

import Data.List (foldl')

-- | Dense polynomial, lowest degree first.
type Poly = [Rational]

-- | Umbral substitution: replace x^n by a sequence a_n.
umbral :: [Rational] -> Poly -> Rational
umbral aseq p = sum (zipWith (*) p (aseq ++ repeat 0))

-- | Forward difference operator Δp(x) = p(x+1) - p(x).
forwardDiff :: Poly -> Poly
forwardDiff p = zipWith (-) (shift p) p
  where
    shift q = 0 : q   -- approximate; exact shift needs binomial expansion

-- | Appell sequence: p_n'(x) = n p_{n-1}(x).
appell :: Int -> Poly
appell 0 = [1]
appell n = integrate (fromIntegral n * head (appell (n-1)) : tail (appell (n-1)))
  where
    integrate q = 0 : zipWith (/) q [1..]

-- | Binomial-type sequence (basic example: falling factorials).
binomialType :: Int -> Poly
binomialType n = falling n
  where
    falling 0 = [1]
    falling k = multiply [0,1] (falling (k-1))   -- x(x-1)... rough
    multiply a b = foldl' addShift [] (zip [0..] a)
      where
        addShift acc (i, c) = zipWith (+) (acc ++ repeat 0) (replicate i 0 ++ map (*c) b)

evalPoly :: Poly -> Rational -> Rational
evalPoly p x = foldr (\c acc -> c + x * acc) 0 (reverse p)
