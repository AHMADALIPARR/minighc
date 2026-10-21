{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

module MiniGHC.Geometry.Umbral
  ( Poly, umbral, forwardDiff, appell, appellSeq, binomialType, evalPoly ) where
import MiniGHC.Polynomial (Poly)
import qualified MiniGHC.Polynomial as P
-- | The umbral functional of a moment sequence a, applied
-- to a polynomial in the monomial basis: L(x^k) = a_k.
-- Moments beyond the supplied list are taken to be 0.
umbral :: [Rational] -> Poly -> Rational
umbral aseq p = sum (zipWith (*) p (aseq ++ repeat 0))
-- Compose with x+1 by Horner's rule, then subtract p(x).
forwardDiff :: Poly -> Poly
forwardDiff p = P.sub (foldr (\c q -> P.add [c] (P.mul [1,1] q)) [] p) p
-- | The Appell sequence of a moment sequence a (a_0 = 1):
-- p_n(x) = sum_k binom(n,k) a_{n-k} x^k. Appell sequences
-- are characterized by p_0 = 1, p_n(0) = a_n, and the
-- derivative rule p'_{n+1} = (n+1) p_n, which holds for
-- this construction for every moment sequence.
appellSeq :: [Rational] -> Int -> Poly
appellSeq a n
  | n < 0 = error "negative polynomial degree"
  | otherwise = [ binom n k * at (n - k) | k <- [0..n] ]
  where
    at i = case drop i a of (x:_) -> x; [] -> 0
    binom m k = fromIntegral (product [m-k+1..m]) / fromIntegral (product [1..k])

-- | The monomial Appell sequence (moments 1, 0, 0, ...):
-- p_n(x) = x^n, the special case of 'appellSeq'.
appell :: Int -> Poly
appell = appellSeq (1 : repeat 0)
binomialType :: Int -> Poly
binomialType n | n < 0 = error "negative polynomial degree"
              | otherwise = foldl P.mul [1] [[negate (fromIntegral k),1] | k <- [0..n-1]]
evalPoly :: Poly -> Rational -> Rational
evalPoly = P.eval
