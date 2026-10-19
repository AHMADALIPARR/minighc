{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

module MiniGHC.Geometry.Umbral
  ( Poly, umbral, forwardDiff, appell, binomialType, evalPoly ) where
import MiniGHC.Polynomial (Poly)
import qualified MiniGHC.Polynomial as P
umbral :: [Rational] -> Poly -> Rational
umbral aseq p = sum (zipWith (*) p (aseq ++ repeat 0))
-- Compose with x+1 by Horner's rule, then subtract p(x).
forwardDiff :: Poly -> Poly
forwardDiff p = P.sub (foldr (\c q -> P.add [c] (P.mul [1,1] q)) [] p) p
-- Monomial Appell sequence, p_0=1 and p_n(0)=0 for n>0.
appell :: Int -> Poly
appell n | n < 0 = error "negative polynomial degree"
         | otherwise = replicate n 0 ++ [1]
binomialType :: Int -> Poly
binomialType n | n < 0 = error "negative polynomial degree"
              | otherwise = foldl P.mul [1] [[negate (fromIntegral k),1] | k <- [0..n-1]]
evalPoly :: Poly -> Rational -> Rational
evalPoly = P.eval
