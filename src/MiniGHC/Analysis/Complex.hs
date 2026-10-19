{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Hand-rolled calculus of imaginary quantities.
--
-- Complex numbers, analytic operations, residues, and the
-- residue theorem in the classical style of the calculus
-- of imaginary quantities.

module MiniGHC.Analysis.Complex
  ( Complex(..)
  , i
  , real
  , imag
  , magnitude
  , argument
  , residue
  , contourIntegral
  , cauchyIntegral
  ) where

import Data.Ratio

-- | Complex number a + b i, with rational components.
data Complex = C Rational Rational
  deriving (Eq, Show)

i :: Complex
i = C 0 1

real :: Complex -> Rational
real (C a _) = a

imag :: Complex -> Rational
imag (C _ b) = b

instance Num Complex where
  C a b + C c d = C (a+c) (b+d)
  C a b - C c d = C (a-c) (b-d)
  C a b * C c d = C (a*c - b*d) (a*d + b*c)
  negate (C a b) = C (negate a) (negate b)
  abs z = C (magnitude z) 0
  signum z@(C a b)
    | z == 0 = 0
    | otherwise = C (a / m) (b / m)
    where m = magnitude z
  fromInteger n = C (fromInteger n) 0

magnitude :: Complex -> Rational
magnitude (C a b) = toRational (sqrt (fromRational (a*a + b*b) :: Double))

argument :: Complex -> Double
argument (C a b) = atan2 (fromRational b) (fromRational a)

-- | Simple pole residue of f(z) = num/den at a root of den.
-- Assumes a simple zero of the denominator.
residue :: (Complex -> Complex) -> (Complex -> Complex) -> Complex -> Complex
residue num den z0 =
  let dden = deriv den
  in num z0 / dden z0
  where
    deriv f = \z -> (f (z + C (1%1000) 0) - f z) * C 1000 0   -- numeric

-- | Contour integral over a closed polygonal path (trapezoid rule).
contourIntegral :: (Complex -> Complex) -> [Complex] -> Complex
contourIntegral f path =
  sum (zipWith segment path (tail path ++ [head path]))
  where
    segment a b =
      let mid = (a + b) * C (1%2) 0
          dz = b - a
      in f mid * dz

-- | Cauchy's integral formula (simplified): f(a) = (1/(2πi)) ∮ f(z)/(z-a) dz
cauchyIntegral :: (Complex -> Complex) -> Complex -> [Complex] -> Complex
cauchyIntegral f a path =
  let integrand z = f z / (z - a)
      oint = contourIntegral integrand path
  in oint / (2 * piApprox * i)
  where
    piApprox = C (355%113) 0   -- rational approximation
