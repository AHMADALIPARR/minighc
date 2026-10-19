{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Hand-rolled Heaviside operational calculus.
--
-- Oliver Heaviside manipulated the differential operator p = d/dt
-- algebraically to solve linear differential equations arising in
-- electrical engineering (circuits, cables, transmission lines)
-- long before a rigorous justification existed.
--
-- This module treats p as a formal algebraic quantity:
--   * p   = differentiation
--   * 1/p = integration
--   * rational functions of p are expanded and applied to signals
--   * Heaviside's expansion theorem for partial fractions

module MiniGHC.Operators.Heaviside
  ( Operator
  , p
  , apply
  , expand
  , heavisideExpand
  , impedance
  , solveStep
  ) where

import Data.Ratio
import Data.List (foldl')

-- | A rational operator in the indeterminate p.
-- Represented as numerator / denominator, both dense polynomials
-- in p (lowest degree first).
data Operator = Op
  { num :: [Rational]
  , den :: [Rational]
  } deriving (Eq, Show)

-- | The differentiation operator p.
p :: Operator
p = Op [0, 1] [1]

-- | Integrate (1/p).
invP :: Operator
invP = Op [1] [0, 1]

-- | Build a constant operator.
constOp :: Rational -> Operator
constOp c = Op [c] [1]

-- | Add two operators.
add :: Operator -> Operator -> Operator
add (Op n1 d1) (Op n2 d2) =
  let d = lcmPoly d1 d2
      n = addPoly (scalePoly (divPoly d d1) n1)
                  (scalePoly (divPoly d d2) n2)
  in reduce (Op n d)

-- | Multiply two operators.
mul :: Operator -> Operator -> Operator
mul (Op n1 d1) (Op n2 d2) = reduce (Op (mulPoly n1 n2) (mulPoly d1 d2))

-- | Apply an operator to a constant (step) input.
-- For a proper rational operator this yields the time response
-- via Heaviside expansion (residues at poles).
apply :: Operator -> Rational -> [(Rational, Rational)]
apply op amp = map (\(r, a) -> (r, amp * a)) (heavisideExpand op)

-- | Heaviside expansion theorem.
-- For Y(p) = N(p)/D(p) with simple poles r_k,
--   y(t) = sum_k  N(r_k) / D'(r_k)  * exp(r_k t)
-- Returns list of (pole, residue).
heavisideExpand :: Operator -> [(Rational, Rational)]
heavisideExpand (Op n d) =
  let poles = roots d
  in [ (r, evalPoly n r / evalPoly (deriv d) r)
     | r <- poles
     , evalPoly (deriv d) r /= 0
     ]

-- | Expand a rational operator into partial fractions (simple poles only).
expand :: Operator -> [(Rational, Rational)]
expand = heavisideExpand

-- | Simple series RLC impedance operator Z(p) = R + p*L + 1/(p*C)
impedance :: Rational -> Rational -> Rational -> Operator
impedance r l c =
  add (add (constOp r) (mul p (constOp l)))
      (mul invP (constOp (1/c)))

-- | Response of a circuit with impedance Z to a unit step voltage.
solveStep :: Operator -> [(Rational, Rational)]
solveStep z = apply (mul invP (recipOp z)) 1
  where
    recipOp (Op n d) = Op d n

------------------------------------------------------------
-- Polynomial helpers (dense, lowest degree first)
------------------------------------------------------------

type Poly = [Rational]

evalPoly :: Poly -> Rational -> Rational
evalPoly qs x = foldr (\c acc -> c + x * acc) 0 (reverse qs)

deriv :: Poly -> Poly
deriv [] = []
deriv [_] = []
deriv (_:qs) = zipWith (*) qs [1..]

roots :: Poly -> [Rational]
roots q =
  -- Rational root candidates for monic-ish polynomials of low degree.
  -- Sufficient for the hand-rolled examples; not a general solver.
  let cands = [0, 1, -1, 2, -2, 1%2, -1%2, 3, -3]
  in filter (\r -> evalPoly q r == 0) cands

reduce :: Operator -> Operator
reduce (Op n d) =
  let g = gcdPoly n d
  in Op (divPoly n g) (divPoly d g)

gcdPoly :: Poly -> Poly -> Poly
gcdPoly a b
  | all (== 0) b = normalize a
  | otherwise = gcdPoly b (modPoly a b)

modPoly :: Poly -> Poly -> Poly
modPoly a b
  | deg a < deg b = a
  | otherwise =
      let lead = last a / last b
          shift = deg a - deg b
          b' = replicate shift 0 ++ map (* lead) b
      in modPoly (zipWith (-) (a ++ repeat 0) (b' ++ repeat 0)) b

divPoly :: Poly -> Poly -> Poly
divPoly a b
  | all (== 0) b = error "div0"
  | deg a < deg b = []
  | otherwise =
      let lead = last a / last b
          shift = deg a - deg b
          b' = replicate shift 0 ++ map (* lead) b
          a' = zipWith (-) (pad a) (pad b')
      in divPoly (trim a') b ++ [lead]
  where pad xs = xs ++ repeat 0

addPoly :: Poly -> Poly -> Poly
addPoly a b = trim (zipWith (+) (pad a) (pad b))
  where pad xs = xs ++ repeat 0

mulPoly :: Poly -> Poly -> Poly
mulPoly a b = foldl' addPoly [] (zipWith scale [0..] a)
  where scale i c = replicate i 0 ++ map (* c) b

scalePoly :: Poly -> Poly -> Poly
scalePoly s q = mulPoly s q

normalize :: Poly -> Poly
normalize q =
  let q' = trim q
  in if null q' then [0] else map (/ last q') q'

trim :: Poly -> Poly
trim = reverse . dropWhile (== 0) . reverse

deg :: Poly -> Int
deg q = length (trim q) - 1

lcmPoly :: Poly -> Poly -> Poly
lcmPoly a b = divPoly (mulPoly a b) (gcdPoly a b)
