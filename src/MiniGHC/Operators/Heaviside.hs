{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

module MiniGHC.Operators.Heaviside
  ( Operator(..), p, invP, constOp, add, mul, apply, expand
  , heavisideExpand, partialFractions, impedance, solveStep ) where
import Data.List (nub)
import qualified MiniGHC.Polynomial as P
data Operator = Op { num :: P.Poly, den :: P.Poly } deriving (Eq,Show)
p, invP :: Operator
p = Op [0,1] [1]
invP = Op [1] [0,1]
constOp :: Rational -> Operator
constOp c = Op [c] [1]
reduce :: Operator -> Operator
reduce (Op n d)
  | null (P.trim d) = error "zero operator denominator"
  | otherwise = let g=P.gcdPoly n d
                    exact a b = case P.divide a b of Right (q,[]) -> q; _ -> error "non-exact polynomial reduction"
                    nn=exact n g; dd=exact d g
                in Op (P.scale (1/last dd) nn) (P.monic dd)
add, mul :: Operator -> Operator -> Operator
add (Op a b) (Op c d) = reduce (Op (P.add (P.mul a d) (P.mul c b)) (P.mul b d))
mul (Op a b) (Op c d) = reduce (Op (P.mul a c) (P.mul b d))
-- Exact decomposition for proper rational functions with simple rational poles.
partialFractions :: Operator -> Either String [(Rational,Rational)]
partialFractions op = do
  let Op n d = reduce op
  if length (P.trim n) >= length d then Left "improper rational function" else do
    rs <- P.rationalRoots d
    if length rs /= length d-1 then Left "denominator has non-rational poles"
    else if length (nub rs) /= length rs then Left "repeated poles require polynomial-exponential terms"
    else Right [(r,P.eval n r / P.eval (P.derivative d) r) | r <- rs]
heavisideExpand, expand :: Operator -> [(Rational,Rational)]
heavisideExpand op = either error id (partialFractions op)
-- | Synonym for 'heavisideExpand'.
expand = heavisideExpand
apply :: Operator -> Rational -> [(Rational,Rational)]
apply op amp = [(r,amp*a) | (r,a) <- heavisideExpand (mul op invP)]
impedance :: Rational -> Rational -> Rational -> Operator
impedance r l c
  | c==0 = error "capacitance must be nonzero"
  | otherwise = add (add (constOp r) (mul p (constOp l))) (mul invP (constOp (1/c)))
-- | Step response of the system whose IMPEDANCE is the
-- given operator Z = n/d: the current drawn by a unit
-- step voltage, I(p) = (1/p) / Z(p) = d / (p * n),
-- returned as (pole, amplitude) pairs of the partial
-- fraction expansion, i.e. the time response is the sum
-- of amp * exp(pole * t).
solveStep :: Operator -> [(Rational,Rational)]
solveStep (Op n d) = apply (Op d n) 1
