{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

{-# LANGUAGE StrictData #-}

-- | Quaternion algebra over strict 'Double' components:
-- the Hamilton product, conjugate, inverse, and rotation
-- of vectors.
--
-- Refinement specifications state the ideal arithmetic
-- identity |q1 * q2| = |q1| * |q2|. Runtime Double
-- arithmetic is rounded; predicates use a tolerance.

module MiniGHC.Operators.Quaternion
  ( QState(..)
  , zero, one, qi, qj, qk
  , fromScalar, scalarPart, vectorPart, fromVector
  , addQ, subQ, negQ, scaleQ
  , conjugate
  , entangle
  , normSq, norm
  , invQ, normalize
  , isUnit
  , rotate
  ) where

-- | 4D state vector (real, i, j, k).
{-@ data QState = QState
      { qR :: Double
      , qI :: Double
      , qJ :: Double
      , qK :: Double
      }
  @-}
data QState = QState
  { qR :: !Double
  , qI :: !Double
  , qJ :: !Double
  , qK :: !Double
  } deriving (Eq, Show)

zero, one, qi, qj, qk :: QState
zero = QState 0 0 0 0
one  = QState 1 0 0 0
qi   = QState 0 1 0 0
qj   = QState 0 0 1 0
qk   = QState 0 0 0 1

-- | Embed a scalar as a quaternion.
fromScalar :: Double -> QState
fromScalar a = QState a 0 0 0

scalarPart :: QState -> Double
scalarPart (QState a _ _ _) = a

vectorPart :: QState -> (Double, Double, Double)
vectorPart (QState _ b c d) = (b, c, d)

-- | Embed a 3-vector as a pure quaternion.
fromVector :: (Double, Double, Double) -> QState
fromVector (b, c, d) = QState 0 b c d

addQ, subQ :: QState -> QState -> QState
addQ (QState a1 b1 c1 d1) (QState a2 b2 c2 d2) =
  QState (a1+a2) (b1+b2) (c1+c2) (d1+d2)
subQ (QState a1 b1 c1 d1) (QState a2 b2 c2 d2) =
  QState (a1-a2) (b1-b2) (c1-c2) (d1-d2)

negQ :: QState -> QState
negQ (QState a b c d) = QState (negate a) (negate b) (negate c) (negate d)

scaleQ :: Double -> QState -> QState
scaleQ s (QState a b c d) = QState (s*a) (s*b) (s*c) (s*d)

-- | Quaternion conjugate: negates the vector part.
-- q * conjugate q = |q|^2 (a scalar).
{-@ conjugate :: q:QState -> {v:QState | normSq v = normSq q} @-}
conjugate :: QState -> QState
conjugate (QState a b c d) = QState a (negate b) (negate c) (negate d)

-- | Squared Euclidean norm.
{-@ normSq :: QState -> {v:Double | v >= 0} @-}
normSq :: QState -> Double
normSq (QState a b c d) = a*a + b*b + c*c + d*d

-- | Euclidean norm.
norm :: QState -> Double
norm q = sqrt (normSq q)

-- | Hamilton product (non-commutative state fusion).
{-@ entangle :: q1:QState -> q2:QState
             -> {q:QState | normSq q = normSq q1 * normSq q2} @-}
entangle :: QState -> QState -> QState
entangle (QState a1 b1 c1 d1) (QState a2 b2 c2 d2) = QState
  (a1*a2 - b1*b2 - c1*c2 - d1*d2)   -- real / temporal
  (a1*b2 + b1*a2 + c1*d2 - d1*c2)   -- i
  (a1*c2 - b1*d2 + c1*a2 + d1*b2)   -- j
  (a1*d2 + b1*c2 - c1*b2 + d1*a2)   -- k

instance Num QState where
  (+) = addQ
  (-) = subQ
  (*) = entangle
  negate = negQ
  abs q = fromScalar (norm q)
  signum q = case normalize q of
               Just u  -> u
               Nothing -> zero
  fromInteger = fromScalar . fromInteger

-- | Multiplicative inverse, conjugate q / |q|^2.
-- 'Nothing' for the zero quaternion, which has no inverse.
{-@ invQ :: q:QState -> Maybe {v:QState | normSq v * normSq q = 1} @-}
invQ :: QState -> Maybe QState
invQ q
  | normSq q == 0 = Nothing
  | otherwise     = Just (scaleQ (1 / normSq q) (conjugate q))

-- | The unit quaternion in the direction of q.
-- 'Nothing' for the zero quaternion.
normalize :: QState -> Maybe QState
normalize q
  | norm q == 0 = Nothing
  | otherwise   = Just (scaleQ (1 / norm q) q)

-- | Unit quaternion predicate.
{-@ predicate IsUnit Q = normSq Q == 1.0 @-}

{-@ type UnitQuat = {q:QState | IsUnit q} @-}

-- | Convenience: check whether a state is (approximately) unit.
isUnit :: QState -> Bool
isUnit q = abs (normSq q - 1.0) < 1e-9

-- | Rotate a vector by a quaternion: with u the unit form
-- of the rotor, v |-> u * v * conjugate u. The rotor need
-- not be unit on input; 'Nothing' only for a zero rotor.
rotate :: QState -> (Double, Double, Double) -> Maybe (Double, Double, Double)
rotate rotor v = do
  u <- normalize rotor
  let r = entangle (entangle u (fromVector v)) (conjugate u)
  pure (vectorPart r)
