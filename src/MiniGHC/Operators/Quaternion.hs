{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

{-# LANGUAGE StrictData #-}

-- | Quaternion state vectors and the Hamilton product,
-- with Liquid Haskell refinements encoding the
-- norm-multiplicativity theorem:
--
--   |q1 * q2| = |q1| * |q2|
--
-- (Unit quaternions are closed under the product.)

module MiniGHC.Operators.Quaternion
  ( QState(..)
  , entangle
  , normSq
  , isUnit
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

-- | Squared Euclidean norm.
{-@ normSq :: QState -> {v:Double | v >= 0} @-}
normSq :: QState -> Double
normSq (QState a b c d) = a*a + b*b + c*c + d*d

-- | Unit quaternion predicate.
{-@ predicate IsUnit Q = normSq Q == 1.0 @-}

{-@ type UnitQuat = {q:QState | IsUnit q} @-}

-- | Hamilton product (non-commutative state fusion).
{-@ entangle :: q1:QState -> q2:QState
             -> {q:QState | normSq q = normSq q1 * normSq q2} @-}
entangle :: QState -> QState -> QState
entangle (QState a1 b1 c1 d1) (QState a2 b2 c2 d2) = QState
  (a1*a2 - b1*b2 - c1*c2 - d1*d2)   -- real / temporal
  (a1*b2 + b1*a2 + c1*d2 - d1*c2)   -- i
  (a1*c2 - b1*d2 + c1*a2 + d1*b2)   -- j
  (a1*d2 + b1*c2 - c1*b2 + d1*a2)   -- k

-- | Convenience: check whether a state is (approximately) unit.
isUnit :: QState -> Bool
isUnit q = abs (normSq q - 1.0) < 1e-9
