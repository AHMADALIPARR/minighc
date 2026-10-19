{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Hand-rolled Schubert Enumerative Calculus.
--
-- Schubert symbols, special Schubert cycles, and the
-- Pieri rule for intersection on Grassmannians.

module MiniGHC.Geometry.Schubert
  ( SchubertSymbol
  , pieri
  , special
  , degree
  ) where

import Data.List (sort, nub)

-- | A Schubert symbol is a strictly increasing sequence of integers
-- (partition indexing a Schubert variety in a Grassmannian).
type SchubertSymbol = [Int]

-- | Pieri rule: intersection of a Schubert cycle with a special cycle.
-- Returns the sum (list) of resulting symbols.
pieri :: SchubertSymbol -> Int -> [SchubertSymbol]
pieri lambda k =
  nub $ sort $
    [ sort (map (+d) lambda)
    | d <- compositions (length lambda) k
    , valid (zipWith (+) lambda d)
    ]
  where
    compositions n s = go n s
      where
        go 0 0 = [[]]
        go 0 _ = []
        go m r = [ x:xs | x <- [0..r], xs <- go (m-1) (r-x) ]
    valid xs = and (zipWith (<) xs (tail xs)) || length xs <= 1

-- | Special Schubert cycle σ_k (single part).
special :: Int -> SchubertSymbol
special k = [k]

-- | Degree of a zero-dimensional Schubert intersection (simplified count).
degree :: [SchubertSymbol] -> Integer
degree cycles
  | all isPoint cycles = 1
  | otherwise = 0
  where
    isPoint [0] = True
    isPoint _   = False
