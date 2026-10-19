{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Hand-rolled Grassmann Ausdehnungslehre (1844).
--
-- Exterior algebra: multivectors, exterior product (∧),
-- and the progressive / regressive products in the spirit of 1844.

module MiniGHC.Geometry.Grassmann
  ( MultiVector
  , exterior
  , grade
  , progressive
  , regressive
  , basis
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)

-- | A multivector is a linear combination of basis blades.
-- Blades are sorted lists of distinct indices (exterior monomials).
type Blade = [Int]
type MultiVector = Map Blade Rational

-- | Exterior (wedge) product.
exterior :: MultiVector -> MultiVector -> MultiVector
exterior a b = Map.filter (/= 0) $
  Map.fromListWith (+)
    [ (sort (ba ++ bb), sign ba bb * ca * cb)
    | (ba, ca) <- Map.toList a
    , (bb, cb) <- Map.toList b
    , null (ba `intersect` bb)
    ]
  where
    intersect xs ys = filter (`elem` ys) xs
    sign xs ys = if inversions (xs ++ ys) `mod` 2 == 0 then 1 else -1
    inversions zs = length [ (i,j) | i <- [0..length zs-1], j <- [i+1..length zs-1], zs!!i > zs!!j ]

-- | Grade of a multivector (highest, or Nothing if mixed).
grade :: MultiVector -> Maybe Int
grade mv = case nub (map length (Map.keys mv)) of
  [g] -> Just g
  _   -> Nothing

-- | Progressive product (meet in the 1844 sense, simplified).
progressive :: MultiVector -> MultiVector -> MultiVector
progressive = exterior   -- progressive product coincides with exterior for complementary grades in simple cases

-- | Regressive product (join, dual of exterior).
regressive :: Int -> MultiVector -> MultiVector -> MultiVector
regressive n a b = dual n (exterior (dual n a) (dual n b))
  where
    dual dim mv = Map.mapKeys (complement dim) mv
    complement dim blade = [i | i <- [1..dim], i `notElem` blade]

-- | Unit basis blade.
basis :: [Int] -> MultiVector
basis b = Map.singleton (sort b) 1
