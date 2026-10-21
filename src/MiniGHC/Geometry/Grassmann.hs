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
  , interior
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

-- | Inversions of a list of indices: the parity that signs
-- a permutation of basis vectors.
inversions :: [Int] -> Int
inversions zs = length [ (i,j) | i <- [0..length zs-1], j <- [i+1..length zs-1], zs!!i > zs!!j ]

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

-- | Interior product (left contraction) for the standard
-- basis metric: e_I contracted into e_J is zero unless I is
-- a sub-blade of J, in which case it is the complementary
-- blade signed so that e_I \/ (e_I contracted into e_J)
-- reproduces e_J. Bilinear extension of the blade rule;
-- contraction lowers grade, e.g. e1 into (e1 \/ e2) is e2
-- and e2 into (e1 \/ e2) is -e1.
interior :: MultiVector -> MultiVector -> MultiVector
interior a b = Map.filter (/= 0) $
  Map.fromListWith (+)
    [ (rest, sgn * ca * cb)
    | (ba, ca) <- Map.toList a
    , (bb, cb) <- Map.toList b
    , all (`elem` bb) ba
    , let rest = [x | x <- bb, x `notElem` ba]
    , let sgn = if even (inversions (ba ++ rest)) then 1 else -1
    ]

-- | The grade of a multivector when it is homogeneous
-- (every blade of the same grade); Nothing for a mixed
-- multivector or for zero.
grade :: MultiVector -> Maybe Int
grade mv = case nub (map length (Map.keys mv)) of
  [g] -> Just g
  _   -> Nothing

-- | Progressive product. In the Ausdehnungslehre the
-- progressive product of two subspaces whose grades sum
-- to at most the ambient dimension IS the exterior
-- product: it builds the smallest subspace containing
-- both. The complementary regime (grades summing beyond
-- the ambient dimension) is the regressive product.
progressive :: MultiVector -> MultiVector -> MultiVector
progressive = exterior

-- | Regressive product (join, dual of exterior).
regressive :: Int -> MultiVector -> MultiVector -> MultiVector
regressive n a b = undual n (exterior (dual n a) (dual n b))
  where
    dual dim mv = Map.fromListWith (+)
      [(complement dim blade,ca * orientation blade (complement dim blade)) | (blade,ca)<-Map.toList mv]
    undual dim mv = Map.fromListWith (+)
      [(complement dim blade,ca * orientation (complement dim blade) blade) | (blade,ca)<-Map.toList mv]
    complement dim blade = [i | i <- [1..dim], i `notElem` blade]
    orientation xs ys = if even (length [() | x<-xs,y<-ys,x>y]) then 1 else -1

-- | Unit basis blade.
basis :: [Int] -> MultiVector
basis b
  | length (nub b)/=length b=Map.empty
  | otherwise=Map.singleton (sort b) (if even (length [() | (i,x)<-zip [0..] b,y<-drop (i+1) b,x>y]) then 1 else -1)
