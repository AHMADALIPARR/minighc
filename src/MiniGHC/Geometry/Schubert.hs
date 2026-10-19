{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

module MiniGHC.Geometry.Schubert
  ( SchubertSymbol, pieri, special, littlewoodRichardson, multiply, degree ) where

import qualified Data.Map.Strict as M
import Data.List (foldl')

-- Partitions use weakly decreasing row lengths; trailing zero rows are omitted.
type SchubertSymbol = [Int]
normal :: SchubertSymbol -> SchubertSymbol
normal = reverse . dropWhile (==0) . reverse
valid :: SchubertSymbol -> Bool
valid xs=all (>=0) xs && and (zipWith (>=) xs (drop 1 xs))
partitions :: Int -> Int -> Int -> [SchubertSymbol]
partitions 0 _ _=[[]]
partitions s w h
  | s<0 || w<=0 || h<=0=[]
  | otherwise=[x:xs | x<-[min s w,min s w-1..1],xs<-partitions (s-x) x (h-1)]

special :: Int -> SchubertSymbol
special k | k<0=error "negative Schubert degree"
          | otherwise=normal [k]

pieri :: SchubertSymbol -> Int -> [SchubertSymbol]
pieri a k
  | not (valid a) || k<0=error "invalid partition or Pieri degree"
  | otherwise=[c | (c,m)<-M.toList (multiply a (special k)),m/=0]

-- Count lattice-word semistandard tableaux of skew shape c/a and content b.
littlewoodRichardson :: SchubertSymbol -> SchubertSymbol -> SchubertSymbol -> Integer
littlewoodRichardson a b c
  | not (all valid [a,b,c]) || sum a+sum b/=sum c || any (\i->at a i>at c i) [0..length a-1]=0
  | otherwise=fill cells M.empty (replicate (length b) 0)
  where
    at xs i=if i<length xs then xs!!i else 0
    cells=[(r,col) | (r,w)<-zip [0..] c,col<-[w,w-1..at a r+1]]
    fill [] _ counts=if counts==b then 1 else 0
    fill ((r,col):rest) assigned counts=sum
      [fill rest (M.insert (r,col) v assigned) next
      | v<-[1..length b]
      , counts!!(v-1)<b!!(v-1)
      , maybe True (v<=) (M.lookup (r,col+1) assigned)
      , maybe True (<v) (M.lookup (r-1,col) assigned)
      , let next=[n+if i==v-1 then 1 else 0 | (i,n)<-zip [0..] counts]
      , and (zipWith (>=) next (drop 1 next))]

multiply :: SchubertSymbol -> SchubertSymbol -> M.Map SchubertSymbol Integer
multiply a b
  | not (valid a && valid b)=error "invalid Schubert partition"
  | otherwise=M.fromList [(c,m) | c<-partitions (sum a+sum b) (sum (take 1 a)+sum (take 1 b)) (length a+length b),let m=littlewoodRichardson a b c,m>0]

-- Exact top intersection number on Gr(k,n). The ambient Grassmannian is
-- required: a list of partitions alone does not determine an intersection.
degree :: Int -> Int -> [SchubertSymbol] -> Either String Integer
degree k n cycles
  | k<0 || n<k=Left "invalid Grassmannian dimensions"
  | any (\p->not (valid p) || length (normal p)>k || any (>(n-k)) p) cycles=Left "partition outside Grassmannian rectangle"
  | sum (map sum cycles)/=k*(n-k)=Left "intersection is not zero-dimensional"
  | otherwise=Right (M.findWithDefault 0 (normal (replicate k (n-k))) productMap)
  where
    productMap=foldl' step (M.singleton [] 1) cycles
    step acc p=M.filterWithKey (\shape _->length shape<=k && all (<=(n-k)) shape) $
      M.fromListWith (+) [(shape,m*c) | (a,m)<-M.toList acc,(shape,c)<-M.toList (multiply a (normal p))]
