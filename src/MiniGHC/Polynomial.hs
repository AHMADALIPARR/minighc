module MiniGHC.Polynomial
  ( Poly, trim, add, sub, scale, mul, eval, derivative, integral
  , divide, gcdPoly, monic, rationalRoots ) where

import Data.List (nub)
import Data.Ratio (numerator, denominator, (%))

type Poly = [Rational]
trim :: Poly -> Poly
trim = reverse . dropWhile (==0) . reverse
add :: Poly -> Poly -> Poly
add a b = trim (go a b)
  where go [] ys = ys
        go xs [] = xs
        go (x:xs) (y:ys) = (x+y):go xs ys
sub :: Poly -> Poly -> Poly
sub a b = add a (map negate b)
scale :: Rational -> Poly -> Poly
scale c = trim . map (c*)
mul :: Poly -> Poly -> Poly
mul a b = foldr add [] [replicate i 0 ++ scale c b | (i,c) <- zip [0..] a]
eval :: Poly -> Rational -> Rational
eval p x = foldr (\c acc -> c+x*acc) 0 p
derivative :: Poly -> Poly
derivative [] = []
derivative (_:xs) = trim (zipWith (*) xs [1..])
integral :: Poly -> Poly
integral p = 0:zipWith (/) p [1..]

divide :: Poly -> Poly -> Either String (Poly,Poly)
divide a b
  | null divisor = Left "polynomial division by zero"
  | otherwise = Right (go [] (trim a))
  where
    divisor = trim b
    go q r
      | length r < length divisor = (trim q,r)
      | otherwise = let term = replicate (length r-length divisor) 0 ++ [last r/last divisor]
                    in go (add q term) (sub r (mul term divisor))
monic :: Poly -> Poly
monic p = case trim p of [] -> []; xs -> scale (1/last xs) xs
gcdPoly :: Poly -> Poly -> Poly
gcdPoly a b = case trim b of
  [] -> monic a
  bs -> case divide a bs of Right (_,r) -> gcdPoly bs r; Left e -> error e

-- All rational roots, including multiplicity, by the rational-root theorem.
rationalRoots :: Poly -> Either String [Rational]
rationalRoots p = go (trim p)
  where
    go [] = Left "zero polynomial has infinitely many roots"
    go [_] = Right []
    go (0:xs) = (0:) <$> go xs
    go xs = case [r | r <- candidates xs, eval xs r == 0] of
      [] -> Right []
      r:_ -> case divide xs [-r,1] of
        Right (q,[]) -> (r:) <$> go q
        _ -> Left "root division failed"
    candidates xs = let m = foldr lcm 1 (map denominator xs)
                        ints = map (numerator . (*fromInteger m)) xs
                    in nub [s*(a%b) | a <- divisors (abs (head ints)), b <- divisors (abs (last ints)), s <- [-1,1]]
    divisors n = concat [if d*d==n then [d] else [d,n `div` d] | d <- takeWhile (\k -> k*k<=n) [1..], n `mod` d==0]
