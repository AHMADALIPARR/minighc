{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Built-in linter for MiniGHC source programs.
--
-- Reuses the existing parser and type checker and adds additional
-- static checks (unused bindings).

module MiniGHC.Lint
  ( LintResult(..)
  , lintSource
  , lintFile
  , formatLint
  ) where

import MiniGHC (parseTerm, synth, Tm(..), Pat(..))
import Data.Set (Set)
import qualified Data.Set as Set

data LintResult = LintResult
  { lintErrors   :: [String]
  , lintWarnings :: [String]
  } deriving (Show)

lintSource :: String -> LintResult
lintSource src =
  case parseTerm src of
    Left err -> LintResult ["parse error: " ++ err] []
    Right tm ->
      let tyRes = case synth [] tm of
                    Nothing -> ["type error"]
                    Just _  -> []
          unused = findUnused tm
          warns = ["unused binding: " ++ x | x <- unused]
      in LintResult tyRes warns

lintFile :: FilePath -> IO LintResult
lintFile path = lintSource <$> readFile path

formatLint :: FilePath -> LintResult -> String
formatLint path (LintResult errs warns) =
  let header = path ++ ":"
      errLines = ["  error: " ++ e | e <- errs]
      warnLines = ["  warning: " ++ w | w <- warns]
  in unlines (header : errLines ++ warnLines)

findUnused :: Tm -> [String]
findUnused tm = Set.toList (Set.fromList (walk tm))
  where
    free :: Tm -> Set String
    free (Var x) = Set.singleton x
    free (Lam x b) = free b Set.\\ Set.singleton x
    free (App f a) = free f `Set.union` free a
    free (Let x e b) = free e `Set.union` (free b Set.\\ Set.singleton x)
    free (LetRec bs b) =
      let boundNames = Set.fromList (map fst bs)
          inBinds = foldr (Set.union . free . snd) Set.empty bs
      in (inBinds `Set.union` free b) Set.\\ boundNames
    free (If c t e) = free c `Set.union` free t `Set.union` free e
    free (Pair a b) = free a `Set.union` free b
    free (Fst e) = free e
    free (Snd e) = free e
    free (Succ e) = free e
    free (Pred e) = free e
    free (IsZero e) = free e
    free (Add a b) = free a `Set.union` free b
    free (Sub a b) = free a `Set.union` free b
    free (Mul a b) = free a `Set.union` free b
    free (Con _ args) = foldr (Set.union . free) Set.empty args
    free (Case s alts) = free s `Set.union` foldr (Set.union . freeAlt) Set.empty alts
    free (Ann e _) = free e
    free _ = Set.empty

    freeAlt (p, r) = free r Set.\\ patBinders p

    walk (Let x e b) = [x | x `Set.notMember` free b] ++ walk e ++ walk b
    walk (LetRec bs b) =
      let names=Set.fromList (map fst bs)
          reach seen = let more=Set.unions [free rhs | (x,rhs)<-bs,x `Set.member` seen] `Set.intersection` names
                           next=seen `Set.union` more
                       in if next==seen then seen else reach next
          live=reach (free b `Set.intersection` names)
      in Set.toList (names Set.\\ live) ++ concatMap (walk.snd) bs ++ walk b
    walk (Lam x b) = [x | x `Set.notMember` free b] ++ walk b
    walk (App a b) = walk a ++ walk b
    walk (If a b c) = walk a ++ walk b ++ walk c
    walk (Pair a b) = walk a ++ walk b
    walk (Case s alts) = walk s ++ concat [Set.toList (patBinders p Set.\\ free rhs) ++ walk rhs | (p,rhs)<-alts]
    walk (Con _ xs) = concatMap walk xs
    walk (Fst e) = walk e
    walk (Snd e) = walk e
    walk (Succ e) = walk e
    walk (Pred e) = walk e
    walk (IsZero e) = walk e
    walk (Add a b) = walk a ++ walk b
    walk (Sub a b) = walk a ++ walk b
    walk (Mul a b) = walk a ++ walk b
    walk (Ann e _) = walk e
    walk _ = []

    patBinders :: Pat -> Set String
    patBinders (PVar x) = Set.singleton x
    patBinders (PCon _ xs) = Set.fromList xs
    patBinders (PPair p1 p2) = patBinders p1 `Set.union` patBinders p2
    patBinders _ = Set.empty
