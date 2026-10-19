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
findUnused tm = Set.toList (bound tm `Set.difference` free tm)
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

    bound :: Tm -> Set String
    bound (Let x _ b) = Set.singleton x `Set.union` bound b
    bound (LetRec bs b) = Set.fromList (map fst bs) `Set.union` bound b
    bound (Lam _ b) = bound b
    bound (App f a) = bound f `Set.union` bound a
    bound (If c t e) = bound c `Set.union` bound t `Set.union` bound e
    bound (Pair a b) = bound a `Set.union` bound b
    bound (Case s alts) = bound s `Set.union` foldr (Set.union . bound . snd) Set.empty alts
    bound (Ann e _) = bound e
    bound _ = Set.empty

    patBinders :: Pat -> Set String
    patBinders (PVar x) = Set.singleton x
    patBinders (PCon _ xs) = Set.fromList xs
    patBinders (PPair p1 p2) = patBinders p1 `Set.union` patBinders p2
    patBinders _ = Set.empty
