{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Built-in linter for MiniGHC source programs.
--
-- Reuses the existing parser and type checker and adds additional
-- static checks (unused bindings, simple style rules, etc.).

module MiniGHC.Lint
  ( LintResult(..)
  , lintSource
  , lintFile
  , formatLint
  ) where

import MiniGHC (parseTerm, synth)
import Data.List (nub, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (readFile)

data LintResult = LintResult
  { lintErrors   :: [String]
  , lintWarnings :: [String]
  } deriving (Show)

lintSource :: String -> LintResult
lintSource src =
  case parseTerm src of
    Left err -> LintResult [ "parse error: " ++ err ] []
    Right tm ->
      let tyRes = case synth [] tm of
                    Nothing -> ["type error"]
                    Just _  -> []
          unused = findUnused tm
          warns = [ "unused binding: " ++ x | x <- unused ]
      in LintResult tyRes warns

lintFile :: FilePath -> IO LintResult
lintFile path = lintSource <$> readFile path

formatLint :: FilePath -> LintResult -> String
formatLint path (LintResult errs warns) =
  let header = path ++ ":"
      errLines = [ "  error: " ++ e | e <- errs ]
      warnLines = [ "  warning: " ++ w | w <- warns ]
  in unlines (header : errLines ++ warnLines)

-- Very simple free/bound analysis for unused top-level-ish bindings
findUnused :: MiniGHC.Tm -> [String]
findUnused tm = Set.toList (bound tm `Set.difference` free tm)
  where
    free (MiniGHC.Var x) = Set.singleton x
    free (MiniGHC.Lam x b) = free b Set.\\ Set.singleton x
    free (MiniGHC.App f a) = free f `Set.union` free a
    free (MiniGHC.Let x e b) = free e `Set.union` (free b Set.\\ Set.singleton x)
    free (MiniGHC.LetRec bs b) =
      let boundNames = Set.fromList (map fst bs)
          inBinds = foldr (Set.union . free . snd) Set.empty bs
      in (inBinds `Set.union` free b) Set.\\ boundNames
    free (MiniGHC.If c t e) = free c `Set.union` free t `Set.union` free e
    free (MiniGHC.Pair a b) = free a `Set.union` free b
    free (MiniGHC.Fst e) = free e
    free (MiniGHC.Snd e) = free e
    free (MiniGHC.Succ e) = free e
    free (MiniGHC.Pred e) = free e
    free (MiniGHC.IsZero e) = free e
    free (MiniGHC.Add a b) = free a `Set.union` free b
    free (MiniGHC.Sub a b) = free a `Set.union` free b
    free (MiniGHC.Mul a b) = free a `Set.union` free b
    free (MiniGHC.Con _ args) = foldr (Set.union . free) Set.empty args
    free (MiniGHC.Case s alts) = free s `Set.union` foldr (Set.union . freeAlt) Set.empty alts
    free (MiniGHC.Ann e _) = free e
    free _ = Set.empty

    freeAlt (p, r) = free r Set.\\ patBinders p

    bound (MiniGHC.Let x _ b) = Set.singleton x `Set.union` bound b
    bound (MiniGHC.LetRec bs b) = Set.fromList (map fst bs) `Set.union` bound b
    bound (MiniGHC.Lam _ b) = bound b
    bound (MiniGHC.App f a) = bound f `Set.union` bound a
    bound (MiniGHC.If c t e) = bound c `Set.union` bound t `Set.union` bound e
    bound (MiniGHC.Pair a b) = bound a `Set.union` bound b
    bound (MiniGHC.Case s alts) = bound s `Set.union` foldr (Set.union . bound . snd) Set.empty alts
    bound (MiniGHC.Ann e _) = bound e
    bound _ = Set.empty

    patBinders (MiniGHC.PVar x) = Set.singleton x
    patBinders (MiniGHC.PCon _ xs) = Set.fromList xs
    patBinders (MiniGHC.PPair p1 p2) = patBinders p1 `Set.union` patBinders p2
    patBinders _ = Set.empty
