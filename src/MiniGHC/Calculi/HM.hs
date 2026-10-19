{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | A small Hindley–Milner fragment (monomorphic for brevity;
-- the structure is the classic one).

module MiniGHC.Calculi.HM
  ( Ty(..), Tm(..), infer
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Ty = TInt | TBool | TArr Ty Ty | TVar Int
  deriving (Eq, Show)

data Tm
  = Var String
  | Lam String Tm
  | App Tm Tm
  | LitInt Integer
  | LitBool Bool
  deriving (Eq, Show)

type Subst = Map Int Ty
type Env = Map String Ty

infer :: Tm -> Maybe Ty
infer tm = do
  (ty, _) <- go Map.empty 0 tm
  pure ty
  where
    go :: Env -> Int -> Tm -> Maybe (Ty, Subst)
    go env _ (Var x) = do
      t <- Map.lookup x env
      pure (t, Map.empty)
    go env n (Lam x b) = do
      let tv = TVar n
          env' = Map.insert x tv env
      (tb, s) <- go env' (n+1) b
      pure (TArr (apply s tv) tb, s)
    go env n (App f a) = do
      (tf, s1) <- go env n f
      (ta, s2) <- go (Map.map (apply s1) env) n a
      let tf' = apply s2 tf
      case tf' of
        TArr t1 t2 | t1 == ta -> pure (t2, compose s2 s1)
        _ -> Nothing
    go _ _ (LitInt _) = pure (TInt, Map.empty)
    go _ _ (LitBool _) = pure (TBool, Map.empty)

apply :: Subst -> Ty -> Ty
apply s (TVar i) = Map.findWithDefault (TVar i) i s
apply s (TArr a b) = TArr (apply s a) (apply s b)
apply _ t = t

compose :: Subst -> Subst -> Subst
compose s2 s1 = Map.map (apply s2) s1 `Map.union` s2
