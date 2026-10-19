{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Simply-typed lambda calculus.

module MiniGHC.Calculi.STLC
  ( Ty(..), Tm(..), Ctx, synth, check, eval, Val(..)
  ) where

data Ty = TBase String | TArr Ty Ty
  deriving (Eq, Show)

data Tm
  = Var String
  | Lam String Ty Tm
  | App Tm Tm
  deriving (Eq, Show)

type Ctx = [(String, Ty)]

synth :: Ctx -> Tm -> Maybe Ty
synth ctx (Var x) = lookup x ctx
synth ctx (Lam x t b) = TArr t <$> synth ((x,t):ctx) b
synth ctx (App f a) = do
  tf <- synth ctx f
  case tf of
    TArr t1 t2 -> check ctx a t1 >> pure t2
    _ -> Nothing

check :: Ctx -> Tm -> Ty -> Maybe ()
check ctx tm expected = do
  actual <- synth ctx tm
  if actual == expected then pure () else Nothing

data Val = VLam (Val -> Val) | VBase String

eval :: [(String, Val)] -> Tm -> Maybe Val
eval env (Var x) = lookup x env
eval env (Lam x _ b) = pure $ VLam (\v -> case eval ((x,v):env) b of Just r -> r; Nothing -> error "STLC eval")
eval env (App f a) = do
  VLam k <- eval env f
  va <- eval env a
  pure (k va)
