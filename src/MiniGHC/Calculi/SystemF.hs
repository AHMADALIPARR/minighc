{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | System F (polymorphic lambda calculus).

module MiniGHC.Calculi.SystemF
  ( Ty(..), Tm(..), Ctx, synth, eval, Val(..)
  ) where

data Ty
  = TVar String
  | TArr Ty Ty
  | TForall String Ty
  deriving (Eq, Show)

data Tm
  = Var String
  | Lam String Ty Tm
  | App Tm Tm
  | TLam String Tm          -- type abstraction
  | TApp Tm Ty              -- type application
  deriving (Eq, Show)

type Ctx = [(String, Ty)]

substTy :: String -> Ty -> Ty -> Ty
substTy x s (TVar y) | x == y = s
                     | otherwise = TVar y
substTy x s (TArr a b) = TArr (substTy x s a) (substTy x s b)
substTy x s (TForall y b)
  | x == y = TForall y b
  | y `elem` free s = let z=fresh y (free s ++ free b ++ [x])
                     in TForall z (substTy x s (substTy y (TVar z) b))
  | otherwise = TForall y (substTy x s b)

free :: Ty -> [String]
free (TVar x)=[x]
free (TArr a b)=free a++free b
free (TForall x b)=filter (/=x) (free b)
fresh :: String -> [String] -> String
fresh x used=head [x++replicate n '\'' | n<-[1..],x++replicate n '\'' `notElem` used]

synth :: Ctx -> Tm -> Maybe Ty
synth ctx (Var x) = lookup x ctx
synth ctx (Lam x t b) = TArr t <$> synth ((x,t):ctx) b
synth ctx (App f a) = do
  tf <- synth ctx f
  case tf of
    TArr t1 t2 -> do
      ta <- synth ctx a
      if ta == t1 then pure t2 else Nothing
    _ -> Nothing
synth ctx (TLam x b) = TForall x <$> synth ctx b
synth ctx (TApp e t) = do
  te <- synth ctx e
  case te of
    TForall x b -> pure (substTy x t b)
    _ -> Nothing

data Val
  = VLam (Val -> Val)
  | VTLam (Ty -> Val)
  | VBase String

eval :: [(String, Val)] -> Tm -> Maybe Val
eval env (Var x) = lookup x env
eval env (Lam x _ b) = pure $ VLam (\v -> case eval ((x,v):env) b of Just r -> r; Nothing -> error "SF eval")
eval env (App f a) = do
  VLam k <- eval env f
  va <- eval env a
  pure (k va)
eval env (TLam _ b) = pure $ VTLam (\_ -> case eval env b of Just r -> r; Nothing -> error "SF teval")
eval env (TApp e t) = do
  VTLam k <- eval env e
  pure (k t)
