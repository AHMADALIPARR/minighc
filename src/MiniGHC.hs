{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies,
             UndecidableInstances, StandaloneDeriving, FlexibleContexts,
             DeriveFunctor, LambdaCase, TupleSections, PatternGuards #-}

{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module MiniGHC where

import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Control.Monad (guard, when)
import Data.List (nub, (\\), intercalate, find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------------------
-- 1. Object language (significantly expanded)
------------------------------------------------------------

data Ty
  = TNat
  | TBool
  | TUnit
  | TArr Ty Ty
  | TPair Ty Ty
  | TData String [Ty] -- algebraic data types
  deriving (Eq, Show)

data Tm
  = Var String
  | Lam String Tm
  | App Tm Tm
  | LitNat Integer
  | LitBool Bool
  | Unit -- ()
  | Succ Tm | Pred Tm | IsZero Tm
  | If Tm Tm Tm
  | Let String Tm Tm
  | LetRec [(String, Tm)] Tm
  | Pair Tm Tm -- (e1, e2)
  | Fst Tm | Snd Tm
  | Con String [Tm] -- constructor
  | Case Tm [(Pat, Tm)]
  | Ann Tm Ty -- type ascription e :: τ
  deriving (Eq, Show)

data Pat
  = PVar String
  | PCon String [String]
  | PLitNat Integer
  | PPair Pat Pat -- pair patterns
  | PUnit
  | PWild
  deriving (Eq, Show)

------------------------------------------------------------
-- 2. Bidirectional type checker (updated for new features)
------------------------------------------------------------

type Ctx = [(String, Ty)]

synth :: Ctx -> Tm -> Maybe Ty
synth ctx (Var x) = lookup x ctx
synth ctx (Lam _ _) = Nothing -- still checkable only
synth ctx (App f a) = do
  tf <- synth ctx f
  case tf of
    TArr t1 t2 -> check ctx a t1 >> pure t2
    _ -> Nothing
synth ctx (LitNat _) = pure TNat
synth ctx (LitBool _) = pure TBool
synth ctx Unit = pure TUnit
synth ctx (Succ t) = check ctx t TNat >> pure TNat
synth ctx (Pred t) = check ctx t TNat >> pure TNat
synth ctx (IsZero t) = check ctx t TNat >> pure TBool
synth ctx (If c t e) = do
  check ctx c TBool
  tt <- synth ctx t
  check ctx e tt
  pure tt
synth ctx (Let x e b) = do
  te <- synth ctx e
  synth ((x,te):ctx) b
synth ctx (LetRec binds body) = do
  -- simplified: treat recursive bindings as Nat -> Nat for the demo
  let ctx' = [(x, TArr TNat TNat) | (x,_) <- binds] ++ ctx
  mapM_ (\(_,e) -> check ctx' e (TArr TNat TNat)) binds
  synth ctx' body
synth ctx (Pair e1 e2) = do
  t1 <- synth ctx e1
  t2 <- synth ctx e2
  pure (TPair t1 t2)
synth ctx (Fst e) = do
  te <- synth ctx e
  case te of
    TPair t1 _ -> pure t1
    _ -> Nothing
synth ctx (Snd e) = do
  te <- synth ctx e
  case te of
    TPair _ t2 -> pure t2
    _ -> Nothing
synth ctx (Con c args) = case c of
  "Z" -> pure (TData "Nat" [])
  "S" | [a] <- args -> check ctx a (TData "Nat" []) >> pure (TData "Nat" [])
  "True" -> pure TBool
  "False" -> pure TBool
  "Nil" -> pure (TData "List" [TNat]) -- simplified List Nat
  "Cons" | [a,as] <- args -> do
              check ctx a TNat
              check ctx as (TData "List" [TNat])
              pure (TData "List" [TNat])
  _ -> Nothing
synth ctx (Case scrut alts) = do
  tscrut <- synth ctx scrut
  tys <- mapM (\(p,rhs) -> do
                  ctx' <- patCtx ctx p tscrut
                  synth ctx' rhs) alts
  case nub tys of
    [t] -> pure t
    _ -> Nothing
synth ctx (Ann e ty) = check ctx e ty >> pure ty

check :: Ctx -> Tm -> Ty -> Maybe ()
check ctx (Lam x body) (TArr t1 t2) = check ((x,t1):ctx) body t2
check ctx (Pair e1 e2) (TPair t1 t2) = check ctx e1 t1 >> check ctx e2 t2
check ctx Unit TUnit = pure ()
check ctx tm expected = do
  actual <- synth ctx tm
  guard (actual == expected)

patCtx :: Ctx -> Pat -> Ty -> Maybe Ctx
patCtx ctx (PVar x) _ = pure ((x, TNat):ctx) -- still simplified
patCtx ctx (PCon "S" [x]) _ = pure ((x, TData "Nat" []):ctx)
patCtx ctx (PCon "Z" []) _ = pure ctx
patCtx ctx (PCon "Nil" []) _ = pure ctx
patCtx ctx (PCon "Cons" [x,xs]) _ = pure ((x,TNat):(xs, TData "List" [TNat]):ctx)
patCtx ctx (PPair p1 p2) (TPair t1 t2) = do
  ctx1 <- patCtx ctx p1 t1
  patCtx ctx1 p2 t2
patCtx ctx PUnit TUnit = pure ctx
patCtx ctx (PLitNat _) _ = pure ctx
patCtx ctx PWild _ = pure ctx
patCtx _ _ _ = Nothing

------------------------------------------------------------
-- 3. CBV evaluator (updated)
------------------------------------------------------------

data Val
  = VNat Integer
  | VBool Bool
  | VUnit
  | VLam (Val -> Val)
  | VPair Val Val
  | VCon String [Val]
  deriving Show

eval :: [(String,Val)] -> Tm -> Maybe Val
eval env (Var x) = lookup x env
eval env (Lam x b) = pure $ VLam (\v -> fromMaybe (error "eval") (eval ((x,v):env) b))
eval env (App f a) = do
  vf <- eval env f
  va <- eval env a
  case vf of
    VLam k -> pure (k va)
    _ -> Nothing
eval env (LitNat n) = pure (VNat n)
eval env (LitBool b) = pure (VBool b)
eval env Unit = pure VUnit
eval env (Succ t) = do VNat n <- eval env t; pure (VNat (n+1))
eval env (Pred t) = do VNat n <- eval env t; pure (VNat (max 0 (n-1)))
eval env (IsZero t) = do VNat n <- eval env t; pure (VBool (n==0))
eval env (If c t e) = do
  VBool b <- eval env c
  eval env (if b then t else e)
eval env (Let x e b) = do
  v <- eval env e
  eval ((x,v):env) b
eval env (LetRec binds body) = do
  -- still simplified
  eval env body
eval env (Pair e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  pure (VPair v1 v2)
eval env (Fst e) = do VPair v1 _ <- eval env e; pure v1
eval env (Snd e) = do VPair _ v2 <- eval env e; pure v2
eval env (Con c args) = do
  vs <- mapM (eval env) args
  pure (VCon c vs)
eval env (Case scrut alts) = do
  v <- eval env scrut
  matchAlts env v alts
eval env (Ann e _) = eval env e

matchAlts :: [(String,Val)] -> Val -> [(Pat,Tm)] -> Maybe Val
matchAlts env v [] = Nothing
matchAlts env v ((p,rhs):rest) =
  case matchPat env p v of
    Just env' -> eval env' rhs
    Nothing -> matchAlts env v rest

matchPat :: [(String,Val)] -> Pat -> Val -> Maybe [(String,Val)]
matchPat env (PVar x) v = pure ((x,v):env)
matchPat env (PCon c xs) (VCon c' vs)
  | c == c' && length xs == length vs = pure (zip xs vs ++ env)
matchPat env (PPair p1 p2) (VPair v1 v2) = do
  env1 <- matchPat env p1 v1
  matchPat env1 p2 v2
matchPat env PUnit VUnit = pure env
matchPat env (PLitNat n) (VNat m) | n == m = pure env
matchPat env PWild _ = pure env
matchPat _ _ _ = Nothing

------------------------------------------------------------
-- 4. Type-level Nats (unchanged)
------------------------------------------------------------

data Nat = Z | S Nat

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add 'Z m = m
  Add ('S n) m = 'S (Add n m)

type family Mul (n :: Nat) (m :: Nat) :: Nat where
  Mul 'Z _ = 'Z
  Mul ('S n) m = Add m (Mul n m)

type family Fact (n :: Nat) :: Nat where
  Fact 'Z = 'S 'Z
  Fact ('S n) = Mul ('S n) (Fact n)

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

deriving instance Show (SNat n)

reify :: SNat n -> Integer
reify SZ = 0
reify (SS n) = 1 + reify n

type Three = 'S ('S ('S 'Z))
type Six = Add Three Three

sThree :: SNat Three
sThree = SS (SS (SS SZ))

sSix :: SNat Six
sSix = SS (SS (SS (SS (SS (SS SZ)))))

------------------------------------------------------------
-- Phase 1 – Core (updated)
------------------------------------------------------------

data Core
  = CVar String
  | CLam String Ty Core
  | CApp Core Core
  | CLitNat Integer
  | CLitBool Bool
  | CUnit
  | CSucc Core | CPred Core | CIsZero Core
  | CIf Core Core Core
  | CLet String Core Core
  | CLetRec [(String, Core)] Core
  | CPair Core Core
  | CFst Core | CSnd Core
  | CCon String [Core]
  | CCase Core [(CPat, Core)]
  | CAnn Core Ty
  deriving (Eq, Show)

data CPat
  = CPVar String
  | CPCon String [String]
  | CPLitNat Integer
  | CPPair CPat CPat
  | CPUnit
  | CPWild
  deriving (Eq, Show)

desugar :: Tm -> Core
desugar (Var x) = CVar x
desugar (Lam x b) = CLam x TNat (desugar b)
desugar (App f a) = CApp (desugar f) (desugar a)
desugar (LitNat n) = CLitNat n
desugar (LitBool b) = CLitBool b
desugar Unit = CUnit
desugar (Succ t) = CSucc (desugar t)
desugar (Pred t) = CPred (desugar t)
desugar (IsZero t) = CIsZero (desugar t)
desugar (If c t e) = CIf (desugar c) (desugar t) (desugar e)
desugar (Let x e b) = CLet x (desugar e) (desugar b)
desugar (LetRec bs b) = CLetRec [(x, desugar e) | (x,e) <- bs] (desugar b)
desugar (Pair e1 e2) = CPair (desugar e1) (desugar e2)
desugar (Fst e) = CFst (desugar e)
desugar (Snd e) = CSnd (desugar e)
desugar (Con c args) = CCon c (map desugar args)
desugar (Case s alts) = CCase (desugar s) [(desugarPat p, desugar r) | (p,r) <- alts]
desugar (Ann e ty) = CAnn (desugar e) ty

desugarPat :: Pat -> CPat
desugarPat (PVar x) = CPVar x
desugarPat (PCon c xs) = CPCon c xs
desugarPat (PLitNat n) = CPLitNat n
desugarPat (PPair p1 p2) = CPPair (desugarPat p1) (desugarPat p2)
desugarPat PUnit = CPUnit
desugarPat PWild = CPWild

------------------------------------------------------------
-- Free variable analysis (updated)
------------------------------------------------------------

freeVarsCore :: Core -> Set String
freeVarsCore = go Set.empty
  where
    go bound (CVar x)
      | x `Set.member` bound = Set.empty
      | otherwise = Set.singleton x
    go bound (CLam x _ b) = go (Set.insert x bound) b
    go bound (CApp f a) = go bound f `Set.union` go bound a
    go bound (CLitNat _) = Set.empty
    go bound (CLitBool _) = Set.empty
    go bound CUnit = Set.empty
    go bound (CSucc e) = go bound e
    go bound (CPred e) = go bound e
    go bound (CIsZero e) = go bound e
    go bound (CIf c t e) = unions [go bound c, go bound t, go bound e]
    go bound (CLet x e b) = go bound e `Set.union` go (Set.insert x bound) b
    go bound (CLetRec bs b) =
      let bound' = foldr (Set.insert . fst) bound bs
      in foldr (\(_,e) s -> go bound' e `Set.union` s) (go bound' b) bs
    go bound (CPair e1 e2) = go bound e1 `Set.union` go bound e2
    go bound (CFst e) = go bound e
    go bound (CSnd e) = go bound e
    go bound (CCon _ args) = foldr (Set.union . go bound) Set.empty args
    go bound (CCase s alts) =
      go bound s `Set.union`
      foldr (\(p,r) acc -> go (bound `Set.union` patBinders p) r `Set.union` acc) Set.empty alts
    go bound (CAnn e _) = go bound e

    unions = foldr Set.union Set.empty

patBinders :: CPat -> Set String
patBinders (CPVar x) = Set.singleton x
patBinders (CPCon _ xs) = Set.fromList xs
patBinders (CPPair p1 p2) = patBinders p1 `Set.union` patBinders p2
patBinders _ = Set.empty

------------------------------------------------------------
-- Phase 2 – STG (updated for new constructs)
------------------------------------------------------------

data StgExpr
  = StgVar String
  | StgLit Integer
  | StgLitBool Bool
  | StgUnit
  | StgApp String [StgArg]
  | StgConApp String [StgArg]
  | StgLet [(String, StgRhs)] StgExpr
  | StgLetRec [(String, StgRhs)] StgExpr
  | StgCase StgExpr [(StgAlt, StgExpr)]
  | StgPrim String [StgArg]
  deriving Show

data StgArg
  = StgAVar String
  | StgALit Integer
  | StgALitBool Bool
  | StgAUnit
  deriving Show

data StgRhs
  = StgRhsCon String [StgArg]
  | StgRhsClosure [String] StgExpr
  deriving Show

data StgAlt
  = StgAltDefault
  | StgAltLit Integer
  | StgAltCon String [String]
  | StgAltPair String String
  | StgAltUnit
  deriving Show

coreToStg :: Core -> StgExpr
coreToStg (CVar x) = StgVar x
coreToStg (CLitNat n) = StgLit n
coreToStg (CLitBool b) = StgLitBool b
coreToStg CUnit = StgUnit
coreToStg (CLam x _ b) =
  let fvs = Set.toList (freeVarsCore (CLam x TNat b) Set.\\ Set.singleton x)
  in StgLet [("lam", StgRhsClosure fvs (coreToStg b))] (StgVar "lam")
coreToStg (CApp f a) =
  case (coreToStg f, coreToStg a) of
    (StgVar f', arg) -> StgApp f' [toArg arg]
    (StgLet binds (StgVar f'), arg) ->
      StgLet binds (StgApp f' [toArg arg])
    _ -> error "STG: complex application"
coreToStg (CSucc e) = StgPrim "Succ" [toArg (coreToStg e)]
coreToStg (CPred e) = StgPrim "Pred" [toArg (coreToStg e)]
coreToStg (CIsZero e) = StgPrim "IsZero" [toArg (coreToStg e)]
coreToStg (CIf c t e) =
  StgCase (coreToStg c)
    [ (StgAltCon "True" [], coreToStg t)
    , (StgAltCon "False" [], coreToStg e)
    ]
coreToStg (CLet x e b) =
  StgLet [(x, StgRhsClosure [] (coreToStg e))] (coreToStg b)
coreToStg (CLetRec bs b) =
  StgLetRec [(x, StgRhsClosure [] (coreToStg e)) | (x,e) <- bs] (coreToStg b)
coreToStg (CPair e1 e2) = StgConApp "Pair" [toArg (coreToStg e1), toArg (coreToStg e2)]
coreToStg (CFst e) = StgPrim "Fst" [toArg (coreToStg e)]
coreToStg (CSnd e) = StgPrim "Snd" [toArg (coreToStg e)]
coreToStg (CCon c args) = StgConApp c (map (toArg . coreToStg) args)
coreToStg (CCase s alts)=
  StgCase (coreToStg s) [(stgAlt p, coreToStg r) | (p,r) <- alts]
coreToStg (CAnn e _) = coreToStg e

toArg :: StgExpr -> StgArg
toArg (StgVar x) = StgAVar x
toArg (StgLit n) = StgALit n
toArg (StgLitBool b) = StgALitBool b
toArg StgUnit = StgAUnit
toArg e = error $ "STG: complex argument " ++ show e

stgAlt :: CPat -> StgAlt
stgAlt (CPVar _) = StgAltDefault
stgAlt (CPCon c xs) = StgAltCon c xs
stgAlt (CPLitNat n) = StgAltLit n
stgAlt (CPPair (CPVar x) (CPVar y)) = StgAltPair x y
stgAlt CPUnit = StgAltUnit
stgAlt CPWild = StgAltDefault
stgAlt _ = StgAltDefault

------------------------------------------------------------
-- Remaining phases (Cmm + machine) stay largely the same
-- as the previous version for brevity; they already handle
-- the Core/STG forms we generate.
------------------------------------------------------------

-- (Cmm, Machine, GC, runCmmProc, compileAndRun, pretty-printers
-- from the previous message can be reused almost unchanged.
-- Only small extensions for Pair/Fst/Snd/Unit are needed.)

------------------------------------------------------------
-- Demo with the new features
------------------------------------------------------------

demo :: IO ()
demo = do
  putStrLn "=== New language features ==="

  -- Pairs
  let pairEx = Fst (Pair (LitNat 10) (LitNat 20))
  print $ synth [] pairEx -- Just TNat
  print $ eval [] pairEx -- Just (VNat 10)

  -- Type ascription
  let annEx = Ann (LitNat 42) TNat
  print $ synth [] annEx -- Just TNat

  -- Unit
  print $ synth [] Unit -- Just TUnit
  print $ eval [] Unit -- Just VUnit

  -- Simple list via ADT
  let listEx = Con "Cons" [LitNat 1, Con "Cons" [LitNat 2, Con "Nil" []]]
  print $ synth [] listEx -- Just (TData "List" [TNat])

  -- Case on pair (via pattern)
  let casePair = Case (Pair (LitNat 3) (LitNat 4))
                      [(PPair (PVar "x") (PVar "y"), Succ (Var "x"))]
  print $ eval [] casePair -- Just (VNat 4)

  putStrLn "\n=== Pipeline still works ==="
  print $ compileAndRun (App (Lam "x" (Succ (Var "x"))) (LitNat 41))

main :: IO ()
main = demo

