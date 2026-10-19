{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

module MiniGHC.Calculi.HM (Ty(..), Tm(..), infer) where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
data Ty = TInt | TBool | TArr Ty Ty | TVar Int deriving (Eq,Show)
data Tm = Var String | Lam String Tm | App Tm Tm | LitInt Integer
        | LitBool Bool | Let String Tm Tm deriving (Eq,Show)
data Scheme = Forall [Int] Ty
type Subst = M.Map Int Ty
type Env = M.Map String Scheme
vars :: Ty -> S.Set Int
vars (TVar n)=S.singleton n
vars (TArr a b)=vars a `S.union` vars b
vars _=S.empty
apply :: Subst -> Ty -> Ty
apply s (TVar n)=M.findWithDefault (TVar n) n s
apply s (TArr a b)=TArr (apply s a) (apply s b)
apply _ t=t
compose :: Subst -> Subst -> Subst
compose a b=M.map (apply a) b `M.union` a
schemeVars :: Scheme -> S.Set Int
schemeVars (Forall ns t)=vars t S.\\ S.fromList ns
applyEnv :: Subst -> Env -> Env
applyEnv s=M.map (\(Forall ns t)->Forall ns (apply (foldr M.delete s ns) t))
generalize :: Env -> Ty -> Scheme
generalize env t=Forall (S.toList (vars t S.\\ S.unions (map schemeVars (M.elems env)))) t
unify :: Ty -> Ty -> Maybe Subst
unify a b | a==b=Just M.empty
unify (TVar n) t | n `S.notMember` vars t=Just (M.singleton n t)
unify t (TVar n)=unify (TVar n) t
unify (TArr a b) (TArr c d)=do
  s<-unify a c
  r<-unify (apply s b) (apply s d)
  pure (compose r s)
unify _ _=Nothing
infer :: Tm -> Maybe Ty
infer tm=do (s,t,_)<-go M.empty 0 tm; pure (apply s t)
  where
    go :: Env -> Int -> Tm -> Maybe (Subst,Ty,Int)
    go env n (Var x)=do
      Forall vs t<-M.lookup x env
      let fresh=M.fromList (zip vs (map TVar [n..]))
      pure (M.empty,apply fresh t,n+length vs)
    go env n (Lam x b)=do
      (s,t,m)<-go (M.insert x (Forall [] (TVar n)) env) (n+1) b
      pure (s,TArr (apply s (TVar n)) t,m)
    go env n (App f a)=do
      (s,t,m)<-go env n f
      (r,u,k)<-go (applyEnv s env) m a
      v<-unify (apply r t) (TArr u (TVar k))
      pure (compose v (compose r s),apply v (TVar k),k+1)
    go env n (Let x e b)=do
      (s,t,m)<-go env n e
      let env'=applyEnv s env
      (r,u,k)<-go (M.insert x (generalize env' (apply s t)) env') m b
      pure (compose r s,u,k)
    go _ n (LitInt _)=Just (M.empty,TInt,n)
    go _ n (LitBool _)=Just (M.empty,TBool,n)
