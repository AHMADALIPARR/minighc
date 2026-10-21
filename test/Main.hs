module Main where
import qualified MiniGHC as C
import qualified MiniGHC.Lint as L
import qualified MiniGHC.Geometry.Umbral as U
import qualified MiniGHC.Geometry.Schubert as S
import qualified MiniGHC.Geometry.Grassmann as G
import qualified MiniGHC.Polynomial as P
import qualified MiniGHC.Operators.Heaviside as H
import qualified MiniGHC.Calculi.HM as HM
import qualified MiniGHC.Operators.Quaternion as Q
import qualified Data.Map.Strict as M
import qualified MiniGHC.Analysis.Complex as Z
import Data.List (sort)
import Control.Monad (unless,forM_)

assert :: String -> Bool -> IO ()
assert name ok=unless ok (error name)
run :: String -> String -> IO ()
run source expected=case C.parseTerm source of
  Left e->error (source++": "++e)
  Right tm->do
    assert ("machine: "++source) (show (C.compileAndRun tm)==expected)
    assert ("differential: "++source) (show (C.eval [] tm)==expected)
main :: IO ()
main=do
  run "let p = (10,20) in fst p + snd p" "Just VNat 30"
  run "let xs = Cons 1 Nil in case xs of Cons x rest -> x; Nil -> 0" "Just VNat 1"
  run "case 9 of 0 -> 1; _ -> 2" "Just VNat 2"
  run "case (1,2) of (x,y) -> x+y" "Just VNat 3"
  run "case true of True -> 1; False -> 0" "Just VNat 1"
  run "()" "Just VUnit"
  assert "invalid character is a parse error" (case C.parseTerm "@" of Left _->True; _->False)
  assert "constructor arity rejected" (C.synth [] (C.Con "True" [C.LitNat 1])==Nothing)
  assert "negative Nat rejected" (C.synth [] (C.LitNat (-1))==Nothing)
  run "let rec fact = \\n -> if iszero n then 1 else n * fact (pred n) in fact 8" "Just VNat 40320"
  run "let rec even = \\n -> if iszero n then 1 else odd (pred n); odd = \\n -> if iszero n then 0 else even (pred n) in even 10" "Just VNat 1"
  forM_ [0..12::Integer] $ \a->forM_ [0..12::Integer] $ \b->
    run (show a++" - "++show b) ("Just VNat "++show (max 0 (a-b)))
  assert "used binding lint" (null (L.lintWarnings (L.lintSource "let x = 1 in x")))
  assert "unused binding lint" (L.lintWarnings (L.lintSource "let x = 1 in 2")==["unused binding: x"])
  assert "forward difference constant" (null (U.forwardDiff [1]))
  assert "forward difference square" (U.forwardDiff [0,0,1]==[1,2])
  assert "falling factorial" (U.binomialType 3==[0,2,-3,1])
  assert "polynomial evaluation" (U.evalPoly [1,2] 3==7)
  forM_ [0..8] $ \n->assert "Appell identity" (P.derivative (U.appell (n+1))==P.scale (fromIntegral (n+1)) (U.appell n))
  forM_ [[1,2,3],[0,1],[-5,0,2]] $ \p->forM_ [[1,1],[-2,1]] $ \q->
    assert "division reconstruction" (case P.divide p q of Right (quotient,remainder)->P.add (P.mul quotient q) remainder==P.trim p; _->False)
  assert "Pieri" (sort (S.pieri [1] 1)==sort [[2],[1,1]])
  assert "Gr(2,4) four lines" (S.degree 2 4 (replicate 4 [1])==Right 2)
  assert "Gr(2,4) rows" (S.degree 2 4 [[2],[2]]==Right 1)
  assert "Gr(2,4) columns" (S.degree 2 4 [[1,1],[1,1]]==Right 1)
  assert "Gr(2,4) incompatible" (S.degree 2 4 [[2],[1,1]]==Right 0)
  forM_ [1..4] $ \n->forM_ [1..n] $ \j->
    assert "regressive identity" (G.regressive n (G.basis [j]) (G.basis [1..n])==G.basis [j])
  assert "exterior square" (G.exterior (G.basis [1]) (G.basis [1])==mempty)
  assert "RC step" (H.solveStep (H.impedance 1 0 1)==[(-1,1)])
  assert "rational root outside old candidate list" (H.partialFractions (H.Op [1] [-7,1])==Right [(7,1)])
  assert "HM application" (HM.infer (HM.App (HM.Lam "x" (HM.Var "x")) (HM.LitInt 1))==Just HM.TInt)
  assert "HM let polymorphism" (HM.infer (HM.Let "id" (HM.Lam "x" (HM.Var "x")) (HM.Let "n" (HM.App (HM.Var "id") (HM.LitInt 1)) (HM.App (HM.Var "id") (HM.LitBool True))))==Just HM.TBool)
  assert "HM occurs check" (HM.infer (HM.Lam "x" (HM.App (HM.Var "x") (HM.Var "x")))==Nothing)
  assert "empty contour" (Z.contourIntegral id []==0)
  assert "LR coefficient 2" (S.littlewoodRichardson [2,1] [2,1] [3,2,1]==2)
  assert "Pieri horizontal strips" (sort (S.pieri [2,1] 2)==sort [[4,1],[3,2],[3,1,1],[2,2,1]])
  assert "Schubert product [2,1]x[1]" (M.toList (S.multiply [2,1] [1])==[([2,1,1],1),([2,2],1),([3,1],1)])
  assert "contraction e1 into e1^e2" (G.interior (G.basis [1]) (G.basis [1,2])==G.basis [2])
  assert "contraction e2 into e1^e2" (G.interior (G.basis [2]) (G.basis [1,2])==M.singleton [1] (-1))
  assert "contraction blade into volume" (G.interior (G.basis [1,2]) (G.basis [1,2,3])==G.basis [3])
  assert "contraction disjoint" (G.interior (G.basis [1]) (G.basis [2])==mempty)
  assert "contraction by scalar" (G.interior (G.basis []) (G.basis [1,2])==G.basis [1,2])
  forM_ [0..6::Int] $ \n->assert "Appell derivative rule" (P.derivative (U.appellSeq [1,1,2,6,24,120,720] (n+1))==P.scale (fromIntegral (n+1)) (U.appellSeq [1,1,2,6,24,120,720] n))
  assert "Appell value at 0 is the moment" (U.evalPoly (U.appellSeq [1,1,2,6,24,120,720] 4) 0==24)
  assert "appellSeq monomial case" (U.appellSeq (1:repeat 0) 3==[0,0,0,1])
  assert "umbral moment" (U.umbral [1,1,1,1] [0,0,1]==1)
  assert "operator p * invP = 1" (H.mul H.p H.invP==H.constOp 1)
  assert "partial fractions two poles" (H.partialFractions (H.Op [1] [2,-3,1])==Right [(1,-1),(2,1)])
  assert "i squared" (Q.entangle Q.qi Q.qi==Q.QState (-1) 0 0 0)
  assert "j squared" (Q.entangle Q.qj Q.qj==Q.QState (-1) 0 0 0)
  assert "k squared" (Q.entangle Q.qk Q.qk==Q.QState (-1) 0 0 0)
  assert "ij = k" (Q.entangle Q.qi Q.qj==Q.qk)
  assert "ji = -k" (Q.entangle Q.qj Q.qi==Q.negQ Q.qk)
  assert "jk = i" (Q.entangle Q.qj Q.qk==Q.qi)
  assert "kj = -i" (Q.entangle Q.qk Q.qj==Q.negQ Q.qi)
  assert "ki = j" (Q.entangle Q.qk Q.qi==Q.qj)
  assert "ik = -j" (Q.entangle Q.qi Q.qk==Q.negQ Q.qj)
  assert "q * conjugate q = normSq" (Q.entangle (Q.QState 1 2 3 4) (Q.conjugate (Q.QState 1 2 3 4))==Q.fromScalar 30)
  assert "normSq multiplicative" (Q.normSq (Q.entangle (Q.QState 1 2 3 4) (Q.QState 5 6 7 8))==Q.normSq (Q.QState 1 2 3 4)*Q.normSq (Q.QState 5 6 7 8))
  assert "inverse law" (case Q.invQ (Q.QState 1 2 3 4) of Just v-> let Q.QState a b c d = Q.entangle (Q.QState 1 2 3 4) v in abs (a-1)<1e-9 && abs b<1e-9 && abs c<1e-9 && abs d<1e-9; Nothing->False)
  assert "zero has no inverse" (Q.invQ Q.zero==Nothing)
  assert "normalize" (Q.normalize (Q.QState 0 3 0 0)==Just (Q.QState 0 1 0 0))
  assert "rotate x by 90 deg about z" (case Q.rotate (Q.QState (sqrt 0.5) 0 0 (sqrt 0.5)) (1,0,0) of Just (x,y,z)-> abs x<1e-9 && abs (y-1)<1e-9 && abs z<1e-9; Nothing->False)
  assert "rotate zero rotor" (Q.rotate Q.zero (1,0,0)==Nothing)
  putStrLn "Compiler, closure machine, inference and exact-math regressions passed."
