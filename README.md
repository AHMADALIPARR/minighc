# Functional compiler and exact calculi

MiniGHC compiles a small functional language to a strict closure IR and stack
machine. Its library also provides type calculi, rational polynomial arithmetic,
exterior algebra, Schubert intersections and rational operational calculus.
It is an independent compiler project; it does not implement the full GHC
language, runtime or optimizer. Existing package/module names remain compatible.

## Build and run

Requires GHC 8.8 or later and the bundled `base` and `containers` libraries.

```sh
sh test.sh
.build/bin/minighc examples/factorial.mgh
.build/bin/minighc examples/pairs.mgh
.build/bin/minighc examples/list.mgh
.build/bin/minighc --lint examples/pairs.mgh
```

The examples return factorial 120, pair sum 30, and a two-element list. Invalid
input exits unsuccessfully. `cabal build` and `cabal test` are also supported.

## Compilation

The pipeline is source parsing, bidirectional checking, Core lowering,
atomization and closure conversion, instruction generation, then stack-machine
execution. Recursive closures capture lexical environments. The machine supports
applications, recursion, conditionals, constructor matches, pairs and arithmetic.
The STG-like IR is strict, matching this language's evaluator; it is not GHC's
lazy STG machine. The direct interpreter remains available for differential tests.

The surface checker assigns recursive function groups Nat -> Nat. The separate
Hindley–Milner module implements unification, occurs checking, fresh variables and
let-polymorphic inference. The STLC and System F modules expose their own typed
term interfaces. These are separate calculi, rather than unimplemented compiler
phases hidden behind exported names.

## Mathematical interfaces

- Polynomials use low-degree-first rational coefficients. Division, GCD,
  differentiation, multiplication and rational-root enumeration are exact.
- Umbral operations include exact forward differences, the monomial Appell
  sequence and falling factorials.
- Schubert symbols use decreasing partitions. `degree k n cycles` computes top
  intersection numbers on Gr(k,n) using Littlewood–Richardson tableaux; the
  Grassmannian must be supplied explicitly. Invalid dimensions return `Left`.
- Grassmann products track orientation signs and annihilate repeated basis indices.
- Heaviside decomposition is exact for proper rational functions with simple
  rational poles. `partialFractions` reports repeated or irrational poles with
  `Left` rather than returning an incomplete answer. Numerical transcendental
  inverse-Laplace transforms are outside this rational interface.
- Complex contour functions are numerical quadrature. Magnitudes use floating
  point, and the default residue derivative is a central difference;
  `residueWithDerivative` accepts an explicitly supplied derivative.

`test.sh` exercises the compiler against the direct interpreter, recursive
closures, pattern matching, linting, HM inference, polynomial identities,
Grassmann orientation and known Gr(2,4) intersection numbers.

License: BSD-3-Clause; see LICENSE.
