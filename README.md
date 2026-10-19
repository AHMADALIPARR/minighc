# MiniGHC

A small functional-language compiler written in Haskell. The implementation is organised as a sequence of explicit intermediate representations, each with its own data type and transformation. Companion mathematical libraries live in a separate directory and are not part of the compiler build.

## Overview

MiniGHC accepts source files with the extension `.mgh`, type-checks them with a bidirectional checker, and evaluates them with a call-by-value interpreter. The same terms can be lowered through an explicitly typed Core language and an STG-shaped representation.

The package is a Cabal library plus an executable. The library exposes the compiler and a linter; the executable provides a command-line interface.

## Surface language

The language is defined by the types `Ty` and `Tm` in `src/MiniGHC.hs`.

Types are natural numbers, Booleans, unit, function types, pairs, and algebraic data types of the form `TData name arguments`. Terms include variables, lambda abstraction, application, integer and Boolean literals, unit, the Peano primitives `succ`/`pred`/`iszero`, arithmetic operators, conditionals, non-recursive and recursive `let`, pairs and projections, data constructors, case analysis, and type ascriptions.

Patterns support variables, constructors, integer literals, nested pairs, unit, and wildcards.

## Section 1 — Lexer and parser

Concrete syntax is handled by a hand-written lexer and a recursive-descent parser, both in `src/MiniGHC.hs`.

The lexer produces a token stream. The parser is a `newtype` over `[Token] -> Either String (a, [Token])` with `Functor`, `Applicative`, `Monad`, and `Alternative` instances. It supports `let`/`let rec`, `if`/`then`/`else`, `case`/`of`, lambda abstraction, application, pairs, arithmetic operators with precedence, and the built-in primitives.

The entry point is `parseTerm :: String -> Either String Tm`.

## Section 2 — Bidirectional type checking

Type checking is split into synthesis and checking, following the classic bidirectional discipline.

- `synth :: Ctx -> Tm -> Maybe Ty` produces a type from a term.
- `check :: Ctx -> Tm -> Ty -> Maybe ()` verifies a term against an expected type.

Introduction forms (lambda, pair construction, unit) are checked. Elimination forms (application, projections, case) synthesise. Pattern matching extends the context through `patCtx`, which receives the type of the scrutinee and binds variables accordingly. Recursive bindings are currently assigned a monomorphic arrow type.

## Section 3 — Evaluation

A call-by-value interpreter evaluates well-typed terms. Values are numbers, Booleans, unit, closures, pairs, and constructor applications.

Recursive `let` is implemented by knot-tying: the bindings are evaluated in an environment that already contains the recursive values, relying on laziness. The entry point is `eval :: [(String, Val)] -> Tm -> Maybe Val`.

## Section 4 — Core intermediate language

After type checking, terms are desugared into `Core`, an explicitly typed lambda calculus that retains constructors, case, pairs, and primitive operations. The translation is mostly structure-preserving.

Free-variable analysis (`freeVarsCore`) walks Core terms while tracking bound variables. The result is used by later phases to determine which variables must be captured by a closure.

## Section 5 — STG-shaped representation

Core can be converted to an STG-like form in which every lambda is a closure and applications are restricted to the shape expected by an STG machine. The conversion computes free variables and builds explicit closures. This representation is the intended starting point for a bytecode or native backend; the current default path is type-check then evaluate.

## Section 6 — Linter

`src/MiniGHC/Lint.hs` reuses the parser and type checker. It reports parse errors, type errors, and unused bindings discovered by a simple free/bound analysis. The command-line interface accepts `--lint FILE.mgh`.

## Section 7 — Command-line interface

`app/Main.hs` is the executable entry point. With no arguments it runs built-in demonstrations. With a file path it parses, type-checks, and evaluates the file. With `--lint` it invokes the linter.

## Building

Requires GHC 9.2 or later and Cabal.

```sh
cabal build
cabal run minighc
cabal run minighc -- examples/factorial.mgh
cabal run minighc -- --lint examples/pairs.mgh
```

## Examples

The `examples/` directory contains small programs:

- `factorial.mgh` — recursive factorial using `let rec` and arithmetic
- `pairs.mgh` — pair construction and projection
- `list.mgh` — construction of a list via constructors

## Companion mathematics

The `math/` directory holds independent implementations that are not compiled as part of the MiniGHC library:

| Module | Content |
| --- | --- |
| `math/Geometry/Umbral.hs` | Umbral calculus: polynomial sequences, umbral operator, forward difference, Appell and binomial-type sequences |
| `math/Geometry/Grassmann.hs` | Grassmann’s Ausdehnungslehre (1844): multivectors, exterior product, progressive and regressive products |
| `math/Geometry/Schubert.hs` | Schubert enumerative calculus: Schubert symbols and the Pieri rule |
| `math/Operators/Heaviside.hs` | Heaviside operational calculus: algebraic treatment of the differential operator \( p = d/dt \), expansion theorem, simple circuit impedance |
| `math/Analysis/Complex.hs` | Calculus of imaginary quantities: complex arithmetic, residues, polygonal contour integration, Cauchy’s integral formula |
| `math/Operators/Quaternion.hs` | Quaternion state vectors and the Hamilton product, with Liquid Haskell refinements for norm multiplicativity |
| `math/Calculi/STLC.hs` | Simply-typed lambda calculus with bidirectional checking and evaluation |
| `math/Calculi/SystemF.hs` | System F with type abstraction, type application, and substitution |
| `math/Calculi/HM.hs` | A small Hindley–Milner inference fragment |

These modules can be imported or extended independently of the compiler.

## Layout

```
src/MiniGHC.hs          compiler (parser, type checker, evaluator, Core)
src/MiniGHC/Lint.hs     linter
app/Main.hs             executable
examples/               sample programs
math/                   companion mathematical libraries
minighc.cabal           package description
LICENSE                 BSD-3-Clause
```

## License

BSD-3-Clause. See `LICENSE`.
