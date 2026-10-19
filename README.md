# MiniGHC

MiniGHC is a compiler for a small functional language. It implements a concrete surface syntax, a bidirectional type checker, a call-by-value evaluator, an explicitly typed Core intermediate language with free-variable analysis, and an STG-shaped representation. A separate directory contains hand-rolled implementations of classical mathematical calculi that are independent of the compiler.

The project is organised as a Cabal library and executable. Source programs use the extension `.mgh`.

## Language

The surface language provides:

- Base types: natural numbers, Booleans, and unit
- Function types and pairs
- Algebraic data types via constructors (`Z`/`S`, `Nil`/`Cons`, and others)
- Lambda abstraction and application
- Non-recursive and recursive `let`
- Conditionals
- Arithmetic operators (`+`, `-`, `*`) and the primitives `succ`, `pred`, `iszero`
- Case analysis with patterns (variables, constructors, integer literals, pairs, unit, wildcards)
- Type ascriptions

A recursive-descent parser translates concrete syntax into the abstract syntax defined by `Tm` and `Pat`.

## Compiler pipeline

The compiler proceeds through the following phases, all implemented in `src/MiniGHC.hs`:

1. **Lexer and parser**  
   Source text is tokenised and parsed into the internal term representation.

2. **Bidirectional type checking**  
   Synthesis (`synth`) produces a type from a term in a context. Checking (`check`) verifies a term against an expected type. Introduction forms are checked; elimination forms synthesise. Pattern matching extends the context via `patCtx`, which carries the type of the scrutinee. Recursive bindings currently receive a monomorphic arrow type.

3. **Evaluation**  
   Well-typed terms are evaluated by a call-by-value interpreter. Recursive `let` bindings are tied using laziness.

4. **Core**  
   Terms are desugared into an explicitly typed Core language. Free-variable analysis over Core provides the information required for closure conversion.

5. **STG**  
   Core is converted to an STG-like form in which every lambda is a closure and applications follow the shape expected by an STG machine. This representation is the natural starting point for a bytecode or native backend.

The default execution path type-checks a program and then evaluates it. The Core and STG representations are available for further passes.

## Linter

`src/MiniGHC/Lint.hs` reuses the parser and type checker and reports additional static issues, including unused bindings.

```sh
cabal run minighc -- --lint examples/factorial.mgh
```

## Building and running

Requires GHC 9.2 or later and Cabal.

```sh
cabal build
cabal run minighc
cabal run minighc -- examples/factorial.mgh
cabal run minighc -- examples/pairs.mgh
cabal run minighc -- examples/list.mgh
```

## Project layout

```
src/MiniGHC.hs          core compiler (parser, type checker, evaluator, Core, STG)
src/MiniGHC/Lint.hs     linter
app/Main.hs             executable entry point
examples/               sample .mgh programs
math/                   companion mathematical libraries (not part of the build)
minighc.cabal           package description
```

## Companion mathematics

The `math/` directory contains independent, hand-rolled implementations of classical calculi. These modules are not part of the compiler build:

- **Umbral Calculus** (`math/Geometry/Umbral.hs`) — polynomial sequences, umbral operator, forward difference, Appell and binomial-type sequences
- **Grassmann Ausdehnungslehre (1844)** (`math/Geometry/Grassmann.hs`) — multivectors, exterior product, progressive and regressive products
- **Schubert Enumerative Calculus** (`math/Geometry/Schubert.hs`) — Schubert symbols and the Pieri rule
- **Heaviside operational calculus** (`math/Operators/Heaviside.hs`) — algebraic manipulation of the differential operator \( p = d/dt \)
- **Complex-variable calculus** (`math/Analysis/Complex.hs`) — complex numbers, residues, contour integration, Cauchy’s integral formula
- **Quaternions** (`math/Operators/Quaternion.hs`) — Hamilton product with Liquid Haskell refinements for norm multiplicativity
- **Simply-typed lambda calculus** (`math/Calculi/STLC.hs`)
- **System F** (`math/Calculi/SystemF.hs`)
- **Hindley–Milner fragment** (`math/Calculi/HM.hs`)

## Status

The compiler implements the phases listed above. Natural extensions include:

- Full Hindley–Milner inference for recursive groups
- Source locations and structured diagnostics
- Additional Core optimisation passes
- A bytecode or Cmm-style backend driven from the STG representation

## License

BSD-3-Clause. See `LICENSE`.
