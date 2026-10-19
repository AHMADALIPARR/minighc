# MiniGHC

MiniGHC is a compiler for a small functional language. It provides a concrete surface syntax, a bidirectional type checker, a call-by-value evaluator, an explicitly typed Core intermediate language with free-variable analysis, and an STG-shaped representation suitable for further lowering. The package also contains independent implementations of the classic lambda calculi (simply-typed lambda calculus, System F, and a Hindley–Milner fragment) that form the theoretical basis of the language and of GHC’s intermediate representations.

The project is a Cabal library plus executable. Source programs are ordinary text files with the extension `.mgh`.

## Language

The surface language includes:

- Base types `Nat`, `Bool`, and unit
- Function types and pairs
- Algebraic data types via constructors (`Z`/`S`, `Nil`/`Cons`, and others)
- Lambda abstraction and application
- Non-recursive and recursive `let`
- Conditionals
- Arithmetic operators `+`, `-`, `*` and the primitives `succ`, `pred`, `iszero`
- Case analysis with patterns (variables, constructors, integer literals, pairs, unit, wildcards)
- Type ascriptions

The lexer and recursive-descent parser translate concrete syntax into the abstract syntax defined by `Tm` and `Pat`.

## Compiler pipeline

1. **Lexer and parser**  
   Source text is tokenised and parsed into the internal term representation.

2. **Bidirectional type checking**  
   Synthesis (`synth`) produces a type from a term in a context; checking (`check`) verifies a term against an expected type. Introduction forms are checked and elimination forms synthesise. Pattern matching extends the context by `patCtx`, which propagates the type of the scrutinee. Recursive bindings currently receive a monomorphic arrow type.

3. **Evaluation**  
   Well-typed terms are evaluated by a call-by-value interpreter. Recursive `let` bindings are tied using laziness so that recursive and mutually recursive functions can be used.

4. **Core**  
   Terms are desugared into an explicitly typed Core language. Free-variable analysis over Core supplies the information required for closure conversion.

5. **STG**  
   Core is converted to an STG-like form in which every lambda is a closure and applications follow the shape expected by an STG machine. This representation is the natural starting point for a bytecode or native backend.

The default execution path type-checks a program and then evaluates it. The Core and STG representations are available for optimisation passes or alternative code generators.

## Classic calculi

Separate modules implement the lambda calculi that underlie both the MiniGHC language and GHC’s design:

- **Simply-typed lambda calculus** (`MiniGHC.Calculi.STLC`) — syntax, bidirectional type checker, and evaluator.
- **System F** (`MiniGHC.Calculi.SystemF`) — polymorphic lambda calculus with type abstraction, type application, and type substitution.
- **Hindley–Milner fragment** (`MiniGHC.Calculi.HM`) — a small inference implementation illustrating the classic structure of constraint generation and substitution.

These modules are self-contained and may be used or extended independently of the main pipeline.

## Building and running

Requires GHC 9.2 or later and Cabal.

```sh
cabal build
cabal run minighc                       # built-in demonstrations
cabal run minighc -- examples/factorial.mgh
cabal run minighc -- examples/pairs.mgh
cabal run minighc -- examples/list.mgh
```

## Project layout

```
src/MiniGHC.hs                 main compiler (parser, type checker, evaluator, Core, STG)
src/MiniGHC/Calculi.hs         re-exports of the calculi
src/MiniGHC/Calculi/STLC.hs    simply-typed lambda calculus
src/MiniGHC/Calculi/SystemF.hs System F
src/MiniGHC/Calculi/HM.hs      Hindley–Milner fragment
app/Main.hs                    executable entry point
examples/                      sample .mgh programs
```

## Status

The compiler implements the phases listed above. Natural extensions that build directly on the existing intermediate representations include:

- Full Hindley–Milner inference for recursive groups
- Source locations and structured diagnostics
- Additional Core optimisation passes
- A bytecode or Cmm-style backend driven from the STG representation

## License

BSD-3-Clause. See `LICENSE`.


## Linter

A built-in linter reuses the parser and type checker and reports additional static issues (unused bindings, parse/type errors).

```sh
cabal run minighc -- --lint examples/factorial.mgh
```

The linter runs automatically on every push and pull request via GitHub Actions (`.github/workflows/lint.yml`).
