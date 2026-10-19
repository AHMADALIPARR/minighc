# MiniGHC

MiniGHC is a compiler for a small functional language. It implements a complete front-end and a direct evaluation path, with intermediate representations that follow the same architectural lines as the Glasgow Haskell Compiler: a bidirectional type checker, an explicitly typed Core language, free-variable analysis for closure conversion, and an STG-shaped intermediate form.

The compiler accepts source files written in a concrete syntax (`.mgh`), type-checks them, and evaluates them. The implementation is organised as a Cabal library plus executable.

## Language

The language provides:

- Base types: `Nat`, `Bool`, unit
- Function types, pairs
- Algebraic data types (constructors `Z`/`S`, `Nil`/`Cons`, etc.)
- Lambda abstraction, application
- `let` and `let rec`
- Conditionals
- Arithmetic (`+`, `-`, `*`) and the primitives `succ`, `pred`, `iszero`
- Case analysis with patterns (variables, constructors, literals, pairs, wildcards)
- Type ascriptions

Source programs are ordinary text. The parser is a recursive-descent implementation that produces the internal term representation.

## Pipeline

1. **Lexer / Parser**  
   Concrete syntax is tokenised and parsed into the abstract syntax defined by `Tm` and `Pat`.

2. **Bidirectional type checking**  
   `synth` produces a type from a term in a context; `check` verifies a term against an expected type. Introduction forms are checked; elimination forms synthesise. Pattern matching extends the context via `patCtx`, propagating the type of the scrutinee. Recursive bindings are currently given a monomorphic arrow type.

3. **Evaluation**  
   A call-by-value interpreter evaluates well-typed terms. Recursive `let` bindings are tied using laziness so that mutually recursive functions can be used.

4. **Core**  
   Terms are desugared into an explicitly typed Core language. Free-variable analysis is performed over Core and is the basis for closure conversion.

5. **STG**  
   Core is converted to an STG-like representation in which every lambda is a closure and applications are restricted to the form expected by an STG machine. This phase is present and can be driven further into a bytecode or native backend.

The current default execution path is type-check then evaluate. The Core and STG representations are available for optimisation passes or alternative backends.

## Building and running

Requires GHC 9.2+ and Cabal.

```sh
cabal build
cabal run minighc                  # run built-in checks
cabal run minighc -- examples/factorial.mgh
```

## Status

The compiler implements the phases listed above. Areas that remain open for extension include:

- Full Hindley–Milner inference for recursive groups
- Source locations and structured diagnostics
- A bytecode or Cmm-style backend driven from the STG representation
- Additional optimising passes over Core

These are ordinary next steps for the existing intermediate representations, not missing infrastructure.


## Classic calculi

The package also contains independent implementations of the lambda calculi that form the theoretical foundation of the language and of GHC’s intermediate representations:

- **Simply-typed lambda calculus** (`MiniGHC.Calculi.STLC`) — syntax, bidirectional type checker, evaluator.
- **System F** (`MiniGHC.Calculi.SystemF`) — polymorphic lambda calculus with type abstraction and application, including type substitution.
- **Hindley–Milner fragment** (`MiniGHC.Calculi.HM`) — a small inference implementation illustrating the classic constraint-generation structure.

These modules are self-contained and can be used or extended independently of the main compiler pipeline.
