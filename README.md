# MiniGHC

MiniGHC is a deliberately small, self-contained compiler for a functional language that mirrors, at a pedagogical scale, several of the intermediate representations and design choices found in the Glasgow Haskell Compiler. It is not a subset of Haskell, nor does it claim binary or source compatibility with GHC. Its purpose is to make the structure of a modern functional-language compiler concrete and executable: a surface syntax, a bidirectional type checker, a call-by-value evaluator, an explicitly typed Core language, free-variable analysis, and a first step toward an STG-like representation.

The implementation lives in a single module (`src/MiniGHC.hs`) so that the whole pipeline can be read from top to bottom. Each major phase is introduced by a numbered section comment. The project is organised as a conventional Cabal package so that it can be built, tested, and extended in the usual way.

## Surface language

The object language is defined by the types `Ty` and `Tm`. Types include natural numbers, Booleans, the unit type, function types, pairs, and a simple form of algebraic data types written `TData name arguments`. Terms comprise the usual functional core—variables, lambda abstraction, application—together with integer and Boolean literals, the unit value, the classic Peano primitives `Succ`, `Pred` and `IsZero`, conditionals, non-recursive and recursive `let`, pairs and projections, data constructors, case analysis, and type ascriptions.

Patterns are first-class: variables, constructors, integer literals, nested pairs, unit, and wildcards. The language is therefore expressive enough to write small recursive programs over numbers and lists while remaining small enough that every translation step stays visible.

## Bidirectional type checking

Type checking is performed by the pair of functions `synth` and `check`. Synthesis produces a type from a term in a context; checking verifies that a term has an expected type. This split is the classic bidirectional discipline: introduction forms (lambda, pair construction, unit) are checked, while elimination forms (application, projections, case) synthesise. The implementation handles the usual base cases, propagates expected types through conditionals and `let`, and special-cases a handful of constructors (`Z`, `S`, `True`, `False`, `Nil`, `Cons`) so that a few algebraic types can be used without a full data-declaration syntax.

Pattern matching extends the context via `patCtx`. The current implementation is intentionally simplified—many binders are still given the type `TNat`—yet the structure is already the one required for a proper type-directed elaboration. Type ascriptions act as a bridge between the two modes: an ascribed term is checked against the supplied type and then synthesises that type.

## Call-by-value evaluator

A separate interpreter (`eval`) implements a call-by-value semantics for the same language. Values are numbers, Booleans, unit, closures, pairs, and constructor applications. The evaluator is written in the same style as the type checker—pattern matching on the term constructors—so the correspondence between static and dynamic semantics is easy to see. Recursive bindings are still treated simply; a production system would build a fixed-point environment or use a mutable reference. Case analysis is driven by a small pattern matcher that binds variables and continues with the first successful alternative.

## Type-level natural numbers

A short section demonstrates GHC’s type-level programming facilities. Promoted natural numbers, closed type families for addition, multiplication and factorial, and a singleton GADT `SNat` are defined. The values `sThree` and `sSix` show that the type checker can reduce these families at compile time. This fragment is independent of the object-language pipeline; it exists to illustrate the same type-level technology that GHC itself uses for its own internal invariants and for user-level type-level programming.

## Core intermediate language

After type checking, terms are desugared into `Core`. Core is an explicitly typed lambda calculus that retains constructors, case, pairs and the primitive operations, but eliminates some of the surface-language sugar. The translation (`desugar`) is mostly homomorphic; the interesting case is lambda, which currently receives a placeholder type. In a complete compiler the types would be those produced by the type checker, and the Core language would be the starting point for all subsequent optimisations.

Free-variable analysis is defined over Core (`freeVarsCore`). It walks the term, maintaining a set of bound variables, and is used by the next phase to decide which variables must be captured by a closure. Pattern binders are treated uniformly via `patBinders`.

## Toward STG

The final implemented phase converts Core into an STG-like expression language (`StgExpr`). Closures are represented explicitly: a lambda becomes a `StgLet` that binds a closure whose free variables have been computed by the preceding analysis. Application is restricted to the form expected by STG (a variable applied to arguments). Primitive operations become `StgPrim` nodes; data construction becomes `StgConApp`; case becomes `StgCase` with a simplified alternative language. The conversion is still partial—complex arguments and certain pattern forms fall back to an error or a default alternative—but the shape of the translation is the classic one: every function is a closure, every application is a known call or an enter, and free variables are explicit.

The remainder of a realistic pipeline (Cmm, a register-based abstract machine, garbage collection, and a native code generator) is left as future work. The comments in the source indicate where those phases would attach; the present code already produces an STG term that could be fed to such a backend.

## Pipeline and execution

The function `demo` exercises the new language features (pairs, ascriptions, unit, lists, case on pairs) by both type-checking and evaluating them, then shows that the older compilation path still accepts a simple successor application. The `main` entry point simply runs the demonstration. Because every phase is a pure function from one data type to another, it is straightforward to insert pretty-printers, property tests, or alternative backends.

## Relationship to GHC

Real GHC performs far more work: a sophisticated parser, a constraint solver, type-class resolution, a rich Core language with coercions, a long sequence of optimising passes, the Spineless Tagless G-machine, Cmm, and multiple native code generators. MiniGHC isolates a few of the essential ideas—bidirectional checking, an explicitly typed Core, free-variable analysis for closure conversion, and an STG-shaped intermediate—so that they can be studied in isolation. The type-level natural-number example further shows that the same compiler infrastructure used to implement the language can also be used to compute at the type level.

## Limitations and status

Several parts of the implementation are intentionally incomplete:

- Pattern-context construction still assigns many variables the type `TNat`.
- Recursive `let` bindings are given a fixed arrow type and are not properly tied in the evaluator.
- The STG conversion rejects certain argument forms and pattern shapes.
- There is no source location information, no pretty-printer for diagnostics, and no separate lexer or parser; terms are constructed directly in Haskell.

These gaps are deliberate. They keep the code short enough to read in one sitting while still exposing the structure of each phase. Filling any of them is a well-scoped extension.

## Building and running

The package requires a recent GHC (9.2 or later) and Cabal.

```sh
cabal build
cabal run minighc
```

The executable prints the results of the built-in demonstrations. The source is organised so that individual phases can be imported and tested independently once the module is split, should that become desirable.

MiniGHC is offered as a readable, executable sketch of a functional-language compiler. It can be used as a teaching aid, as a starting point for experiments with intermediate languages, or simply as a concrete reference for the ideas that underlie GHC.
