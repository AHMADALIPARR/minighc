# F* formal contracts

Formal contracts for a tool registry, an execution engine, and a
security model, plus the dispatcher that wires them into one dispatch
path. The modules are checked by F* (release 2026.10.18); every claim
below is a machine-checked definition, lemma, or assertion, not a
comment about intent.

## Verify

```sh
sh fstar/check.sh
```

`check.sh` checks the modules in dependency order, one file per
invocation, rebuilding the `.fstar-cache/` directory from scratch each
run. This F* version rejects multi-file invocations, so the cache is
how later modules see earlier ones. The default toolchain is the
release unpacked under `~/workspace/.tools/fstar`; point `FSTAR_BIN` at
another `fstar.exe` to use a different one.

## Modules

- **ToolRegistry.fst** — tool specs (id, capabilities, typed parameter
  specs with optional integer bounds), registration, parameter
  validation, capability dispatch. Registration is a `Pure` function
  with a precondition (fresh id, non-empty id and name, valid schemas)
  and a postcondition (version bumped by one, tool present). Two lemmas
  prove registration preserves well-formedness (valid schemas, no
  duplicate ids) and the version guard (version = entry count).
  Integer parameters are really parsed and range-checked: the parser
  reads digits (optional leading minus) and rejects empty, non-numeric,
  and out-of-range values.
- **SecurityContracts.fst** — principals, capabilities, auth contexts,
  deny-by-default policy, resource caps, and an auditable protocol
  state. A protocol state is secure exactly when its step counter
  agrees with its audit trail (`step = audit count`); appending a
  record preserves that invariant, proven as lemmas. Audit is
  append-only: there is deliberately no operation that wipes the trail.
- **ExecutionEngine.fst** — execution states, plans, dispatch policy,
  budget reservation, and a plan runner. `run_plan` advances once per
  step, audits each step name, reserves one unit of budget per step,
  and fails closed on exhaustion; a lemma proves every run ends in a
  terminal state.
- **Dispatcher.fst** — the wiring. Dispatch is a gate chain, in order,
  and every failure is a rejection with a specific reason:
  1. the tool must be registered (lookup by id);
  2. every capability the tool declares must be granted;
  3. every provided parameter must validate against its spec;
  4. the caller must be authorized for every declared capability, and
     the resource cap must be well-formed;
  5. the engine's dispatch policy and plan validity must hold.
  Only then does the plan run, and the dispatch itself is audited.
  The dispatcher opens none of the three modules (all three define
  names like `capability` and `list_length`), so every reference is
  qualified, and capability comparison across the registry and the
  security model is done at the string level to avoid fragile
  refinement-subtyping between the two capability types.

## What the scaffold got wrong

The scaffold these modules grew from was never run through the
checker, and it showed. Fixed while implementing, all confirmed by
failed checks before the fix and passing checks after:

- An assertion granted only `read` to a tool requiring `read` and
  `write`, so it asserted false and the file could never verify.
- `result_from_submission` advanced twice (once inside `try_dispatch`,
  once explicitly) and compared against a wildcard pattern, which is
  not a valid expression; it now dispatches once and completes the
  dispatched state.
- `secure_protocol_state` was a tautology (`active = true || active =
  false`) and `step_is_monotone` was vacuous for a `nat`; the first is
  now the step/audit-count invariant, the second is replaced by a lemma
  proving each append moves the step up by exactly one.
- `state_after_event` appended without incrementing the step, silently
  breaking the invariant; it goes through `append_audit` like every
  other record now.
- Integer bounds were documentation only: the old validator accepted
  any non-empty string when bounds were present.
- Well-formedness ignored duplicate ids, so a duplicate could silently
  shadow a registration; the version guard always returned true.
- Several `let rec` declarations were not recursive at all, and two
  value aliases (`default_registry`, `default_guard`, and friends)
  used `let rec` where F* only allows it on functions.
- The pair type is written `&`, not `*`, in this F* version: `list
  (param_name * string)` parses as multiplication and fails.
- Two facts about this checker that shaped the code: SMT does not
  reduce a recursive parser over string primitives, so computational
  asserts (integer vectors, exact run traces, dispatch fixtures) go
  through the normalizer (`norm [delta; iota; zeta; primops]`), which
  evaluates them to values; and the `reason` refinement (non-empty
  denial reasons) needs the stdlib `concat_length` lemma applied at
  concatenation sites, since the solver does not otherwise know that
  concatenation preserves length.
