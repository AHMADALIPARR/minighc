module ExecutionEngine

open FStar.List.Tot
open FStar.Tactics

let empty_string : string = ""

type execution_id = s:string{ s <> "" }
type audit_entry = string
type resource_budget = nat

type execution_status =
  | Ready
  | Running
  | Completed
  | Failed of string

type execution_state =
  { id:execution_id;
    status:execution_status;
    counter:nat;
    budget:resource_budget;
    audit:list audit_entry }

type execution_plan =
  { name:string;
    description:string;
    steps:list string;
    max_steps:nat }

type dispatch_result =
  | Dispatched of execution_state
  | Rejected of string

type retry_policy =
  { max_retries:nat;
    backoff:nat }

type resource_trace =
  { used:nat;
    limit:nat }

let rec list_length (#a:Type) (xs:list a) : nat =
  match xs with
  | [] -> 0
  | _::tl -> 1 + list_length tl

let initial_state (id:execution_id) : execution_state =
  { id = id; status = Ready; counter = 0; budget = 1000; audit = [] }

let advance_state (s:execution_state) : execution_state =
  { s with counter = s.counter + 1; status = Running; audit = "advance" :: s.audit }

let fail_state (s:execution_state) (msg:string) : execution_state =
  { s with status = Failed msg; audit = ("fail:" ^ msg) :: s.audit }

let complete_state (s:execution_state) : execution_state =
  { s with status = Completed; audit = "complete" :: s.audit }

let dispatch_policy (s:execution_state) (p:execution_plan) : bool =
  s.status = Ready && p.max_steps >= list_length p.steps && s.counter < p.max_steps

let plan_valid (p:execution_plan) : bool =
  p.name <> "" && p.description <> "" && p.max_steps >= list_length p.steps

let try_dispatch (s:execution_state) (p:execution_plan) : dispatch_result =
  if dispatch_policy s p && plan_valid p then Dispatched (advance_state s)
  else Rejected "dispatch rejected"

let apply_retry_policy (s:execution_state) (r:retry_policy) : execution_state =
  if r.max_retries <= 5 && r.backoff <= 1000 then { s with counter = s.counter + 1; audit = "retry" :: s.audit }
  else fail_state s "retry policy invalid"

let resource_trace_valid (t:resource_trace) : bool =
  t.used <= t.limit && t.limit <= 1000000

let reserve_budget (s:execution_state) (amount:nat) : execution_state =
  if amount <= s.budget then { s with budget = s.budget - amount; audit = "reserve" :: s.audit }
  else fail_state s "budget exhausted"

let release_budget (s:execution_state) (amount:nat) : execution_state =
  { s with budget = s.budget + amount; audit = "release" :: s.audit }

let execution_ready (s:execution_state) : bool =
  s.status = Ready

let execution_terminal (s:execution_state) : bool =
  match s.status with
  | Completed -> true
  | Failed _ -> true
  | _ -> false

let state_health (s:execution_state) : bool =
  s.counter <= 1000000 && s.budget <= 1000000

let add_audit (s:execution_state) (entry:audit_entry) : execution_state =
  { s with audit = entry :: s.audit }

let step_plan (name:string) (description:string) (steps:list string) : execution_plan =
  { name = name; description = description; steps = steps; max_steps = list_length steps + 1 }

let execution_summary (s:execution_state) : string =
  match s.status with
  | Ready -> "ready"
  | Running -> "running"
  | Completed -> "completed"
  | Failed msg -> "failed: " ^ msg

let confirmation_required (s:execution_state) : bool =
  s.status = Running || s.status = Ready

let cancel_execution (s:execution_state) : execution_state =
  if confirmation_required s then { s with status = Ready; audit = "cancel" :: s.audit }
  else s

(* execution_ok is a real health check, not a tautology. Every status a
   constructor can produce satisfies it: advance_state leaves Running
   with counter >= 1, complete_state and fail_state leave a non-empty
   audit trail, and a Ready state is unconstrained. A literal state that
   claims Completed with an empty audit fails it, as it should. *)
let execution_ok (s:execution_state) : bool =
  state_health s &&
  (match s.status with
   | Completed -> list_length s.audit > 0
   | Failed _ -> list_length s.audit > 0
   | Running -> s.counter > 0
   | Ready -> true)

(* A submission dispatches once and then completes. The previous version
   advanced twice (once inside try_dispatch, once explicitly) and also
   compared against a wildcard pattern, which is not a valid
   expression. Dispatch, then complete the dispatched state. *)
let result_from_submission (s:execution_state) (p:execution_plan) : dispatch_result =
  match try_dispatch s p with
  | Dispatched s' -> Dispatched (complete_state s')
  | Rejected r -> Rejected r

(* Run a plan to completion. Each step advances the state, audits its
   own name, and reserves one unit of budget; exhaustion fails the run
   closed with "budget exhausted" and stops it there, and a terminal
   state stops the run wherever it is. An empty plan completes at once.
   The budget never goes negative: reserve_budget refuses to overdraw.
   The recursion is on the step list itself, so termination is
   structural. *)
(* The state argument is rebuilt each step, so it cannot be the measure;
   the step list is: every recursive call consumes its head. *)
let rec run_steps (s:execution_state) (steps:list string) : Tot execution_state (decreases steps) =
  if execution_terminal s then s
  else
    match steps with
    | [] -> complete_state s
    | step::rest ->
      let s1 = reserve_budget (add_audit (advance_state s) step) 1 in
      run_steps s1 rest

let run_plan (s:execution_state) (p:execution_plan) : execution_state =
  run_steps s p.steps

(* A run always ends in a terminal state: by induction on the remaining
   steps. The empty plan completes, a terminal state stays terminal, and
   every step either terminates the run or shortens the remaining plan. *)
let rec run_steps_ends_terminal (s:execution_state) (steps:list string)
  : Lemma (execution_terminal (run_steps s steps)) (decreases steps) =
  if execution_terminal s then ()
  else
    match steps with
    | [] -> ()
    | step::rest ->
      run_steps_ends_terminal
        (reserve_budget (add_audit (advance_state s) step) 1)
        rest

let run_plan_ends_terminal (s:execution_state) (p:execution_plan)
  : Lemma (execution_terminal (run_plan s p)) =
  run_steps_ends_terminal s p.steps

let _ = assert (dispatch_policy (initial_state "job-1") (step_plan "plan" "demo" ["a"; "b"]))
let _ = assert (resource_trace_valid { used = 10; limit = 100 })
let _ = assert (execution_terminal (complete_state (initial_state "job-1")))
let _ = assert (execution_terminal (run_plan (initial_state "job-1") (step_plan "plan" "demo" ["a"; "b"])))
let _ = assert (execution_ok (run_plan (initial_state "job-1") (step_plan "plan" "demo" ["a"; "b"])))
(* Exact-run asserts go through the normalizer: the budget-exhaustion
   trace and the submission state are concrete computations through
   several layers of state updates, which SMT does not reduce, while
   the normalizer evaluates them to values. *)
let _ = assert (execution_summary (run_plan { (initial_state "job-1") with budget = 1 } (step_plan "plan" "demo" ["a"; "b"; "c"])) = "failed: budget exhausted") by (norm [delta; iota; zeta; primops])
let _ = assert (result_from_submission (initial_state "job-1") (step_plan "plan" "demo" ["a"; "b"]) = Dispatched (complete_state (advance_state (initial_state "job-1")))) by (norm [delta; iota; zeta; primops])
