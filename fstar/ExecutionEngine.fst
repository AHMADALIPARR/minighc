module ExecutionEngine

open FStar.List.Tot

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

let rec initial_state (id:execution_id) : execution_state =
  { id = id; status = Ready; counter = 0; budget = 1000; audit = [] }

let rec advance_state (s:execution_state) : execution_state =
  { s with counter = s.counter + 1; status = Running; audit = "advance" :: s.audit }

let rec fail_state (s:execution_state) (msg:string) : execution_state =
  { s with status = Failed msg; audit = ("fail:" ^ msg) :: s.audit }

let rec complete_state (s:execution_state) : execution_state =
  { s with status = Completed; audit = "complete" :: s.audit }

let rec dispatch_policy (s:execution_state) (p:execution_plan) : bool =
  s.status = Ready && p.max_steps >= list_length p.steps && s.counter < p.max_steps

let rec plan_valid (p:execution_plan) : bool =
  p.name <> "" && p.description <> "" && p.max_steps >= list_length p.steps

let rec try_dispatch (s:execution_state) (p:execution_plan) : dispatch_result =
  if dispatch_policy s p && plan_valid p then Dispatched (advance_state s)
  else Rejected "dispatch rejected"

let rec apply_retry_policy (s:execution_state) (r:retry_policy) : execution_state =
  if r.max_retries <= 5 && r.backoff <= 1000 then { s with counter = s.counter + 1; audit = "retry" :: s.audit }
  else fail_state s "retry policy invalid"

let rec resource_trace_valid (t:resource_trace) : bool =
  t.used <= t.limit && t.limit <= 1000000

let rec reserve_budget (s:execution_state) (amount:nat) : execution_state =
  if amount <= s.budget then { s with budget = s.budget - amount; audit = "reserve" :: s.audit }
  else fail_state s "budget exhausted"

let rec release_budget (s:execution_state) (amount:nat) : execution_state =
  { s with budget = s.budget + amount; audit = "release" :: s.audit }

let rec execution_ready (s:execution_state) : bool =
  s.status = Ready

let rec execution_terminal (s:execution_state) : bool =
  match s.status with
  | Completed -> true
  | Failed _ -> true
  | _ -> false

let rec state_health (s:execution_state) : bool =
  s.counter <= 1000000 && s.budget <= 1000000

let rec add_audit (s:execution_state) (entry:audit_entry) : execution_state =
  { s with audit = entry :: s.audit }

let rec step_plan (name:string) (description:string) (steps:list string) : execution_plan =
  { name = name; description = description; steps = steps; max_steps = list_length steps + 1 }

let rec execution_summary (s:execution_state) : string =
  match s.status with
  | Ready -> "ready"
  | Running -> "running"
  | Completed -> "completed"
  | Failed msg -> "failed: " ^ msg

let rec confirmation_required (s:execution_state) : bool =
  s.status = Running || s.status = Ready

let rec cancel_execution (s:execution_state) : execution_state =
  if confirmation_required s then { s with status = Ready; audit = "cancel" :: s.audit }
  else s

let rec execution_ok (s:execution_state) : bool =
  state_health s && (match s.status with | Ready -> true | Running -> true | Completed -> true | Failed _ -> true)

let rec result_from_submission (s:execution_state) (p:execution_plan) : dispatch_result =
  if try_dispatch s p = Dispatched _ then Dispatched (complete_state (advance_state s))
  else Rejected "submission invalid"

let _ = assert (dispatch_policy (initial_state "job-1") (step_plan "plan" "demo" ["a"; "b"]))
let _ = assert (resource_trace_valid { used = 10; limit = 100 })
let _ = assert (execution_terminal (complete_state (initial_state "job-1")))

























































































































































