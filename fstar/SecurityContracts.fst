module SecurityContracts

open FStar.List.Tot
open FStar.String
open FStar.Tactics

let empty_string : string = ""

type principal = s:string{String.length s > 0}
type capability = s:string{String.length s > 0}
type action = s:string{String.length s > 0}
type reason = s:string{String.length s > 0}

type auth_context =
  { principal:principal;
    capabilities:list capability }

type policy_result =
  | Allowed
  | Denied of reason

type audit_record =
  { event:string;
    principal:principal;
    action:action;
    success:bool }

type state_guard =
  { require_auth:bool;
    require_capability:bool;
    deny_on_failure:bool }

type resource_cap =
  { max_calls:nat;
    max_bytes:nat;
    max_seconds:nat }

type secure_transition =
  { from_state:string;
    to_state:string;
    required_capability:capability }

type protocol_state =
  { step:nat;
    active:bool;
    audited:list audit_record }

let rec list_length (#a:Type) (xs:list a) : nat =
  match xs with
  | [] -> 0
  | _::tl -> 1 + list_length tl

let rec capability_present (c:capability) (caps:list capability) : bool =
  match caps with
  | [] -> false
  | h::t -> if h = c then true else capability_present c t

let authorization_grants (ctx:auth_context) (needed:capability) : bool =
  capability_present needed ctx.capabilities

let fail_closed (guard:state_guard) : policy_result =
  if guard.deny_on_failure then Denied "fail-closed" else Allowed

let enforce_policy (ctx:auth_context) (needed:capability) (g:state_guard) : policy_result =
  if g.require_auth && String.length ctx.principal = 0 then Denied "missing principal"
  else if g.require_capability && not (authorization_grants ctx needed) then (let _ = concat_length needed ": capability missing" in Denied (needed ^ ": capability missing"))
  else Allowed

let min_call_budget (cap:resource_cap) : nat =
  cap.max_calls

let resource_limit_ok (cap:resource_cap) : bool =
  cap.max_calls <= 1000000 && cap.max_bytes <= 1000000000 && cap.max_seconds <= 86400

let apply_capability_limit (cap:resource_cap) (calls:nat) : resource_cap =
  if calls <= cap.max_calls then cap else { cap with max_calls = calls }

let audit_event (principal:principal) (action:action) (success:bool) : audit_record =
  { event = "audit"; principal = principal; action = action; success = success }

let audit_record_ok (r:audit_record) : bool =
  String.length r.event > 0 && String.length r.action > 0 && String.length r.principal > 0

let append_audit (state:protocol_state) (record:audit_record) : protocol_state =
  { state with audited = record :: state.audited; step = state.step + 1 }

let audit_count (state:protocol_state) : nat =
  list_length state.audited

(* A protocol state is secure exactly when its step counter agrees with
   its audit trail: every audit append increments the step by one, so a
   state built by appending has step = number of audit records. The
   previous version was a tautology (active = true || active = false)
   and accepted every state, including a tampered one. *)
let secure_protocol_state (state:protocol_state) : bool =
  state.step = audit_count state

(* Appending one audit record increments the step by exactly one. *)
let append_audit_step_lemma (state:protocol_state) (record:audit_record)
  : Lemma ((append_audit state record).step = state.step + 1) =
  ()

(* Appending preserves the secure-state invariant: the step and the
   audit count both grow by one. *)
let append_audit_preserves_secure (state:protocol_state) (record:audit_record)
  : Lemma
    (requires (secure_protocol_state state))
    (ensures (secure_protocol_state (append_audit state record))) =
  ()

let transition_requires (t:secure_transition) (ctx:auth_context) : policy_result =
  if authorization_grants ctx t.required_capability then Allowed else (let _ = concat_length t.required_capability ": missing transition capability" in Denied (t.required_capability ^ ": missing transition capability"))

let principal_has_required_capability (ctx:auth_context) (needed:capability) : bool =
  authorization_grants ctx needed

(* The concat_length fact is established before the branch here, so it is
   in context for the Denied argument whichever way the branch goes. *)
let denies_missing_capability (ctx:auth_context) (needed:capability) : policy_result =
  let _ = concat_length needed ": not authorized" in
  if principal_has_required_capability ctx needed then Allowed else Denied (needed ^ ": not authorized")

let basic_context (principal:principal) : auth_context =
  { principal = principal; capabilities = [] }

let grant_capability (ctx:auth_context) (c:capability) : auth_context =
  { principal = ctx.principal; capabilities = c :: ctx.capabilities }

let prune_capabilities (ctx:auth_context) : auth_context =
  { principal = ctx.principal; capabilities = [] }

let valid_action (a:action) : bool =
  String.length a > 0

let valid_principal (p:principal) : bool =
  String.length p > 0

let valid_transition (t:secure_transition) : bool =
  String.length t.from_state > 0 && String.length t.to_state > 0 && String.length t.required_capability > 0

let policy_minimally_safe (g:state_guard) : bool =
  g.require_auth || g.require_capability || g.deny_on_failure

let default_guard : state_guard =
  { require_auth = true; require_capability = true; deny_on_failure = true }

let secure_guard (g:state_guard) : bool =
  policy_minimally_safe g

let resource_guard_ok (cap:resource_cap) : bool =
  cap.max_calls > 0 && cap.max_bytes > 0 && cap.max_seconds > 0

let check_resource_cap (cap:resource_cap) : policy_result =
  if resource_limit_ok cap && resource_guard_ok cap then Allowed else Denied "resource budget invalid"

let add_audit_step (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action true)

let mark_failure (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action false)

let state_is_active (state:protocol_state) : bool =
  state.active

(* The old step_is_monotone claimed step >= 0, which is vacuous for a
   nat and says nothing about any transition. Monotonicity is a claim
   about appending, and it is proven as append_audit_step_lemma above:
   every append moves the step up by exactly one, never down. *)

let protocol_is_auditable (state:protocol_state) : bool =
  state.active = true && audit_count state > 0

let capability_guard_ok (ctx:auth_context) (needed:capability) : bool =
  authorization_grants ctx needed

let deny_if_not_granted (ctx:auth_context) (needed:capability) : policy_result =
  if capability_guard_ok ctx needed then Allowed else Denied "capability not granted"

let allowed_transition (ctx:auth_context) (t:secure_transition) : bool =
  principal_has_required_capability ctx t.required_capability

let transition_success (ctx:auth_context) (t:secure_transition) : policy_result =
  if allowed_transition ctx t then Allowed else Denied "transition not authorized"

(* Same rebuilt-record recursion as the registry: the audit list is the
   measure. *)
let rec audit_contains_event (state:protocol_state) (event:string) : Tot bool (decreases state.audited) =
  match state.audited with
  | [] -> false
  | h::t -> if h.event = event then true else audit_contains_event { state with audited = t } event

let principal_is_known (ctx:auth_context) : bool =
  valid_principal ctx.principal

let capability_binding_ok (ctx:auth_context) : bool =
  principal_is_known ctx

let zero_state : protocol_state =
  { step = 0; active = true; audited = [] }

let read_only_state : protocol_state =
  { step = 0; active = false; audited = [] }

(* A system event goes through append_audit like every other record, so
   the step counter stays in agreement with the trail. The previous
   version appended without incrementing the step and silently broke
   the secure-state invariant. *)
let state_after_event (state:protocol_state) (event:string) : protocol_state =
  append_audit state { event = event; principal = "system"; action = "event"; success = true }

let record_transition (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action true)

let fail_transition (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action false)

(* clear_audit is deliberately gone. An audit trail that any caller can
   silently wipe is not an audit trail: it destroys the evidence the
   secure-state invariant counts. Audit is append-only in this model.
   If rotation is ever needed, it must be a new privileged operation
   that seals the old trail, not a quiet delete. *)

let safe_state_after_audit (state:protocol_state) : bool =
  state.active = true && secure_protocol_state state

let capability_context_new (principal:principal) (caps:list capability) : auth_context =
  { principal = principal; capabilities = caps }

let state_guard_is_strong (g:state_guard) : bool =
  g.require_auth && g.require_capability && g.deny_on_failure

let guard_policy_summary (g:state_guard) : string =
  if state_guard_is_strong g then "strong" else "weak"

let capability_projection (ctx:auth_context) : list capability =
  ctx.capabilities

let transition_step_count (state:protocol_state) : nat =
  state.step

let policy_action_consistent (ctx:auth_context) (a:action) : bool =
  valid_action a && valid_principal ctx.principal

let capability_is_present (ctx:auth_context) (c:capability) : bool =
  capability_present c ctx.capabilities

let principal_has_capability (ctx:auth_context) (c:capability) : bool =
  capability_is_present ctx c

let deny_lack_of_capability (ctx:auth_context) (c:capability) : policy_result =
  if principal_has_capability ctx c then Allowed else Denied "missing capability"

let transition_policy_ok (ctx:auth_context) (t:secure_transition) : bool =
  allowed_transition ctx t && valid_transition t

let secure_action_result (ctx:auth_context) (needed:capability) (guard:state_guard) : policy_result =
  enforce_policy ctx needed guard

let guard_defensive_check (guard:state_guard) : bool =
  guard.require_auth || guard.require_capability || guard.deny_on_failure

let principal_capability_map (ctx:auth_context) : nat =
  list_length ctx.capabilities

let deny_by_default (ctx:auth_context) (needed:capability) : policy_result =
  if principal_has_capability ctx needed then Allowed else Denied "deny-by-default"

let security_roundtrip (ctx:auth_context) (needed:capability) (guard:state_guard) : policy_result =
  enforce_policy ctx needed guard

let transition_ledger (state:protocol_state) : list audit_record =
  state.audited

let protocol_policy_state (state:protocol_state) : bool =
  secure_protocol_state state

let assert_secure_state (state:protocol_state) : bool =
  secure_protocol_state state

let grant_and_authorize (ctx:auth_context) (cap:capability) : policy_result =
  let ctx' = grant_capability ctx cap in
  if authorization_grants ctx' cap then Allowed else Denied "authorization failed"

let recheck_capability (ctx:auth_context) (cap:capability) : bool =
  authorization_grants ctx cap

let secure_capability_bundle (ctx:auth_context) : list capability =
  ctx.capabilities

let validated_capability_context (ctx:auth_context) : bool =
  principal_is_known ctx && capability_binding_ok ctx

let capability_policy_fails_closed (ctx:auth_context) (needed:capability) : policy_result =
  if validated_capability_context ctx && authorization_grants ctx needed then Allowed else Denied "fail-closed"

let effective_policy (ctx:auth_context) (needed:capability) : policy_result =
  if valid_principal ctx.principal then enforce_policy ctx needed default_guard else Denied "principal invalid"

let policy_result_ok (r:policy_result) : bool =
  match r with
  | Allowed -> true
  | Denied _ -> false

let deny_if_invalid (ctx:auth_context) (needed:capability) : policy_result =
  if valid_principal ctx.principal then enforce_policy ctx needed default_guard else Denied "invalid context"

let capability_limit_ok (cap:resource_cap) : bool =
  cap.max_calls > 0 && cap.max_seconds >= 0 && cap.max_bytes >= 0

let default_resource_cap : resource_cap =
  { max_calls = 100; max_bytes = 1048576; max_seconds = 60 }

let protected_transition (ctx:auth_context) (t:secure_transition) : policy_result =
  if valid_transition t then transition_requires t ctx else Denied "invalid transition"

let audited_transition (state:protocol_state) (ctx:auth_context) (t:secure_transition) : protocol_state =
  if allowed_transition ctx t then append_audit state (audit_event ctx.principal "transition" true) else fail_transition state ctx.principal "transition"

let zero_capability_context : auth_context =
  { principal = "system"; capabilities = [] }

let context_is_secure (ctx:auth_context) : bool =
  valid_principal ctx.principal

let state_is_secure (ctx:auth_context) (state:protocol_state) : bool =
  context_is_secure ctx && secure_protocol_state state

let capability_authority (ctx:auth_context) (needed:capability) : bool =
  authorization_grants ctx needed

let authorization_is_fail_closed (ctx:auth_context) (needed:capability) : policy_result =
  if capability_authority ctx needed then Allowed else Denied "fail-closed"

let enforce_transition (ctx:auth_context) (t:secure_transition) : policy_result =
  transition_requires t ctx

let resource_and_auth_ok (ctx:auth_context) (cap:resource_cap) (needed:capability) : bool =
  resource_limit_ok cap && authorization_grants ctx needed

let secure_call_result (ctx:auth_context) (needed:capability) (cap:resource_cap) : policy_result =
  if resource_and_auth_ok ctx cap needed then Allowed else Denied "security policy rejected"

(* Concrete end-of-module checks go through the normalizer, as in
   ToolRegistry: SMT does not reliably reduce these small computations
   through the policy helpers, while the normalizer evaluates them to
   values. *)
let _ = assert (authorization_grants { principal = "alice"; capabilities = ["read"] } "read") by (norm [delta; iota; zeta; primops])
let _ = assert (enforce_policy { principal = "alice"; capabilities = ["read"] } "read" default_guard = Allowed) by (norm [delta; iota; zeta; primops])
let _ = assert (deny_if_not_granted { principal = "alice"; capabilities = [] } "write" = Denied "capability not granted") by (norm [delta; iota; zeta; primops])
let _ = assert (resource_limit_ok default_resource_cap) by (norm [delta; iota; zeta; primops])
let _ = assert (secure_protocol_state zero_state) by (norm [delta; iota; zeta; primops])
let _ = assert (secure_protocol_state (append_audit zero_state (audit_event "alice" "read" true))) by (norm [delta; iota; zeta; primops])
let _ = assert (not (secure_protocol_state { step = 3; active = true; audited = [] })) by (norm [delta; iota; zeta; primops])
