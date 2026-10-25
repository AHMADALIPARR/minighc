module SecurityContracts

open FStar.List.Tot
open FStar.String

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

let rec authorization_grants (ctx:auth_context) (needed:capability) : bool =
  capability_present needed ctx.capabilities

let rec fail_closed (guard:state_guard) : policy_result =
  if guard.deny_on_failure then Denied "fail-closed" else Allowed

let rec enforce_policy (ctx:auth_context) (needed:capability) (g:state_guard) : policy_result =
  if g.require_auth && String.length ctx.principal = 0 then Denied "missing principal"
  else if g.require_capability && not (authorization_grants ctx needed) then Denied (needed ^ ": capability missing")
  else Allowed

let rec min_call_budget (cap:resource_cap) : nat =
  cap.max_calls

let rec resource_limit_ok (cap:resource_cap) : bool =
  cap.max_calls <= 1000000 && cap.max_bytes <= 1000000000 && cap.max_seconds <= 86400

let rec apply_capability_limit (cap:resource_cap) (calls:nat) : resource_cap =
  if calls <= cap.max_calls then cap else { cap with max_calls = calls }

let rec audit_event (principal:principal) (action:action) (success:bool) : audit_record =
  { event = "audit"; principal = principal; action = action; success = success }

let rec audit_record_ok (r:audit_record) : bool =
  String.length r.event > 0 && String.length r.action > 0 && String.length r.principal > 0

let rec append_audit (state:protocol_state) (record:audit_record) : protocol_state =
  { state with audited = record :: state.audited; step = state.step + 1 }

let rec audit_count (state:protocol_state) : nat =
  list_length state.audited

let rec secure_protocol_state (state:protocol_state) : bool =
  state.active = true || state.active = false

let rec transition_requires (t:secure_transition) (ctx:auth_context) : policy_result =
  if authorization_grants ctx t.required_capability then Allowed else Denied (t.required_capability ^ ": missing transition capability")

let rec principal_has_required_capability (ctx:auth_context) (needed:capability) : bool =
  authorization_grants ctx needed

let rec denies_missing_capability (ctx:auth_context) (needed:capability) : policy_result =
  if principal_has_required_capability ctx needed then Allowed else Denied (needed ^ ": not authorized")

let rec basic_context (principal:principal) : auth_context =
  { principal = principal; capabilities = [] }

let rec grant_capability (ctx:auth_context) (c:capability) : auth_context =
  { principal = ctx.principal; capabilities = c :: ctx.capabilities }

let rec prune_capabilities (ctx:auth_context) : auth_context =
  { principal = ctx.principal; capabilities = [] }

let rec valid_action (a:action) : bool =
  String.length a > 0

let rec valid_principal (p:principal) : bool =
  String.length p > 0

let rec valid_transition (t:secure_transition) : bool =
  String.length t.from_state > 0 && String.length t.to_state > 0 && String.length t.required_capability > 0

let rec policy_minimally_safe (g:state_guard) : bool =
  g.require_auth || g.require_capability || g.deny_on_failure

let rec default_guard : state_guard =
  { require_auth = true; require_capability = true; deny_on_failure = true }

let rec secure_guard (g:state_guard) : bool =
  policy_minimally_safe g

let rec resource_guard_ok (cap:resource_cap) : bool =
  cap.max_calls > 0 && cap.max_bytes > 0 && cap.max_seconds > 0

let rec check_resource_cap (cap:resource_cap) : policy_result =
  if resource_limit_ok cap && resource_guard_ok cap then Allowed else Denied "resource budget invalid"

let rec add_audit_step (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action true)

let rec mark_failure (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action false)

let rec state_is_active (state:protocol_state) : bool =
  state.active

let rec step_is_monotone (state:protocol_state) : bool =
  state.step >= 0

let rec protocol_is_auditable (state:protocol_state) : bool =
  state.active = true && audit_count state > 0

let rec capability_guard_ok (ctx:auth_context) (needed:capability) : bool =
  authorization_grants ctx needed

let rec deny_if_not_granted (ctx:auth_context) (needed:capability) : policy_result =
  if capability_guard_ok ctx needed then Allowed else Denied "capability not granted"

let rec allowed_transition (ctx:auth_context) (t:secure_transition) : bool =
  principal_has_required_capability ctx t.required_capability

let rec transition_success (ctx:auth_context) (t:secure_transition) : policy_result =
  if allowed_transition ctx t then Allowed else Denied "transition not authorized"

let rec audit_contains_event (state:protocol_state) (event:string) : bool =
  match state.audited with
  | [] -> false
  | h::t -> if h.event = event then true else audit_contains_event { state with audited = t } event

let rec principal_is_known (ctx:auth_context) : bool =
  valid_principal ctx.principal

let rec capability_binding_ok (ctx:auth_context) : bool =
  principal_is_known ctx

let rec zero_state : protocol_state =
  { step = 0; active = true; audited = [] }

let rec read_only_state : protocol_state =
  { step = 0; active = false; audited = [] }

let rec state_after_event (state:protocol_state) (event:string) : protocol_state =
  { state with audited = { event = event; principal = "system"; action = "event"; success = true } :: state.audited }

let rec record_transition (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action true)

let rec fail_transition (state:protocol_state) (principal:principal) (action:action) : protocol_state =
  append_audit state (audit_event principal action false)

let rec clear_audit (state:protocol_state) : protocol_state =
  { state with audited = [] }

let rec safe_state_after_audit (state:protocol_state) : bool =
  state.active = true && state.step >= 0

let rec capability_context_new (principal:principal) (caps:list capability) : auth_context =
  { principal = principal; capabilities = caps }

let rec state_guard_is_strong (g:state_guard) : bool =
  g.require_auth && g.require_capability && g.deny_on_failure

let rec guard_policy_summary (g:state_guard) : string =
  if state_guard_is_strong g then "strong" else "weak"

let rec capability_projection (ctx:auth_context) : list capability =
  ctx.capabilities

let rec transition_step_count (state:protocol_state) : nat =
  state.step

let rec policy_action_consistent (ctx:auth_context) (a:action) : bool =
  valid_action a && valid_principal ctx.principal

let rec capability_is_present (ctx:auth_context) (c:capability) : bool =
  capability_present c ctx.capabilities

let rec principal_has_capability (ctx:auth_context) (c:capability) : bool =
  capability_is_present ctx c

let rec deny_lack_of_capability (ctx:auth_context) (c:capability) : policy_result =
  if principal_has_capability ctx c then Allowed else Denied "missing capability"

let rec transition_policy_ok (ctx:auth_context) (t:secure_transition) : bool =
  allowed_transition ctx t && valid_transition t

let rec secure_action_result (ctx:auth_context) (needed:capability) (guard:state_guard) : policy_result =
  enforce_policy ctx needed guard

let rec guard_defensive_check (guard:state_guard) : bool =
  guard.require_auth || guard.require_capability || guard.deny_on_failure

let rec principal_capability_map (ctx:auth_context) : nat =
  list_length ctx.capabilities

let rec deny_by_default (ctx:auth_context) (needed:capability) : policy_result =
  if principal_has_capability ctx needed then Allowed else Denied "deny-by-default"

let rec security_roundtrip (ctx:auth_context) (needed:capability) (guard:state_guard) : policy_result =
  enforce_policy ctx needed guard

let rec transition_ledger (state:protocol_state) : list audit_record =
  state.audited

let rec protocol_policy_state (state:protocol_state) : bool =
  secure_protocol_state state

let rec assert_secure_state (state:protocol_state) : bool =
  secure_protocol_state state

let rec grant_and_authorize (ctx:auth_context) (cap:capability) : policy_result =
  let ctx' = grant_capability ctx cap in
  if authorization_grants ctx' cap then Allowed else Denied "authorization failed"

let rec recheck_capability (ctx:auth_context) (cap:capability) : bool =
  authorization_grants ctx cap

let rec secure_capability_bundle (ctx:auth_context) : list capability =
  ctx.capabilities

let rec validated_capability_context (ctx:auth_context) : bool =
  principal_is_known ctx && capability_binding_ok ctx

let rec capability_policy_fails_closed (ctx:auth_context) (needed:capability) : policy_result =
  if validated_capability_context ctx && authorization_grants ctx needed then Allowed else Denied "fail-closed"

let rec effective_policy (ctx:auth_context) (needed:capability) : policy_result =
  if valid_principal ctx.principal then enforce_policy ctx needed default_guard else Denied "principal invalid"

let rec policy_result_ok (r:policy_result) : bool =
  match r with
  | Allowed -> true
  | Denied _ -> false

let rec deny_if_invalid (ctx:auth_context) (needed:capability) : policy_result =
  if valid_principal ctx.principal then enforce_policy ctx needed default_guard else Denied "invalid context"

let rec capability_limit_ok (cap:resource_cap) : bool =
  cap.max_calls > 0 && cap.max_seconds >= 0 && cap.max_bytes >= 0

let rec default_resource_cap : resource_cap =
  { max_calls = 100; max_bytes = 1048576; max_seconds = 60 }

let rec protected_transition (ctx:auth_context) (t:secure_transition) : policy_result =
  if valid_transition t then transition_requires t ctx else Denied "invalid transition"

let rec audited_transition (state:protocol_state) (ctx:auth_context) (t:secure_transition) : protocol_state =
  if allowed_transition ctx t then append_audit state (audit_event ctx.principal "transition" true) else fail_transition state ctx.principal "transition"

let rec zero_capability_context : auth_context =
  { principal = "system"; capabilities = [] }

let rec context_is_secure (ctx:auth_context) : bool =
  valid_principal ctx.principal

let rec state_is_secure (ctx:auth_context) (state:protocol_state) : bool =
  context_is_secure ctx && secure_protocol_state state

let rec capability_authority (ctx:auth_context) (needed:capability) : bool =
  authorization_grants ctx needed

let rec authorization_is_fail_closed (ctx:auth_context) (needed:capability) : policy_result =
  if capability_authority ctx needed then Allowed else Denied "fail-closed"

let rec enforce_transition (ctx:auth_context) (t:secure_transition) : policy_result =
  transition_requires t ctx

let rec resource_and_auth_ok (ctx:auth_context) (cap:resource_cap) (needed:capability) : bool =
  resource_limit_ok cap && authorization_grants ctx needed

let rec secure_call_result (ctx:auth_context) (needed:capability) (cap:resource_cap) : policy_result =
  if resource_and_auth_ok ctx cap needed then Allowed else Denied "security policy rejected"

let _ = assert (authorization_grants { principal = "alice"; capabilities = ["read"] } "read")
let _ = assert (enforce_policy { principal = "alice"; capabilities = ["read"] } "read" default_guard = Allowed)
let _ = assert (deny_if_not_granted { principal = "alice"; capabilities = [] } "write" = Denied "capability not granted")
let _ = assert (resource_limit_ok default_resource_cap)
