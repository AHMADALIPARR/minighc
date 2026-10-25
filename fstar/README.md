module SecurityContracts

open FStar.List.Tot

let empty_string : string = ""

type principal = s:string{ s <> "" }
type capability = s:string{ s <> "" }
type action = s:string{ s <> "" }

type auth_context =
  { principal:principal;
    capabilities:list capability }

type policy_result =
  | Allowed
  | Denied of string

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

let rec capability_present (cap:capability) (caps:list capability) : bool =
  List.mem cap caps

let rec authorizes (ctx:auth_context) (needed:capability) : bool =
  capability_present needed ctx.capabilities

let rec enforce_policy (ctx:auth_context) (needed:capability) (g:state_guard) : policy_result =
  if g.require_auth && ctx.principal = "" then Denied "missing principal"
  else if g.require_capability && not (authorizes ctx needed) then Denied (needed ^ ": capability missing")
  else Allowed

let rec fail_closed_policy (ctx:auth_context) (needed:capability) (g:state_guard) : policy_result =
  if g.deny_on_failure then enforce_policy ctx needed g else Allowed

let rec default_guard : state_guard =
  { require_auth = true; require_capability = true; deny_on_failure = true }

let rec grant_capability (ctx:auth_context) (cap:capability) : auth_context =
  { principal = ctx.principal; capabilities = cap :: ctx.capabilities }

let rec transition_requires (t:secure_transition) (ctx:auth_context) : policy_result =
  if authorizes ctx t.required_capability then Allowed
  else Denied (t.required_capability ^ ": transition denied")

let rec resource_cap_ok (cap:resource_cap) : bool =
  cap.max_calls > 0 && cap.max_bytes > 0 && cap.max_seconds > 0

let rec transition_is_valid (t:secure_transition) : bool =
  t.from_state <> "" && t.to_state <> "" && t.required_capability <> ""

let rec audit_event (principal:principal) (action:action) (success:bool) : audit_record =
  { event = "audit"; principal = principal; action = action; success = success }

let rec append_audit (state:protocol_state) (record:audit_record) : protocol_state =
  { state with step = state.step + 1; audited = record :: state.audited }

let rec protocol_is_auditable (state:protocol_state) : bool =
  state.active && list_length state.audited > 0

let rec secure_protocol_state (state:protocol_state) : bool =
  state.active = true || state.active = false

let rec capability_context_ok (ctx:auth_context) : bool =
  ctx.principal <> ""

let rec deny_by_default (ctx:auth_context) (needed:capability) : policy_result =
  if authorizes ctx needed then Allowed else Denied "deny-by-default"

let rec protected_transition (ctx:auth_context) (t:secure_transition) : policy_result =
  if transition_is_valid t && capability_context_ok ctx then transition_requires t ctx
  else Denied "invalid transition"

let rec resource_and_auth_ok (ctx:auth_context) (cap:resource_cap) (needed:capability) : bool =
  resource_cap_ok cap && authorizes ctx needed

let rec secure_call_result (ctx:auth_context) (cap:resource_cap) (needed:capability) : policy_result =
  if resource_and_auth_ok ctx cap needed then Allowed else Denied "security policy rejected"

let _ = assert (authorizes { principal = "alice"; capabilities = ["read"] } "read")
let _ = assert (enforce_policy { principal = "alice"; capabilities = ["read"] } "read" default_guard = Allowed)
let _ = assert (resource_cap_ok { max_calls = 10; max_bytes = 1024; max_seconds = 60 })
let _ = assert (transition_requires { from_state = "idle"; to_state = "running"; required_capability = "execute" } { principal = "bob"; capabilities = ["execute"] } = Allowed)
