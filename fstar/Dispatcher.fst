(* Dispatcher: the tool registry, the security contracts, and the
   execution engine wired into one dispatch path.

   The three modules were written as separate scaffolds with no
   connection between them: the engine dispatched plans without ever
   looking at a tool, a capability, a parameter, or a policy. This
   module is the connection. Dispatch is a gate chain, in order, and
   every failure is a rejection with a specific reason, never a silent
   fallback:

     1. the tool must be registered (lookup by id);
     2. every capability the tool declares must be granted;
     3. every provided parameter must validate against its spec,
        including integer ranges;
     4. the caller must be authorized for every capability the tool
        declares, and the resource cap must be well-formed;
     5. the engine's own dispatch policy and plan validity must hold.

   Only then does the plan run, and the dispatch itself is audited.

   Deliberately, this module opens none of the three modules: all three
   define names like capability, capability_present, and list_length,
   so every reference below is qualified. Capability comparison across
   the registry and the security model is done at the string level
   (str_mem), so no fragile refinement-subtyping between the two
   capability types is ever needed: a granted capability is a string
   that appears in the caller's list, and that is the whole check.

   The engine gate uses dispatch_policy and plan_valid directly rather
   than try_dispatch, because try_dispatch already advances the state
   once and run_plan advances once per step; routing through both would
   double-count the dispatch step. *)

module Dispatcher

open FStar.Tactics

(* String-level membership: is c present in a list of security-model
   capabilities? Both sides are strings underneath their refinements,
   so equality here needs no cross-module type coercion. *)
let rec str_mem (c:string) (caps:list SecurityContracts.capability) : bool =
  match caps with
  | [] -> false
  | h::t -> if h = c then true else str_mem c t

(* Every capability a tool declares must be authorized for the caller. *)
let rec all_authorized (ctx:SecurityContracts.auth_context) (caps:list ToolRegistry.capability) : bool =
  match caps with
  | [] -> true
  | h::t -> str_mem h ctx.capabilities && all_authorized ctx t

(* The first declared capability the caller lacks, if any, for the
   rejection message. *)
let rec first_missing_cap (ctx:SecurityContracts.auth_context) (caps:list ToolRegistry.capability) : option string =
  match caps with
  | [] -> None
  | h::t -> if str_mem h ctx.capabilities then first_missing_cap ctx t else Some h

let dispatch_tool
  (r:ToolRegistry.registry)
  (id:ToolRegistry.tool_id)
  (granted:list ToolRegistry.capability)
  (ctx:SecurityContracts.auth_context)
  (provided:list (ToolRegistry.param_name & string))
  (cap:SecurityContracts.resource_cap)
  (s:ExecutionEngine.execution_state)
  (p:ExecutionEngine.execution_plan)
  : ExecutionEngine.dispatch_result =
  match ToolRegistry.tool_lookup r id with
  | None -> ExecutionEngine.Rejected "tool not found"
  | Some tool ->
    if not (ToolRegistry.dispatch_allowed tool granted) then
      ExecutionEngine.Rejected (tool.name ^ ": capability policy denied")
    else
      match ToolRegistry.validate_tool_params tool provided with
      | ToolRegistry.ValidationError e -> ExecutionEngine.Rejected e
      | ToolRegistry.ValidationOk ->
        (match first_missing_cap ctx tool.capabilities with
         | Some missing -> ExecutionEngine.Rejected (missing ^ ": capability missing")
         | None ->
           (match SecurityContracts.check_resource_cap cap with
            | SecurityContracts.Denied reason -> ExecutionEngine.Rejected reason
            | SecurityContracts.Allowed ->
              if not (ExecutionEngine.dispatch_policy s p && ExecutionEngine.plan_valid p) then
                ExecutionEngine.Rejected "dispatch rejected"
              else
                let s' = ExecutionEngine.run_plan s p in
                ExecutionEngine.Dispatched (ExecutionEngine.add_audit s' ("dispatch:" ^ tool.id))))

(* Fixtures for the checks below: one registered tool, one caller that
   holds its capability, the default resource cap, and a two-step plan. *)
let test_tool : ToolRegistry.tool_spec =
  ToolRegistry.make_tool "tool-1" "Tool One" "first tool" ["read"]
    [ToolRegistry.make_parameter "limit" ToolRegistry.IntKind true (Some 0) (Some 100) (Some "10")]

let test_registry : ToolRegistry.registry =
  ToolRegistry.register_tool ToolRegistry.empty_registry test_tool

let test_ctx : SecurityContracts.auth_context =
  { principal = "alice"; capabilities = ["read"] }

let test_plan : ExecutionEngine.execution_plan =
  ExecutionEngine.step_plan "plan" "demo" ["a"; "b"]

let _ = assert (match dispatch_tool test_registry "tool-1" ["read"] test_ctx [("limit", "20")] SecurityContracts.default_resource_cap (ExecutionEngine.initial_state "job-1") test_plan with
  | ExecutionEngine.Dispatched _ -> true
  | ExecutionEngine.Rejected _ -> false) by (norm [delta; iota; zeta; primops])

let _ = assert (match dispatch_tool test_registry "nope" ["read"] test_ctx [("limit", "20")] SecurityContracts.default_resource_cap (ExecutionEngine.initial_state "job-1") test_plan with
  | ExecutionEngine.Rejected "tool not found" -> true
  | _ -> false) by (norm [delta; iota; zeta; primops])

let _ = assert (match dispatch_tool test_registry "tool-1" [] test_ctx [("limit", "20")] SecurityContracts.default_resource_cap (ExecutionEngine.initial_state "job-1") test_plan with
  | ExecutionEngine.Rejected "Tool One: capability policy denied" -> true
  | _ -> false) by (norm [delta; iota; zeta; primops])

let _ = assert (match dispatch_tool test_registry "tool-1" ["read"] test_ctx [("limit", "200")] SecurityContracts.default_resource_cap (ExecutionEngine.initial_state "job-1") test_plan with
  | ExecutionEngine.Rejected "limit: integer value out of range" -> true
  | _ -> false) by (norm [delta; iota; zeta; primops])

let _ = assert (match dispatch_tool test_registry "tool-1" ["read"] { principal = "alice"; capabilities = [] } [("limit", "20")] SecurityContracts.default_resource_cap (ExecutionEngine.initial_state "job-1") test_plan with
  | ExecutionEngine.Rejected "read: capability missing" -> true
  | _ -> false) by (norm [delta; iota; zeta; primops])
