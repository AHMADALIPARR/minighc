module ToolRegistry

open FStar.List.Tot

let empty_string : string = ""

type nonempty_string = s:string{ s <> "" }
type tool_id = nonempty_string
type capability = nonempty_string
type param_name = nonempty_string
type parameter_value = string

type param_kind =
  | IntKind
  | BoolKind
  | StringKind
  | PathKind
  | JsonKind

type parameter_spec =
  { name:param_name;
    kind:param_kind;
    required:bool;
    min_value:option int;
    max_value:option int;
    default_value:option parameter_value }

type tool_spec =
  { id:tool_id;
    name:string;
    summary:string;
    capabilities:list capability;
    params:list parameter_spec }

type registry =
  { version:nat;
    entries:list tool_spec }

type validation_result =
  | ValidationOk
  | ValidationError of string

let rec list_length (#a:Type) (xs:list a) : nat =
  match xs with
  | [] -> 0
  | _::tl -> 1 + list_length tl

let rec contains_tool (id:tool_id) (tools:list tool_spec) : bool =
  match tools with
  | [] -> false
  | h::t -> if h.id = id then true else contains_tool id t

let rec unique_capabilities (caps:list capability) : list capability =
  match caps with
  | [] -> []
  | h::t ->
    let rest = unique_capabilities t in
    if List.mem h rest then rest else h :: rest

let rec parameter_schema_valid (p:parameter_spec) : bool =
  match p.min_value, p.max_value with
  | Some lo, Some hi -> lo <= hi
  | _, _ -> true

let rec parameter_specs_valid (ps:list parameter_spec) : bool =
  match ps with
  | [] -> true
  | p::t -> parameter_schema_valid p && parameter_specs_valid t

let rec register_tool (r:registry) (t:tool_spec) : Pure registry
  (requires (not (contains_tool t.id r.entries) && t.id <> "" && t.name <> "" && parameter_specs_valid t.params))
  (ensures (fun r' -> r'.version = r.version + 1 && contains_tool t.id r'.entries)) =
  { version = r.version + 1; entries = t :: r.entries }

let empty_registry : registry = { version = 0; entries = [] }

let rec capability_present (cap:capability) (caps:list capability) : bool =
  List.mem cap caps

let rec capability_guard (tool:tool_spec) (granted:list capability) : bool =
  let rec aux (caps:list capability) : bool =
    match caps with
    | [] -> true
    | h::t -> capability_present h granted && aux t
  in
  aux tool.capabilities

let rec validate_parameter (p:parameter_spec) (value:string) : validation_result =
  if p.required && value = "" then ValidationError (p.name ^ ": required parameter missing")
  else match p.kind with
  | IntKind ->
    (match p.min_value, p.max_value with
     | Some lo, Some hi ->
       if value = "" then ValidationError (p.name ^ ": integer value missing")
       else ValidationOk
     | _, _ -> ValidationOk)
  | BoolKind ->
    if value = "true" || value = "false" then ValidationOk
    else ValidationError (p.name ^ ": bool value must be true or false")
  | StringKind ->
    if value = "" && p.required then ValidationError (p.name ^ ": string value missing") else ValidationOk
  | PathKind ->
    if value = "" && p.required then ValidationError (p.name ^ ": path missing") else ValidationOk
  | JsonKind ->
    if value = "" && p.required then ValidationError (p.name ^ ": json payload missing") else ValidationOk

let rec validate_tool_params (tool:tool_spec) (provided:list (param_name * string)) : validation_result =
  let rec lookup (name:param_name) (values:list (param_name * string)) : option string =
    match values with
    | [] -> None
    | (n,v)::rest -> if n = name then Some v else lookup name rest
  in
  let rec check_params (ps:list parameter_spec) : validation_result =
    match ps with
    | [] -> ValidationOk
    | p::rest ->
      (match lookup p.name provided with
       | Some v ->
         (match validate_parameter p v with
          | ValidationOk -> check_params rest
          | ValidationError e -> ValidationError e)
       | None ->
         if p.required then ValidationError (p.name ^ ": missing required argument")
         else check_params rest)
  in
  check_params tool.params

let rec registry_is_well_formed (r:registry) : bool =
  match r.entries with
  | [] -> true
  | h::t -> parameter_specs_valid h.params && registry_is_well_formed { version = r.version; entries = t }

let rec default_value_for (p:parameter_spec) : option string = p.default_value

let rec make_parameter
  (name:param_name)
  (kind:param_kind)
  (required:bool)
  (min_value:option int)
  (max_value:option int)
  (default_value:option string)
  : parameter_spec =
  { name = name; kind = kind; required = required; min_value = min_value; max_value = max_value; default_value = default_value }

let rec make_tool
  (id:tool_id)
  (name:string)
  (summary:string)
  (capabilities:list capability)
  (params:list parameter_spec)
  : tool_spec =
  { id = id; name = name; summary = summary; capabilities = capabilities; params = params }

let rec registry_capabilities (r:registry) : list capability =
  match r.entries with
  | [] -> []
  | h::t -> unique_capabilities (h.capabilities @ registry_capabilities { version = r.version; entries = t })

let rec registry_compatible (r:registry) (required:list capability) : bool =
  let available = registry_capabilities r in
  let rec aux (need:list capability) : bool =
    match need with
    | [] -> true
    | h::t -> capability_present h available && aux t
  in
  aux required

let rec registry_total_params (r:registry) : nat =
  match r.entries with
  | [] -> 0
  | h::t -> list_length h.params + registry_total_params { version = r.version; entries = t }

let rec capability_count (caps:list capability) : nat = list_length (unique_capabilities caps)

let rec dispatch_allowed (tool:tool_spec) (granted:list capability) : bool =
  let rec aux (caps:list capability) : bool =
    match caps with
    | [] -> true
    | h::t -> capability_present h granted && aux t
  in
  aux tool.capabilities

let rec valid_tool_registration (r:registry) (t:tool_spec) : bool =
  not (contains_tool t.id r.entries) && parameter_specs_valid t.params && registry_is_well_formed r

let rec policy_result_for (tool:tool_spec) (granted:list capability) : validation_result =
  if dispatch_allowed tool granted then ValidationOk else ValidationError (tool.name ^ ": capability policy denied")

let rec tool_lookup (r:registry) (id:tool_id) : option tool_spec =
  match r.entries with
  | [] -> None
  | h::t -> if h.id = id then Some h else tool_lookup { version = r.version; entries = t } id

let rec bind_default_values (p:parameter_spec) : parameter_spec =
  { p with default_value = Some (match p.default_value with | Some v -> v | None -> "") }

let rec registry_version_guard (r:registry) : bool =
  true

let rec tool_security_ok (tool:tool_spec) (granted:list capability) : bool =
  dispatch_allowed tool granted && parameter_specs_valid tool.params

let rec tool_registry_summary (r:registry) : nat =
  list_length r.entries + registry_total_params r + capability_count (registry_capabilities r)

let rec query_tool (r:registry) (id:tool_id) : option tool_spec =
  tool_lookup r id

let rec default_registry : registry = empty_registry

let _ = assert (registry_is_well_formed empty_registry)
let _ = assert (not (contains_tool "tool-1" []))
let _ = assert (validate_parameter (make_parameter "limit" IntKind true (Some 0) (Some 100) (Some "10")) "20" = ValidationOk)
let _ = assert (validate_parameter (make_parameter "flag" BoolKind true None None None) "false" = ValidationOk)
let _ = assert (dispatch_allowed (make_tool "tool-1" "Tool One" "first tool" ["read"; "write"] []) ["read"]) 

















































































































































































