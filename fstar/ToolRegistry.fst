module ToolRegistry

open FStar.List.Tot
open FStar.String
open FStar.Tactics

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

let parameter_schema_valid (p:parameter_spec) : bool =
  match p.min_value, p.max_value with
  | Some lo, Some hi -> lo <= hi
  | _, _ -> true

let rec parameter_specs_valid (ps:list parameter_spec) : bool =
  match ps with
  | [] -> true
  | p::t -> parameter_schema_valid p && parameter_specs_valid t

let register_tool (r:registry) (t:tool_spec) : Pure registry
  (requires (not (contains_tool t.id r.entries) && t.id <> "" && t.name <> "" && parameter_specs_valid t.params))
  (ensures (fun r' -> r'.version = r.version + 1 && contains_tool t.id r'.entries)) =
  { version = r.version + 1; entries = t :: r.entries }

let empty_registry : registry = { version = 0; entries = [] }

let capability_present (cap:capability) (caps:list capability) : bool =
  List.mem cap caps

(* A tool may be dispatched exactly when every capability it declares
   has been granted. Defined here, before its policy alias, because F*
   definitions must precede their uses. *)
let dispatch_allowed (tool:tool_spec) (granted:list capability) : bool =
  let rec aux (caps:list capability) : bool =
    match caps with
    | [] -> true
    | h::t -> capability_present h granted && aux t
  in
  aux tool.capabilities

(* capability_guard is dispatch_allowed under its policy name. Kept as
   a named alias so policy call sites read as policy. *)
let capability_guard (tool:tool_spec) (granted:list capability) : bool =
  dispatch_allowed tool granted

(* Integer parsing. Strings are parsed digit by digit (an optional
   leading minus, then digits only) so that min_value / max_value are
   real bounds, not documentation. The loop is driven by `remaining`,
   the number of characters left, not by String.length: the refinement
   on `remaining` (i + remaining = length s) is what lets the checker
   prove the substring precondition and termination, and it is also
   what lets the solver actually compute the parser on literals — a
   loop guarded by String.length on a variable does not reduce in SMT,
   while this countdown shape does. A value that is empty, non-numeric,
   or out of range is an error, never a silent acceptance. *)
let digit_value (d:string) : option int =
  if d = "0" then Some 0
  else if d = "1" then Some 1
  else if d = "2" then Some 2
  else if d = "3" then Some 3
  else if d = "4" then Some 4
  else if d = "5" then Some 5
  else if d = "6" then Some 6
  else if d = "7" then Some 7
  else if d = "8" then Some 8
  else if d = "9" then Some 9
  else None

let rec parse_digits (s:string) (i:nat) (acc:int) (remaining:nat{i + remaining = String.length s}) : Tot (option int) (decreases remaining) =
  if remaining = 0 then Some acc
  else
    match digit_value (String.sub s i 1) with
    | Some v -> parse_digits s (i + 1) (acc * 10 + v) (remaining - 1)
    | None -> None

let parse_int (s:string) : option int =
  if String.length s = 0 then None
  else if String.sub s 0 1 = "-" then
    if String.length s <= 1 then None
    else
      match parse_digits s 1 0 (String.length s - 1) with
      | Some n -> Some (0 - n)
      | None -> None
  else parse_digits s 0 0 (String.length s)

let validate_parameter (p:parameter_spec) (value:string) : validation_result =
  if p.required && value = "" then ValidationError (p.name ^ ": required parameter missing")
  else match p.kind with
  | IntKind ->
    (match parse_int value with
     | None -> ValidationError (p.name ^ ": integer value invalid")
     | Some v ->
       (match p.min_value, p.max_value with
        | Some lo, Some hi ->
          if lo <= v && v <= hi then ValidationOk
          else ValidationError (p.name ^ ": integer value out of range")
        | Some lo, None ->
          if lo <= v then ValidationOk
          else ValidationError (p.name ^ ": integer value below minimum")
        | None, Some hi ->
          if v <= hi then ValidationOk
          else ValidationError (p.name ^ ": integer value above maximum")
        | None, None -> ValidationOk))
  | BoolKind ->
    if value = "true" || value = "false" then ValidationOk
    else ValidationError (p.name ^ ": bool value must be true or false")
  | StringKind ->
    if value = "" && p.required then ValidationError (p.name ^ ": string value missing") else ValidationOk
  | PathKind ->
    if value = "" && p.required then ValidationError (p.name ^ ": path missing") else ValidationOk
  | JsonKind ->
    if value = "" && p.required then ValidationError (p.name ^ ": json payload missing") else ValidationOk

let validate_tool_params (tool:tool_spec) (provided:list (param_name & string)) : validation_result =
  let rec lookup (name:param_name) (values:list (param_name & string)) : option string =
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

(* Duplicate tool ids are not well-formed: lookup returns the first
   match, so a duplicate would silently shadow the second registration.
   register_tool already refuses duplicates in its precondition; the
   well-formedness check makes the same rule hold for registries built
   any other way. *)
let rec no_duplicate_tools (tools:list tool_spec) : bool =
  match tools with
  | [] -> true
  | h::t -> not (contains_tool h.id t) && no_duplicate_tools t

(* Well-formedness is a property of the entry list alone, never of the
   version: the schema check recurses on the list itself, so a registry
   built by registration (which bumps the version) satisfies exactly the
   same check as the registry it came from. Threading the version
   through the recursion would make that fact unprovable, since the
   recursive call would concern a different version than the hypothesis. *)
let rec entries_well_formed (tools:list tool_spec) : bool =
  match tools with
  | [] -> true
  | h::t -> parameter_specs_valid h.params && entries_well_formed t

let registry_is_well_formed (r:registry) : bool =
  no_duplicate_tools r.entries && entries_well_formed r.entries

let default_value_for (p:parameter_spec) : option string = p.default_value

let make_parameter
  (name:param_name)
  (kind:param_kind)
  (required:bool)
  (min_value:option int)
  (max_value:option int)
  (default_value:option string)
  : parameter_spec =
  { name = name; kind = kind; required = required; min_value = min_value; max_value = max_value; default_value = default_value }

let make_tool
  (id:tool_id)
  (name:string)
  (summary:string)
  (capabilities:list capability)
  (params:list parameter_spec)
  : tool_spec =
  { id = id; name = name; summary = summary; capabilities = capabilities; params = params }

let rec registry_capabilities (r:registry) : Tot (list capability) (decreases r.entries) =
  match r.entries with
  | [] -> []
  | h::t -> unique_capabilities (h.capabilities @ registry_capabilities { version = r.version; entries = t })

let registry_compatible (r:registry) (required:list capability) : bool =
  let available = registry_capabilities r in
  let rec aux (need:list capability) : bool =
    match need with
    | [] -> true
    | h::t -> capability_present h available && aux t
  in
  aux required

let rec registry_total_params (r:registry) : Tot nat (decreases r.entries) =
  match r.entries with
  | [] -> 0
  | h::t -> list_length h.params + registry_total_params { version = r.version; entries = t }

let capability_count (caps:list capability) : nat = list_length (unique_capabilities caps)

let valid_tool_registration (r:registry) (t:tool_spec) : bool =
  not (contains_tool t.id r.entries) && parameter_specs_valid t.params && registry_is_well_formed r

let policy_result_for (tool:tool_spec) (granted:list capability) : validation_result =
  if dispatch_allowed tool granted then ValidationOk else ValidationError (tool.name ^ ": capability policy denied")

let rec tool_lookup (r:registry) (id:tool_id) : Tot (option tool_spec) (decreases r.entries) =
  match r.entries with
  | [] -> None
  | h::t -> if h.id = id then Some h else tool_lookup { version = r.version; entries = t } id

let bind_default_values (p:parameter_spec) : parameter_spec =
  { p with default_value = Some (match p.default_value with | Some v -> v | None -> "") }

(* The version counts registrations: a registry built by register_tool
   from empty always has version = number of entries, because every
   registration increments both by exactly one. A registry whose version
   disagrees with its entry count was not built by registration and
   fails the guard. *)
let registry_version_guard (r:registry) : bool =
  r.version = list_length r.entries

let tool_security_ok (tool:tool_spec) (granted:list capability) : bool =
  dispatch_allowed tool granted && parameter_specs_valid tool.params

let tool_registry_summary (r:registry) : nat =
  list_length r.entries + registry_total_params r + capability_count (registry_capabilities r)

(* query_tool is tool_lookup under its query name; one implementation,
   not two copies of the same search. *)
let query_tool (r:registry) (id:tool_id) : option tool_spec =
  tool_lookup r id

(* Registration preserves the two registry invariants: well-formedness
   (valid parameter schemas, no duplicate ids) and the version guard
   (version = entry count). Both follow from the precondition plus the
   definitions; the solver discharges them by unfolding. *)
let register_preserves_well_formed (r:registry) (t:tool_spec)
  : Lemma
    (requires (not (contains_tool t.id r.entries) && t.id <> "" && t.name <> "" && parameter_specs_valid t.params && registry_is_well_formed r))
    (ensures (registry_is_well_formed (register_tool r t))) =
  ()

let register_preserves_version_guard (r:registry) (t:tool_spec)
  : Lemma
    (requires (not (contains_tool t.id r.entries) && t.id <> "" && t.name <> "" && parameter_specs_valid t.params && registry_version_guard r))
    (ensures (registry_version_guard (register_tool r t))) =
  ()

let default_registry : registry = empty_registry

let _ = assert (registry_is_well_formed empty_registry)
let _ = assert (not (contains_tool "tool-1" []))
(* Integer asserts go through the normalizer, not plain SMT: SMT does not
   reduce a recursive parser over string primitives, but the normalizer
   with primops does — it evaluates String.sub and String.length on
   literals and unfolds the parser, leaving a trivial equality. *)
let _ = assert (validate_parameter (make_parameter "limit" IntKind true (Some 0) (Some 100) (Some "10")) "20" = ValidationOk) by (norm [delta; iota; zeta; primops])
let _ = assert (validate_parameter (make_parameter "flag" BoolKind true None None None) "false" = ValidationOk)
(* The old final assertion granted only "read" to a tool requiring
   "read" and "write", so it asserted false and the file could never
   verify. The grant below matches the requirement. *)
let _ = assert (dispatch_allowed (make_tool "tool-1" "Tool One" "first tool" ["read"; "write"] []) ["read"; "write"]) by (norm [delta; iota; zeta; primops])
let _ = assert (not (dispatch_allowed (make_tool "tool-1" "Tool One" "first tool" ["read"; "write"] []) ["read"])) by (norm [delta; iota; zeta; primops])
let _ = assert (validate_parameter (make_parameter "limit" IntKind true (Some 0) (Some 100) (Some "10")) "200" = ValidationError "limit: integer value out of range") by (norm [delta; iota; zeta; primops])
let _ = assert (validate_parameter (make_parameter "limit" IntKind true (Some 0) (Some 100) (Some "10")) "abc" = ValidationError "limit: integer value invalid") by (norm [delta; iota; zeta; primops])
let _ = assert (validate_parameter (make_parameter "limit" IntKind true (Some 0) (Some 100) (Some "10")) "-5" = ValidationError "limit: integer value out of range") by (norm [delta; iota; zeta; primops])
let _ = assert (validate_parameter (make_parameter "limit" IntKind true (Some 0) (Some 100) (Some "10")) "100" = ValidationOk) by (norm [delta; iota; zeta; primops])
let _ = assert (not (registry_is_well_formed { version = 2; entries = [make_tool "tool-1" "Tool One" "first tool" [] []; make_tool "tool-1" "Tool One" "first tool" [] []] }))
let _ = assert (registry_version_guard empty_registry)
let _ = assert (registry_version_guard (register_tool empty_registry (make_tool "tool-1" "Tool One" "first tool" [] [])))
let _ = assert (query_tool (register_tool empty_registry (make_tool "tool-1" "Tool One" "first tool" [] [])) "tool-1" = Some (make_tool "tool-1" "Tool One" "first tool" [] [])) 
