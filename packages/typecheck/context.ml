open Core

type entry =
  | Untyped of Term.value
  | Typed of Term.value * Term.value
[@@deriving sexp]

let entry_value : entry -> Term.value = function
  | Untyped v -> v
  | Typed (v, _) -> v
;;

let entry_typ : entry -> Term.value option = function
  | Typed (_, t) -> Some t
  | _ -> None
;;

(** The reader part of the checker's environment. Scoped via nested deep
    handlers: a binding's lifetime is exactly its handler's extent, so it is
    impossible for a binding to leak into a sibling scope. *)
type scope =
  { bindings : entry Ident.Map.t
  ; pos : Pos.t
  ; level : int
  }

(** A datatype constructor's elaboration artifacts (design/type_system.md §3):
    the nominal Pi type used for checking applications and patterns, and the
    Scott-encoded delta body used only by conversion. *)
type ctor_info =
  { ctor_name : Ident.t
  ; datatype : Ident.t
  ; tag : int (* index in Map.to_alist order; must match Ir.CtorTags *)
  ; arity : int (* number of explicit fields *)
  ; ctor_type : Term.value
  ; encoding : Term.value
  }

type data_info =
  { data_name : Ident.t
  ; param_arity : int
  ; kind : Term.value
  ; ctors : ctor_info list (* in tag order *)
  ; encoding : Term.value
  ; record_fields : Ident.t list option
    (* [Some names]: a record declaration; the single ctor's field i is named
       [List.nth names i]. [None]: an ordinary sum. *)
  }

type definition =
  | Data of data_info
  | Ctor of ctor_info

(** The state part: threaded purely through [run]'s handler as a
    [state -> 'r] answer function. No refs.

    [definitions] lives in state rather than in scope entries because it must
    be visible during the final [Solve.solve] call, which runs after all
    lexical [with_binding] scopes from inference have unwound but still inside
    [run]. [Solve.solve] must therefore remain inside [Context.run]. *)
type state =
  { meta_gen : Term.Meta.Id.t
  ; errors : Calyx_error.t list
  ; constraints : Constraint.t list
  ; definitions : definition Ident.Map.t
  }

type _ Effect.t +=
  | Scope : scope Effect.t
  | Tell_error : Calyx_error.t -> unit Effect.t
  | Tell_constraint : Constraint.t -> unit Effect.t
  | Take_constraints : Constraint.t list Effect.t
  | Has_constraints : bool Effect.t
  | Fresh_meta : int -> Term.Meta.t Effect.t
  | Define : Ident.t * definition -> unit Effect.t
  | Lookup_defn : Ident.t -> definition option Effect.t

(** Fatal checker failure; caught by [run] (and by [close] at HOAS closure
    boundaries). *)
exception Fail of Calyx_error.t

let fail : Calyx_error.t -> 'a = fun e -> raise (Fail e)

let lift_r : ('a, Calyx_error.t) result -> 'a = function
  | Ok x -> x
  | Error e -> fail e
;;

let lift_o_or_fail : error:Calyx_error.t -> 'a option -> 'a =
  fun ~error -> function
  | Some x -> x
  | None -> fail error
;;

(* {1 Reader} *)

let scope () : scope = Effect.perform Scope
let level () : int = (scope ()).level
let pos () : Pos.t = (scope ()).pos
let bindings () : entry Ident.Map.t = (scope ()).bindings
let lookup : Ident.t -> entry option = fun ident -> Map.find (bindings ()) ident

let lookup_value : Ident.t -> Term.value option =
  fun ident -> Option.map ~f:entry_value (lookup ident)
;;

let lookup_type : Ident.t -> Term.value option =
  fun ident -> Option.bind ~f:entry_typ (lookup ident)
;;

let is_bound : Ident.t -> bool = fun ident -> Option.is_some (lookup ident)

(** Run [f] with [sc] as the current scope. The handler is deep ([try_with]),
    so it persists for every [Scope] perform within [f] and vanishes when [f]
    returns. *)
let under_scope : type a. scope -> (unit -> a) -> a =
  fun sc f ->
  let open Effect.Deep in
  try f () with 
  | effect Scope, k -> continue k sc
;;

let with_scope : f:(scope -> scope) -> (unit -> 'a) -> 'a =
  fun ~f body -> under_scope (f (scope ())) body
;;

let entry_of ~value ?typ () =
  match typ with
  | Some t -> Typed (value, t)
  | None -> Untyped value
;;

let with_binding : Ident.t -> value:Term.value -> ?typ:Term.value -> (unit -> 'a) -> 'a =
  fun ident ~value ?typ body ->
  let data = entry_of ~value ?typ () in
  with_scope
    ~f:(fun sc -> { sc with bindings = Map.set sc.bindings ~key:ident ~data })
    body
;;

let with_bindings : (Ident.t * entry) list -> (unit -> 'a) -> 'a =
  fun entries body ->
  with_scope
    ~f:(fun sc ->
      { sc with
        bindings =
          List.fold entries ~init:sc.bindings ~f:(fun acc (key, data) ->
            Map.set acc ~key ~data)
      })
    body
;;

let with_pos : Pos.t -> (unit -> 'a) -> 'a =
  fun pos body -> with_scope ~f:(fun sc -> { sc with pos }) body
;;

(** Bind [ident] to a fresh neutral at the current level, increment the level
    within the extent of [f]. *)
let with_var : Ident.t -> typ:Term.value -> f:(Term.value -> 'a) -> 'a =
  fun ident ~typ ~f ->
  let sc = scope () in
  let var = `Neutral (Term.NVar (sc.level, ident)) in
  under_scope
    { sc with
      level = sc.level + 1
    ; bindings = Map.set sc.bindings ~key:ident ~data:(Typed (var, typ))
    }
    (fun () -> f var)
;;

(** Capture the current scope into an OCaml closure (HOAS). At invocation time
    the snapshot scope is re-installed, so the body sees its lexical
    environment; state effects (constraints, errors, metas) forward to
    whichever [run] is live at invocation. *)
let close : f:('a -> 'b) -> 'a -> ('b, Calyx_error.t) result =
  fun ~f ->
  let snapshot = scope () in
  fun x ->
    match under_scope snapshot (fun () -> f x) with
    | v -> Ok v
    | exception Fail e -> Error e
;;

(* {1 State} *)

let tell_error : Calyx_error.t -> unit = fun e -> Effect.perform (Tell_error e)
let tell_constraint : Constraint.t -> unit = fun c -> Effect.perform (Tell_constraint c)

let take_constraints : unit -> Constraint.t list =
  fun () -> Effect.perform Take_constraints
;;

let has_constraints : unit -> bool = fun () -> Effect.perform Has_constraints
let fresh_meta : unit -> Term.Meta.t = fun () -> Effect.perform (Fresh_meta (level ()))

(* {1 Definitions} *)

let define : Ident.t -> definition -> unit =
  fun ident defn -> Effect.perform (Define (ident, defn))
;;

let lookup_defn : Ident.t -> definition option =
  fun ident -> Effect.perform (Lookup_defn ident)
;;

let lookup_data : Ident.t -> data_info option =
  fun ident ->
  match lookup_defn ident with
  | Some (Data info) -> Some info
  | _ -> None
;;

let lookup_ctor : Ident.t -> ctor_info option =
  fun ident ->
  match lookup_defn ident with
  | Some (Ctor info) -> Some info
  | _ -> None
;;

let lookup_record : Ident.t -> (data_info * ctor_info * Ident.t list) option =
  fun ident ->
  match lookup_data ident with
  | Some ({ record_fields = Some names; ctors = [ ctor ]; _ } as info) ->
    Some (info, ctor, names)
  | _ -> None
;;

(* {1 Tracing} *)

let trace : type i o. (i, o) Trace.stage -> i -> Lexing.position -> (unit -> o) -> o =
  fun stage focus here f ->
  let sc = scope () in
  let context =
    lazy
      (Map.to_alist sc.bindings
       |> List.map ~f:(function
         | ident, Untyped tm -> Ident.Intern.lookup ident, tm, None
         | ident, Typed (tm, typ) -> Ident.Intern.lookup ident, tm, Some typ))
  in
  let source_location =
    Trace.{ file = here.Lexing.pos_fname; line = here.Lexing.pos_lnum }
  in
  let judgement = Trace.{ stage; focus; context; location = sc.pos; source_location } in
  match Trace.enter judgement with
  | Abort -> raise Trace.Trace_aborted
  | _ ->
    (match f () with
     | result ->
       let (_ : Trace.step_action) = Trace.leave judgement (Trace.Succeeded result) in
       result
     | exception (Fail e as exn) ->
       let (_ : Trace.step_action) = Trace.leave judgement (Trace.Failed e) in
       raise exn)
;;

(* {1 Running} *)

(** Install the state handler and an initial scope, then run [f].

    State is threaded purely: the handler's answer type is
    [state -> ('a, errors) result * state], so every resumption applies the
    continuation to an updated state value — no mutation, nothing to roll
    back or reset. *)
let run
  : ?bindings:entry Ident.Map.t -> (unit -> 'a) -> ('a, Calyx_error.t list) result * state
  =
  fun ?(bindings = Ident.Map.empty) f ->
  let open Effect.Deep in
  let init_scope = { bindings; pos = Pos.empty; level = 0 } in
  let init_state =
    { meta_gen = 0; errors = []; constraints = []; definitions = Ident.Map.empty }
  in
  let comp =
    match (under_scope init_scope f) with 
    | x -> (fun st -> Ok x, st)
    | effect (Tell_error e), k -> fun st ->
        continue k () { st with errors = e :: st.errors }
    | effect (Tell_constraint c), k -> fun st ->
        continue k () { st with constraints = c :: st.constraints }
    | effect Take_constraints, k -> fun st -> 
        continue k st.constraints { st with constraints = [] }
    | effect Has_constraints, k -> fun st -> 
        continue k (not @@ List.is_empty st.constraints) st
    | effect (Fresh_meta level), k -> fun st -> 
        let meta = Term.Meta.make ~id:st.meta_gen ~level () in 
        continue k meta { st with meta_gen = succ st.meta_gen }
    | effect (Define (ident, defn)), k ->  fun st -> 
        continue k () { st with definitions = Map.set st.definitions ~key:ident ~data:defn }
    | effect (Lookup_defn ident),k -> fun st -> 
      continue k (Map.find st.definitions ident) st

  in
  comp init_state
;;
