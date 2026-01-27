open Core

let tracing_enabled = ref false
let enable_tracing () = tracing_enabled := true
let disable_tracing () = tracing_enabled := false

type source_location =
  { file : string
  ; line : int
  }
[@@deriving sexp]

type (_, _) stage =
  | Infer : (Term.t, Term.value * Term.t) stage
  | Check : Term.value -> (Term.t, Term.t) stage
  | Eval : (Term.t, Term.value) stage
  | Quote : (Term.value, Term.t) stage
  | Unify : (Term.value * Term.value, unit) stage

type stage_kind =
  | KInfer
  | KCheck
  | KEval
  | KQuote
  | KUnify

type ('i, 'o) judgement =
  { stage : ('i, 'o) stage
  ; focus : 'i
  ; context : (string * Term.value * Term.value option) list Lazy.t
  ; location : Pos.t
  ; source_location : source_location
  }

type 'o outcome =
  | Succeeded of 'o
  | Failed of CalyxError.t

type step_action =
  | Continue
  | StepInto
  | StepOver
  | StepOut
  | Abort

exception Trace_aborted

type _ Effect.t +=
  | Enter : ('i, 'o) judgement -> step_action Effect.t
  | Leave : ('i, 'o) judgement * 'o outcome -> step_action Effect.t

let enter judgement =
  if !tracing_enabled then Effect.perform (Enter judgement) else Continue
;;

let leave judgement outcome =
  if !tracing_enabled then Effect.perform (Leave (judgement, outcome)) else Continue
;;

let timestamp () =
  let now = Time_float_unix.now () in
  Time_float_unix.to_string_abs now ~zone:(Lazy.force Time_float_unix.Zone.local)
;;

let sexp_of_stage : type i o. (i, o) stage -> Sexp.t = function
  | Infer -> Sexp.Atom "Infer"
  | Check _ -> Sexp.Atom "Check"
  | Eval -> Sexp.Atom "Eval"
  | Quote -> Sexp.Atom "Quote"
  | Unify -> Sexp.Atom "Unify"
;;

let sexp_of_focus : type i o. (i, o) stage -> i -> Sexp.t =
  fun stage focus ->
  match stage with
  | Infer -> Term.sexp_of_t focus
  | Check _ -> Term.sexp_of_t focus
  | Eval -> Term.sexp_of_t focus
  | Quote -> Term.sexp_of_value focus
  | Unify ->
    let lhs, rhs = focus in
    Sexp.List [ Term.sexp_of_value lhs; Term.sexp_of_value rhs ]
;;

let sexp_of_outcome : type i o. (i, o) stage -> o outcome -> Sexp.t =
  fun stage outcome ->
  match outcome with
  | Failed err -> Sexp.List [ Sexp.Atom "Failed"; Sexp.Atom (CalyxError.show err) ]
  | Succeeded result ->
    let result_sexp =
      match stage with
      | Infer ->
        let ty, tm = result in
        Sexp.List [ Term.sexp_of_value ty; Term.sexp_of_t tm ]
      | Check _ -> Term.sexp_of_t result
      | Eval -> Term.sexp_of_value result
      | Quote -> Term.sexp_of_t result
      | Unify -> Sexp.Atom "()"
    in
    Sexp.List [ Sexp.Atom "Succeeded"; result_sexp ]
;;

let handle_by_logging : ?output:Out_channel.t -> (unit -> 'a) -> 'a =
  fun ?(output = Out_channel.stdout) f ->
  let open Effect.Deep in
  Printf.fprintf output ";; Trace started at: %s\n" (timestamp ());
  Out_channel.flush output;
  try_with
    f
    ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Enter judgement ->
            Some
              (fun (k : (a, _) continuation) ->
                let ts = timestamp () in
                Printf.fprintf
                  output
                  "[%s] ENTER %s %s\n"
                  ts
                  (Sexp.to_string_hum @@ sexp_of_stage judgement.stage)
                  (Sexp.to_string_hum @@ sexp_of_focus judgement.stage judgement.focus);
                Out_channel.flush output;
                continue k Continue)
          | Leave (judgement, outcome) ->
            Some
              (fun (k : (a, _) continuation) ->
                let ts = timestamp () in
                Printf.fprintf
                  output
                  "[%s] LEAVE %s: %s\n"
                  ts
                  (Sexp.to_string_hum @@ sexp_of_stage judgement.stage)
                  (Sexp.to_string_hum @@ sexp_of_outcome judgement.stage outcome);
                Out_channel.flush output;
                continue k Continue)
          | _ -> None)
    }
;;

let handle_noop : f:(unit -> 'a) -> 'a =
  fun ~f ->
  let open Effect.Deep in
  try_with
    f
    ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Enter _ -> Some (fun (k : (a, _) continuation) -> continue k Continue)
          | Leave _ -> Some (fun (k : (a, _) continuation) -> continue k Continue)
          | _ -> None)
    }
;;

type node_data =
  | Pending : ('i, 'o) judgement -> node_data
  | Complete : ('i, 'o) judgement * 'o outcome -> node_data

type node =
  { mutable data : node_data
  ; children : node Vector.t
  }

let handle_with_tree : f:(unit -> 'a) -> 'a * node option =
  fun ~f ->
  let root = ref None in
  let stack = Stack.create () in
  let open Effect.Deep in
  let result =
    try_with
      f
      ()
      { effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Enter judgement ->
              Some
                (fun (k : (a, _) continuation) ->
                  let node = { data = Pending judgement; children = Vector.create () } in
                  (match Stack.top stack with
                   | Some parent -> Vector.push parent.children node
                   | None -> root := Some node);
                  Stack.push stack node;
                  continue k Continue)
            | Leave (judgement, outcome) ->
              Some
                (fun (k : (a, _) continuation) ->
                  (match Stack.pop stack with
                   | Some node -> node.data <- Complete (judgement, outcome)
                   | None -> ());
                  continue k Continue)
            | _ -> None)
      }
  in
  result, !root
;;

let get_type_from_node node =
  match node.data with
  | Pending _ -> None
  | Complete ({ stage = Infer; _ }, Succeeded (ty, _)) -> Some ty
  | Complete ({ stage = Check expected; _ }, Succeeded _) -> Some expected
  | Complete ({ stage = Eval; _ }, Succeeded v) -> Some v
  | Complete _ -> None
;;

let get_location node =
  match node.data with
  | Pending j -> j.location
  | Complete (j, _) -> j.location
;;

type step_mode =
  | Into
  | Over
  | Out
  | Run

let should_break ~depth ~mode ~break_depth =
  match mode with
  | Into -> true
  | Over -> depth <= break_depth
  | Out -> depth < break_depth
  | Run -> false
;;

type enter_handler =
  { on_enter : 'i 'o. depth:int -> mode:step_mode -> ('i, 'o) judgement -> step_action }

type leave_handler =
  { on_leave :
      'i 'o.
      depth:int -> mode:step_mode -> ('i, 'o) judgement -> 'o outcome -> step_action
  }

let handle_interactive
  :  on_enter:enter_handler
  -> on_leave:leave_handler
  -> ?on_start:(unit -> unit)
  -> ?on_finish:(unit -> unit)
  -> f:(unit -> 'a)
  -> 'a
  =
  fun ~on_enter:{ on_enter }
    ~on_leave:{ on_leave }
    ?(on_start = Fun.const ())
    ?(on_finish = Fun.const ())
    ~f ->
  enable_tracing ();
  on_start ();
  let depth = ref 0 in
  let mode = ref Into in
  let break_depth = ref 0 in
  let handle_action action current_depth =
    match action with
    | StepInto ->
      mode := Into;
      break_depth := current_depth
    | StepOver ->
      mode := Over;
      break_depth := current_depth
    | StepOut ->
      mode := Out;
      break_depth := current_depth
    | Continue -> mode := Run
    | Abort ->
      on_finish ();
      raise Trace_aborted
  in
  let open Effect.Deep in
  let result =
    match
      try_with
        f
        ()
        { effc =
            (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Enter judgement ->
                Some
                  (fun (k : (a, _) continuation) ->
                    let current_depth = !depth in
                    incr depth;
                    if
                      should_break
                        ~depth:current_depth
                        ~mode:!mode
                        ~break_depth:!break_depth
                    then (
                      let action = on_enter ~depth:current_depth ~mode:!mode judgement in
                      handle_action action current_depth;
                      continue k action)
                    else continue k Continue)
              | Leave (judgement, outcome) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    decr depth;
                    let current_depth = !depth in
                    if
                      should_break
                        ~depth:current_depth
                        ~mode:!mode
                        ~break_depth:!break_depth
                    then (
                      let action =
                        on_leave ~depth:current_depth ~mode:!mode judgement outcome
                      in
                      handle_action action current_depth;
                      continue k action)
                    else continue k Continue)
              | _ -> None)
        }
    with
    | result ->
      on_finish ();
      result
    | exception e ->
      on_finish ();
      raise e
  in
  disable_tracing ();
  result
;;
