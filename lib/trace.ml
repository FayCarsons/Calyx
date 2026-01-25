open Core

let tracing_enabled = ref false
let enable_tracing () = tracing_enabled := true

type source_location =
  { file : string
  ; line : int
  }
[@@deriving sexp]

type judgement_kind =
  | Infer
  | Normalize
  | Check of { against : Sexp.t }
  | Unify of
      { lhs : Sexp.t
      ; rhs : Sexp.t
      ; strategy : [ `Structural | `Normalize | `Eta ]
      }
  | Exhaustion of
      { constructors : string list
      ; covered : string list
      ; missing : string list
      }
[@@deriving sexp]

type judgement =
  { kind : judgement_kind
  ; focus : Sexp.t option [@sexp.option]
  ; context : (string * Sexp.t * Sexp.t option) list [@sexp.list]
  ; location : Pos.t
  ; source_location : source_location
  }
[@@deriving sexp]

type trace_outcome =
  | Succeeded of Sexp.t
  | Failed of CalyxError.t

type step_action =
  | Continue
  | StepInto
  | StepOver
  | StepOut
  | Abort

exception Trace_aborted

type _ Effect.t +=
  | Enter : judgement -> step_action Effect.t
  | Leave : judgement * trace_outcome -> step_action Effect.t

let enter judgement =
  if !tracing_enabled then Effect.perform (Enter judgement) else Continue
;;

let leave judgement outcome =
  if !tracing_enabled then Effect.perform (Leave (judgement, outcome)) else Continue
;;

let trace
  :  judgement_kind
  -> to_sexp:('a -> Sexp.t)
  -> ?focus:Sexp.t
  -> ?context:(string * Sexp.t * Sexp.t option) list
  -> location:Pos.t
  -> Lexing.position
  -> (unit -> 'a)
  -> 'a
  =
  fun kind
    ~to_sexp
    ?focus
    ?(context = [])
    ~location
    Lexing.{ pos_fname = file; pos_lnum = line; _ }
    f ->
  let source_location = { file; line } in
  let judgement = { kind; focus; context; location; source_location } in
  let action = enter judgement in
  match action with
  | Abort -> raise Trace_aborted
  | _ ->
    (try
       let result = f () in
       let outcome = Succeeded (to_sexp result) in
       let _ = leave judgement outcome in
       result
     with
     | Trace_aborted -> raise Trace_aborted
     | exn ->
       let outcome = Failed `Todo in
       let _ = leave judgement outcome in
       raise exn)
;;

let timestamp : unit -> string =
  fun () ->
  let now = Time_float_unix.now () in
  Time_float_unix.to_string_abs now ~zone:(Lazy.force Time_float_unix.Zone.local)
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
                  "[%s] ENTER %s\n"
                  ts
                  (Sexp.to_string_hum @@ sexp_of_judgement { judgement with context = [] });
                Out_channel.flush output;
                continue k Continue)
          | Leave (judgement, outcome) ->
            Some
              (fun (k : (a, _) continuation) ->
                let ts = timestamp () in
                let outcome_str =
                  match outcome with
                  | Succeeded sexp -> Sexp.to_string_hum sexp
                  | Failed err -> CalyxError.show err
                in
                Printf.fprintf
                  output
                  "[%s] LEAVE %s: %s\n"
                  ts
                  (Sexp.to_string_hum @@ sexp_of_judgement { judgement with context = [] })
                  outcome_str;
                Out_channel.flush output;
                continue k Continue)
          | _ -> None)
    }
;;

let handle_interactive
  :  on_enter:(judgement -> step_action)
  -> on_leave:(judgement -> trace_outcome -> step_action)
  -> f:(unit -> 'a)
  -> 'a
  =
  fun ~on_enter ~on_leave ~f ->
  let open Effect.Deep in
  try_with
    f
    ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Enter judgement ->
            Some
              (fun (k : (a, _) continuation) ->
                let action = on_enter judgement in
                continue k action)
          | Leave (judgement, outcome) ->
            Some
              (fun (k : (a, _) continuation) ->
                let action = on_leave judgement outcome in
                continue k action)
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
