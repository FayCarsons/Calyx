open Core

module type M = sig
  type judgement =
    { kind : judgement_kind
    ; focus : Term.t option [@sexp.option]
    ; context : (string * Term.t * Term.value option) list
    ; location : Pos.t
    ; source_location : source_location
    }

  and judgement_kind =
    | Infer of { rule : string option } [@sexp.option]
    | Check of { against : Term.value }
    | Unify of
        { lhs : Term.t
        ; rhs : Term.t
        ; strategy : [ `Structural | `Normalize | `Eta ]
        }
    | Exhaustion of
        { constructors : string list
        ; covered : string list
        ; missing : string list
        }
    | Normalize of
        { term : Term.t
        ; steps : int ref
        }

  and source_location =
    { file : string
    ; line : int
    }

  type trace_result =
    | Success of
        { conclusion : Term.t option [@sexp.option]
        ; constraints_added : Sexp.t
        ; context_after : (string * Term.t * Term.value option) list option [@sexp.option]
        }
    | Failure of CalyxError.t list
  [@@deriving sexp]

  (** Existential wrapper for traced computation results *)
  type displayable =
    | Displayable :
        { value : 'a
        ; to_sexp : 'a -> Sexp.t
        }
        -> displayable

  val display : displayable -> string

  (** Outcome of a traced computation *)
  type trace_outcome =
    | Succeeded of displayable
    | Failed of CalyxError.t

  (** Control flow for interactive stepping *)
  type step_action =
    | Continue (** Run to completion *)
    | StepInto (** Stop at next Enter *)
    | StepOver (** Stop when this frame's Leave fires *)
    | StepOut (** Stop at parent frame's Leave *)
    | Abort (** Cancel execution *)

  exception Trace_aborted

  val trace
    :  judgement_kind
    -> to_sexp:('a -> Sexp.t)
    -> ?focus:Term.t option
    -> ?context:(string * Term.t * Term.value option) list
    -> location:Pos.t
    -> Lexing.position (* from [%here] *)
    -> (unit -> 'a)
    -> 'a

  val handle_by_logging : ?output:Out_channel.t -> (unit -> 'a) -> 'a

  val handle_interactive
    :  on_enter:(judgement -> step_action)
    -> on_leave:(judgement -> trace_outcome -> step_action)
    -> f:(unit -> 'a)
    -> 'a
end

module M : M = struct
  type judgement =
    { kind : judgement_kind
    ; focus : Term.t option [@sexp.option]
    ; context : (string * Term.t * Term.value option) list
    ; location : Pos.t
    ; source_location : source_location
    }
  [@@deriving sexp]

  and judgement_kind =
    | Infer of { rule : string option } [@sexp.option]
    | Check of { against : Term.value }
    | Unify of
        { lhs : Term.t
        ; rhs : Term.t
        ; strategy : [ `Structural | `Normalize | `Eta ]
        }
    | Exhaustion of
        { constructors : string list
        ; covered : string list
        ; missing : string list
        }
    | Normalize of
        { term : Term.t
        ; steps : int ref
        }
  [@@deriving sexp]

  and source_location =
    { file : string
    ; line : int
    }
  [@@deriving sexp]

  type trace_result =
    | Success of
        { conclusion : Term.t option [@sexp.option]
        ; constraints_added : Sexp.t
        ; context_after : (string * Term.t * Term.value option) list option [@sexp.option]
        }
    | Failure of CalyxError.t list
  [@@deriving sexp]

  type displayable =
    | Displayable :
        { value : 'a
        ; to_sexp : 'a -> Sexp.t
        }
        -> displayable

  let display (Displayable { value; to_sexp }) = Sexp.to_string_hum @@ to_sexp value

  type trace_outcome =
    | Succeeded of displayable
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

  let trace
    :  judgement_kind
    -> to_sexp:('a -> Sexp.t)
    -> ?focus:Term.t option
    -> ?context:(string * Term.t * Term.value option) list
    -> location:Pos.t
    -> Lexing.position (* from [%here] *)
    -> (unit -> 'a)
    -> 'a
    =
    fun kind
      ~to_sexp
      ?(focus = None)
      ?(context = [])
      ~location
      Lexing.{ pos_fname = file; pos_lnum = line; _ }
      f ->
    let source_location = { file; line } in
    let judgement = { kind; focus; context; location; source_location } in
    let action = Effect.perform (Enter judgement) in
    match action with
    | Abort -> raise Trace_aborted
    | _ ->
      (try
         let result = f () in
         let outcome = Succeeded (Displayable { value = result; to_sexp }) in
         let _ = Effect.perform (Leave (judgement, outcome)) in
         result
       with
       | Trace_aborted -> raise Trace_aborted
       | exn ->
         let outcome = Failed `Todo in
         let _ = Effect.perform (Leave (judgement, outcome)) in
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
                    (Sexp.to_string_hum @@ sexp_of_judgement judgement);
                  Out_channel.flush output;
                  continue k Continue)
            | Leave (judgement, outcome) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let ts = timestamp () in
                  let outcome_str =
                    match outcome with
                    | Succeeded (Displayable { value = _; to_sexp }) ->
                      (* We can't easily show the value without knowing its type,
                         but we have to_sexp available if needed *)
                      ignore to_sexp;
                      "OK"
                    | Failed err -> CalyxError.show err
                  in
                  Printf.fprintf
                    output
                    "[%s] LEAVE %s: %s\n"
                    ts
                    (Sexp.to_string_hum @@ sexp_of_judgement judgement)
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
end

include M
