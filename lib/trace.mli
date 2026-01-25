type source_location =
  { file : string
  ; line : int
  }

type judgement_kind =
  | Infer
  | Normalize
  | Check of { against : Core.Sexp.t }
  | Unify of
      { lhs : Core.Sexp.t
      ; rhs : Core.Sexp.t
      ; strategy : [ `Structural | `Normalize | `Eta ]
      }
  | Exhaustion of
      { constructors : string list
      ; covered : string list
      ; missing : string list
      }

type judgement =
  { kind : judgement_kind
  ; focus : Core.Sexp.t option [@sexp.option]
  ; context : (string * Core.Sexp.t * Core.Sexp.t option) list
  ; location : Pos.t
  ; source_location : source_location
  }

type trace_outcome =
  | Succeeded of Core.Sexp.t
  | Failed of CalyxError.t

type step_action =
  | Continue
  | StepInto
  | StepOver
  | StepOut
  | Abort

exception Trace_aborted

val enter : judgement -> step_action
val leave : judgement -> trace_outcome -> step_action

val trace
  :  judgement_kind
  -> to_sexp:('a -> Core.Sexp.t)
  -> ?focus:Core.Sexp.t
  -> ?context:(string * Core.Sexp.t * Core.Sexp.t option) list
  -> location:Pos.t
  -> Lexing.position
  -> (unit -> 'a)
  -> 'a

val handle_by_logging : ?output:Out_channel.t -> (unit -> 'a) -> 'a

val handle_interactive
  :  on_enter:(judgement -> step_action)
  -> on_leave:(judgement -> trace_outcome -> step_action)
  -> f:(unit -> 'a)
  -> 'a

val handle_noop : f:(unit -> 'a) -> 'a
val enable_tracing : unit -> unit
