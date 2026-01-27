(** Execution tracing for type checking.

    Provides effect-based tracing that captures the derivation tree
    of type checking, useful for debugging and LSP integration. *)

(** A location within the compiler itself, a position in the OCaml code *)
type source_location =
  { file : string
  ; line : int
  }

(** {1 Stages}

    GADT encoding the input/output types for each checking stage. *)

type (_, _) stage =
  | Infer : (Term.t, Term.value * Term.t) stage
  | Check : Term.value -> (Term.t, Term.t) stage
  | Eval : (Term.t, Term.value) stage
  | Quote : (Term.value, Term.t) stage
  | Unify : (Term.value * Term.value, unit) stage

(** Non-GADT stage kind for filtering *)
type stage_kind =
  | KInfer
  | KCheck
  | KEval
  | KQuote
  | KUnify

(** {1 Judgements and Outcomes} *)

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

(** {1 Stepping} *)

type step_action =
  | Continue
  | StepInto
  | StepOver
  | StepOut
  | Abort

exception Trace_aborted

(** {1 Trace Control} *)

val enable_tracing : unit -> unit
val enter : ('i, 'o) judgement -> step_action
val leave : ('i, 'o) judgement -> 'o outcome -> step_action

(** {1 Handlers} *)

(** Logs all trace events to the output channel with timestamps. *)
val handle_by_logging : ?output:Out_channel.t -> (unit -> 'a) -> 'a

(** Discards all trace events. *)
val handle_noop : f:(unit -> 'a) -> 'a

(** {1 Tree Building} *)

type node_data =
  | Pending : ('i, 'o) judgement -> node_data
  | Complete : ('i, 'o) judgement * 'o outcome -> node_data

type node =
  { mutable data : node_data
  ; children : node Vector.t
  }

(** Builds a trace tree from the execution. Returns the result and the root node. *)
val handle_with_tree : f:(unit -> 'a) -> 'a * node option

(** {1 Tree Queries} *)

(** Extract the type from a completed node (Infer result, Check expected, or Eval result). *)
val get_type_from_node : node -> Term.value option

(** Get the source location of a node. *)
val get_location : node -> Pos.t

(** {1 Serialization} *)

val sexp_of_stage : ('i, 'o) stage -> Sexplib.Sexp.t
val sexp_of_focus : ('i, 'o) stage -> 'i -> Sexplib.Sexp.t
val sexp_of_outcome : ('i, 'o) stage -> 'o outcome -> Sexplib.Sexp.t

(** {1 Interactive Stepping} *)

type step_mode =
  | Into
  | Over
  | Out
  | Run

type enter_handler =
  { on_enter : 'i 'o. depth:int -> mode:step_mode -> ('i, 'o) judgement -> step_action }

type leave_handler =
  { on_leave :
      'i 'o.
      depth:int -> mode:step_mode -> ('i, 'o) judgement -> 'o outcome -> step_action
  }

(** Interactive handler that pauses at trace events.
    [on_enter] is called when entering a judgement.
    [on_leave] is called when leaving a judgement with its outcome.
    [on_start] is called before tracing begins (e.g., to initialize terminal).
    [on_finish] is called when tracing ends (e.g., to release terminal). *)
val handle_interactive
  :  on_enter:enter_handler
  -> on_leave:leave_handler
  -> ?on_start:(unit -> unit)
  -> ?on_finish:(unit -> unit)
  -> f:(unit -> 'a)
  -> 'a
