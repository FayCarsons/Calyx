(** Execution tracing for type checking.

    Provides effect-based tracing that captures the derivation tree
    of type checking, useful for debugging and LSP integration. *)

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
val disable_tracing : unit -> unit
val enter : ('i, 'o) judgement -> step_action
val leave : ('i, 'o) judgement -> 'o outcome -> step_action

(** {1 Handlers} *)

val handle_by_logging : ?output:Out_channel.t -> (unit -> 'a) -> 'a
(** Logs all trace events to the output channel with timestamps. *)

val handle_noop : f:(unit -> 'a) -> 'a
(** Discards all trace events. *)

(** {1 Tree Building} *)

type node_data =
  | Pending : ('i, 'o) judgement -> node_data
  | Complete : ('i, 'o) judgement * 'o outcome -> node_data

type node =
  { mutable data : node_data
  ; children : node Vector.t
  }

val handle_with_tree : f:(unit -> 'a) -> 'a * node option
(** Builds a trace tree from the execution. Returns the result and the root node. *)

(** {1 Tree Queries} *)

val get_type_from_node : node -> Term.value option
(** Extract the type from a completed node (Infer result, Check expected, or Eval result). *)

val get_location : node -> Pos.t
(** Get the source location of a node. *)
