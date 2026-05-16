(** End-to-end compilation pipeline and program execution. *)

(** [compile ?trace backend path] parses, type-checks and code-generates the
    program at [path] with the given [backend], returning the emitted source
    or the accumulated type errors. *)
val compile
  :  ?trace:bool
  -> (module Codegen.M)
  -> string
  -> (string, Calyx_error.t list) result

(** [output_to_file ~extension ~compiler_output] writes [compiler_output] to
    [.calyx/out.<extension>]. *)
val output_to_file : extension:string -> compiler_output:string -> unit

(** [execute backend cmd] runs [cmd] against the generated artifact and prints
    its output. *)
val execute : (module Codegen.M) -> string -> unit
