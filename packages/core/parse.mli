(** Incremental Menhir driver for the Calyx grammar. *)

(** [run source] parses [source] into the list of top-level declarations,
    or returns a human-readable error message. *)
val run : string -> (Term.cst Term.declaration list, string) result
