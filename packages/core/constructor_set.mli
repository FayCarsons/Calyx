(** Global set of identifiers known to name sum-type constructors.

    Populated by the parser as constructor declarations are read, then queried
    to disambiguate constructor patterns from variable patterns. *)

module M : sig
  val member : Ident.t -> bool
  val add : Ident.t -> unit
end
