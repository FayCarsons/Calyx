(** Backend interface shared by all code generators. *)

module type StandardLibrary = sig
  type stage

  val builtins : stage Term.declaration list
end

module type M = sig
  (** Standard library functions for this backend. 
      In the future we should be able to write these as S-expressions with access to compiler internals like 'Opaque' 
  *)
  val standard_library : Context.entry Ident.Map.t

  (** Native infix functions, do not need to be renamed *)
  val native_infix : Ident.t list

  (** Command we can call to run generated code *)
  val execute : string option

  (** The file extension for the backend's representation *)
  val extension : string

  (** Compile a list of top-level declarations to the target language *)
  val compile : Ir.declaration list -> string
end
