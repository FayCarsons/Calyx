open Core

type t =
  [ `Expected of string * string
  | `UnificationFailure of string * string
  | `NotFound of Ident.t
  | `NoField of Ident.t * (Ident.t * string) list
  | `Occurs of string
  | `Parser of string
  | `Stuck of string
  | `Unsupported of string
  | `Positivity of Ident.t * Ident.t
  | `NonExhaustiveMatch of Ident.t list
  | `UnknownConstructor of Ident.t
  | `CtorArity of Ident.t * int * int
  | `ConstructorMismatch of Ident.t * Ident.t
  | `Todo
  ]
[@@deriving sexp]

let show : t -> string =
  let lookup = Ident.Intern.lookup in
  function
  | `Expected (expected, got) -> Printf.sprintf "Expected:\n%s\nGot:\n%s\n" expected got
  | `UnificationFailure (l, r) ->
    Printf.sprintf "Failed to unify type\n%s\nwith:\n%s\n" l r
  | `NotFound ident -> Printf.sprintf "No variable '%s' in scope" (lookup ident)
  | `NoField (accessor, fields) ->
    Printf.sprintf
      "No field '%s' in\n{%s}\n"
      (lookup accessor)
      (List.map ~f:(fun (ident, ty) -> lookup ident ^ " : " ^ ty) fields
       |> String.concat ~sep:"\n,  ")
  | `Occurs m -> Printf.sprintf "Meta %s failed occurs check" m
  | `Parser p -> Printf.sprintf "Parser error:\n%s\n" p
  | `Stuck constraints -> Printf.sprintf "Cannot solve constraints:\n%s\n" constraints
  | `Unsupported feature -> Printf.sprintf "Unsupported: %s" feature
  | `Positivity (datatype, ctor) ->
    Printf.sprintf
      "Datatype '%s' fails the positivity check: constructor '%s' mentions '%s' in a \
       function argument position"
      (lookup datatype)
      (lookup ctor)
      (lookup datatype)
  | `NonExhaustiveMatch missing ->
    Printf.sprintf
      "Non-exhaustive match, missing cases:\n%s\n"
      (String.concat ~sep:", " @@ List.map ~f:lookup missing)
  | `UnknownConstructor ctor -> Printf.sprintf "Unknown constructor '%s'" (lookup ctor)
  | `CtorArity (ctor, expected, got) ->
    Printf.sprintf
      "Constructor '%s' expects %d arguments but the pattern binds %d"
      (lookup ctor)
      expected
      got
  | `ConstructorMismatch (ctor, datatype) ->
    Printf.sprintf
      "Constructor '%s' does not belong to datatype '%s'"
      (lookup ctor)
      (lookup datatype)
  | `Todo -> "Unimplemented feature"
;;

let pp _fmt _err = ()
