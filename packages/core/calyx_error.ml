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
  | `DuplicateField of Ident.t * Ident.t
  | `CtorNameTaken of Ident.t * Ident.t
  | `RecordLiteralMismatch of Ident.t * Ident.t list * Ident.t list
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
  | `CtorNameTaken (ctor, datatype) ->
    Printf.sprintf
      "Cannot derive constructor '%s' for record '%s': the name is already defined"
      (lookup ctor)
      (lookup datatype)
  | `DuplicateField (datatype, field) ->
    Printf.sprintf
      "Record '%s' declares field '%s' more than once"
      (lookup datatype)
      (lookup field)
  | `RecordLiteralMismatch (datatype, missing, extra) ->
    let part label = function
      | [] -> []
      | fields ->
        [ Printf.sprintf
            "%s: %s"
            label
            (String.concat ~sep:", " @@ List.map ~f:lookup fields)
        ]
    in
    Printf.sprintf
      "Record literal does not match '%s'\n%s\n"
      (lookup datatype)
      (String.concat ~sep:"\n" (part "missing fields" missing @ part "extra fields" extra))
  | `Todo -> "Unimplemented feature"
;;

let pp _fmt _err = ()
