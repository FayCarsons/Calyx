open Core

type t =
  [ `Expected of string * string
  | `UnificationFailure of string * string
  | `NotFound of Ident.t
  | `NoField of Ident.t * (Ident.t * string) list
  | `Occurs of string
  | `Parser of string
  | `Stuck of string
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
  | `Todo -> "Unimplemented feature"
;;

let pp _fmt _err = ()
