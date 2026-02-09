open Core

type pos =
  { line : int
  ; beginning_of_line : int
  ; column : int
  }
[@@deriving show]

let sexp_of_pos : pos -> Sexp.t =
  fun { line; beginning_of_line; column } ->
  let open Sexp in
  List
    [ Atom "pos"
    ; List [ Atom "line"; Atom (string_of_int line) ]
    ; List [ Atom "column"; Atom (string_of_int @@ (column - beginning_of_line)) ]
    ]
;;

let pos_of_sexp : Sexp.t -> pos =
  let open Sexp in
  function
  | List [ Atom _; List [ Atom "line"; Atom line ]; List [ Atom "column"; Atom column ] ]
    ->
    let line = int_of_string line
    and column = int_of_string column in
    { line; beginning_of_line = 0; column }
  | _ -> raise (Failure "expected pos")
;;

type t =
  { filename : string
  ; lo : pos
  ; hi : pos
  }
[@@deriving show, sexp]

let pos_empty = { line = 0; beginning_of_line = 0; column = 0 }
let empty = { filename = "<Debug>"; lo = pos_empty; hi = pos_empty }

let pos_of_position : Lexing.position -> pos =
  fun Lexing.{ pos_lnum; pos_bol; pos_cnum; _ } ->
  { line = pos_lnum; beginning_of_line = pos_bol; column = pos_cnum }
;;

let from_parser : string -> Lexing.position -> Lexing.position -> t =
  fun filename lo hi ->
  let lo = pos_of_position lo
  and hi = pos_of_position hi in
  { filename; lo; hi }
;;
