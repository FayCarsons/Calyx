open Core

type t =
  { filename : string
  ; lo : pos
  ; hi : pos
  }
[@@deriving show, sexp]

and pos =
  { line : int
  ; beginning_of_line : int
  ; column : int
  }

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
