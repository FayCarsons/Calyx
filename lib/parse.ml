module Interpreter = Parser.MenhirInterpreter
module Incremental = Parser.Incremental

let run (input : string) : (Term.cst Term.declaration list, string) result =
  let module I = Parser.MenhirInterpreter in
  let module Inc = Parser.Incremental in
  let lexbuf = Lexing.from_string input in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let checkpoint = Inc.program lexbuf.lex_curr_p in
  let rec loop checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
      let checkpoint = I.offer checkpoint (supplier ()) in
      loop checkpoint
    | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop checkpoint
    | I.HandlingError env ->
      (* Try to recover from error *)
      let pos = I.positions env |> snd in
      Printf.printf
        "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      (* Attempt error recovery by resuming *)
      loop @@ I.resume checkpoint
    | I.Accepted result -> Ok result
    | I.Rejected ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Error
        (Printf.sprintf
           "Parse error at line %d, column %d\n"
           pos.pos_lnum
           (pos.pos_cnum - pos.pos_bol))
  in
  try loop checkpoint with
  | Lexer.Lexing_error msg -> Error msg
;;
