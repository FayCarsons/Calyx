open Util

let parse_program (input : string) : (Term.cst Term.declaration list, string) result =
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
           "Parse error at line %d, column %d"
           pos.pos_lnum
           (pos.pos_cnum - pos.pos_bol))
  in
  try loop checkpoint with
  | Lexer.Lexing_error msg -> Error msg
;;

let compile (module Backend : Codegen.S) path =
  let go () =
    let contents = In_channel.open_text path |> In_channel.input_all in
    let* toplevels =
      Result.map_error (fun e -> [ `Parser e ]) @@ parse_program contents
    in
    let desugared = List.map Term.desugar_toplevel toplevels in
    print_endline "Desugared:";
    List.iter (Fun.compose print_string (Pretty.declaration Pretty.ast)) desugared;
    print_newline ();
    let* inferred, solutions = Checker.infer_toplevel desugared in
    print_endline "Inferred:";
    List.iter (Fun.compose print_string (Pretty.declaration Pretty.ast)) inferred;
    print_newline ();
    let zonked = List.map (Zonk.zonk_toplevel solutions) inferred in
    print_endline "Zonked:";
    List.iter (Fun.compose print_string (Pretty.declaration Pretty.ast)) zonked;
    print_newline ();
    let ir = Ir.convert zonked in
    print_endline "IR:";
    List.iter (Fun.compose print_string Ir.PrettyIR.declaration) ir;
    print_newline ();
    Ok ir
  in
  let finally xs =
    match xs with
    | Ok zonked -> Backend.compile zonked
    | Error es -> "Failed to compile:\n" ^ (List.map Pretty.error es |> String.concat "\n")
  in
  Env.handle ~env:(Env.from_bindings Backend.standard_library) (fun () ->
    finally @@ go ())
;;
