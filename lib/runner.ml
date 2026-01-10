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
           "Parse error at line %d, column %d\n"
           pos.pos_lnum
           (pos.pos_cnum - pos.pos_bol))
  in
  try loop checkpoint with
  | Lexer.Lexing_error msg -> Error msg
;;

let output_to_file ~extension ~compiler_output : unit =
  let target_dir = ".calyx" in
  (try Unix.mkdir target_dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let output_path = Filename.concat target_dir (Printf.sprintf "out.%s" extension) in
  Core.Out_channel.write_all output_path ~data:compiler_output;
  Printf.printf "Output written to: %s\n" output_path
;;

let run_program (module Backend : Codegen.M) (cmd : string) =
  let output_path =
    Filename.concat ".calyx" (Printf.sprintf "out.%s" Backend.extension)
  in
  let run_cmd = Printf.sprintf "%s %s 2>&1" cmd output_path in
  Printf.printf "Running: %s\n" run_cmd;
  (* Capture the output using Unix.open_process_in *)
  let chan = Unix.open_process_in run_cmd in
  let output = In_channel.input_all chan in
  let exit_status = Unix.close_process_in chan in
  match exit_status with
  | Unix.WEXITED 0 -> Printf.printf "Program output:\n%s" output
  | Unix.WEXITED code -> Printf.printf "Program exited with code %d:\n%s" code output
  | Unix.WSIGNALED signal ->
    Printf.printf "Program terminated by signal %d:\n%s" signal output
  | Unix.WSTOPPED signal ->
    Printf.printf "Program stopped by signal %d:\n%s" signal output
;;

let compile (module Backend : Codegen.M) (path : string) : (string, Error.t list) result =
  Env.init (Env.from_bindings Backend.standard_library);
  let contents = In_channel.open_text path |> In_channel.input_all in
  let* toplevels =
    try Result.map_error (fun e -> [ `Parser e ]) @@ parse_program contents with
    | Lexer.Lexing_error e -> failwith e
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
  Result.ok @@ Backend.compile ir
;;
