open Util

let mkdir path =
  try Unix.mkdir path 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
;;

let output_to_file ~extension ~compiler_output : unit =
  let target_dir = ".calyx" in
  mkdir target_dir;
  let output_path = Filename.concat target_dir (Printf.sprintf "out.%s" extension) in
  Core.Out_channel.write_all output_path ~data:compiler_output;
  Printf.printf "Output written to: %s\n" output_path
;;

let execute (module Backend : Codegen.M) (cmd : string) =
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

let print_ast_sexp =
  let ( >> ) = Fun.compose in
  print_newline
  >> print_endline
  >> Sexplib.Sexp.to_string_hum
  >> Term.sexp_of_declaration Term.sexp_of_ast
;;

let compile (module Backend : Codegen.M) (path : string)
  : (string, CalyxError.t list) result
  =
  Env.init (Env.from_bindings Backend.standard_library);
  let contents = In_channel.open_text path |> In_channel.input_all in
  let* toplevels =
    try Result.map_error (fun e -> [ `Parser e ]) @@ Parse.run contents with
    | Lexer.Lexing_error e -> failwith e
  in
  let desugared = List.map Term.desugar_toplevel toplevels in
  print_endline "Desugared:";
  List.iter print_ast_sexp desugared;
  let* inferred = Checker.infer_toplevel desugared in
  print_endline "Inferred:";
  List.iter print_ast_sexp inferred;
  let zonked = List.map Zonk.zonk_toplevel inferred in
  print_endline "Zonked:";
  List.iter print_ast_sexp zonked;
  let ir = Ir.convert zonked in
  print_endline "IR:";
  List.iter (Fun.compose print_string Ir.PrettyIR.declaration) ir;
  Result.ok @@ Backend.compile ir
;;
