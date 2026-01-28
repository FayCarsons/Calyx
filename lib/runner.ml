module U = Unix
open Core
module Unix = U

let mkdir path =
  try Unix.mkdir path 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
;;

(** Write compiled program [compiler_output] to `.calyx/out.[extension]` *)
let output_to_file ~extension ~compiler_output : unit =
  let target_dir = ".calyx" in
  mkdir target_dir;
  let output_path = Filename.concat target_dir (Printf.sprintf "out.%s" extension) in
  Core.Out_channel.write_all output_path ~data:compiler_output;
  Printf.printf "Output written to: %s\n" output_path
;;

(** Run the compiled program *)
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
  (fun () -> Out_channel.(newline stdout))
  >> print_endline
  >> Sexplib.Sexp.to_string_hum
  >> Term.sexp_of_declaration Term.sexp_of_t
;;

let compile
  : ?trace:bool -> (module Codegen.M) -> string -> (string, CalyxError.t list) result
  =
  fun ?(trace = false) (module Backend) path ->
  if trace then Trace.enable_tracing ();
  Trace.handle_by_logging (fun () ->
    let result, _ =
      Context.run
        (Context.from_bindings Backend.standard_library)
        (let open Context.Syntax in
         let contents = In_channel.read_all path in
         let* toplevels =
           Parse.run contents |> Result.map_error ~f:(fun e -> `Parser e) |> Context.liftR
         in
         let desugared = List.map toplevels ~f:Term.desugar_toplevel in
         let resolved, _dependency_graph = Resolve.resolve_program desugared in
         let* inferred = Checker.infer_toplevel resolved in
         let* _ = Solve.solve () in
         let* zonked = Context.traverse ~f:Zonk.zonk_toplevel inferred in
         let* errors = Context.errors in
         List.iter errors ~f:(fun e -> Printf.printf "Error: %s\n" (CalyxError.show e));
         assert (List.is_empty errors);
         let ir = Ir.convert zonked in
         Context.pure @@ Backend.compile ir)
    in
    result)
;;
