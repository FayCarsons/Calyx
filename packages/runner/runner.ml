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

let compile
  : ?trace:bool -> (module Codegen.M) -> string -> (string, Calyx_error.t list) result
  =
  fun ?(trace = false) (module Backend) path ->
  if trace then Trace.enable_tracing ();
  Trace.handle_by_logging (fun () ->
    let contents = In_channel.read_all path in
    match Parse.run contents with
    | Error e -> Error [ `Parser e ]
    | Ok toplevels ->
      let desugared = List.map toplevels ~f:Term.desugar_toplevel in
      let resolved, _dependency_graph = Resolve.resolve_program desugared in
      let result, state =
        Context.run ~bindings:Backend.standard_library (fun () ->
          let inferred = Checker.infer_toplevel resolved in
          Solve.solve ();
          List.map ~f:Zonk.zonk_toplevel inferred)
      in
      (match result, state.Context.errors with
       | Ok zonked, [] ->
         let ir = Ir.convert zonked in
         Ok (Backend.compile ir)
       | Ok _, errors -> Error (List.rev errors)
       | Error errors, _ -> Error (List.rev errors)))
;;
