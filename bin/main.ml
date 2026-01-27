open Core
open Calyx

type backend =
  | WGSL
  | JS

let impl_of_backend = function
  | WGSL -> (module Calyx.Codegen.WGSL : Calyx.Codegen.M)
  | JS -> (module Calyx.Codegen.Javascript : Calyx.Codegen.M)
;;

let backend_of_string = function
  | s when String.equal (String.lowercase s) "wgsl" -> Ok WGSL
  | s when String.equal (String.lowercase s) "js" -> Ok JS
  | other -> Error (`Msg (Printf.sprintf "Unsupported backend '%s'" other))
;;

let compile backend path =
  let (module Backend : Codegen.M) = impl_of_backend backend in
  let compiler_output = Runner.compile (module Backend) path in
  match compiler_output with
  | Ok compiler_output ->
    Runner.output_to_file ~extension:Backend.extension ~compiler_output;
    Option.iter Backend.execute ~f:(Runner.execute (module Backend))
  | Error es ->
    Printf.printf
      "Failed to compile:\n%s\n"
      (String.concat ~sep:"\n" @@ List.map ~f:CalyxError.show es)
;;

let format path =
  let source = In_channel.read_all path in
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf path;
  try
    let program = Parser.program Lexer.token lexbuf in
    print_endline (Formatting.render_program program)
  with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf
      "Parse error at %s:%d:%d\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
;;

let step backend path =
  let (module Backend : Codegen.M) = impl_of_backend backend in
  let source = In_channel.read_all path in
  match
    Stepper.run ~source ~f:(fun () ->
      let result, _ =
        Context.run
          (Context.from_bindings Backend.standard_library)
          (let open Context.Syntax in
           let* toplevels =
             Parse.run source
             |> Result.map_error ~f:(fun e -> `Parser e)
             |> Context.liftR
           in
           let desugared = List.map ~f:Term.desugar_toplevel toplevels in
           let* inferred = Checker.infer_toplevel desugared in
           Context.pure inferred)
      in
      result)
  with
  | Ok inferred ->
    Printf.printf "Type checking succeeded with %d declarations\n" (List.length inferred)
  | Error es ->
    Printf.printf
      "Type checking failed:\n%s\n"
      (String.concat ~sep:"\n" @@ List.map ~f:CalyxError.show es)
  | exception Trace.Trace_aborted -> print_endline "Goodbye :3"
;;

let path =
  let open Cmdliner in
  let doc = "The path of the file you would like to compile" in
  Arg.(value & pos 0 string "main.calyx" & info [] ~doc ~docv:"PATH")
;;

let backend =
  let open Cmdliner in
  let doc = "Target backend (wgsl or js)" in
  let print : backend Arg.printer =
    fun fmt -> function
      | WGSL -> Format.fprintf fmt "wgsl"
      | JS -> Format.fprintf fmt "js"
  in
  let backend_conv = Arg.conv (backend_of_string, print) in
  Arg.(value & opt backend_conv JS & info [ "b"; "backend" ] ~doc ~docv:"BACKEND")
;;

let compile_cmd =
  let open Cmdliner in
  let doc = "Compile Calyx source to target backend" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(const compile $ backend $ path)
;;

let format_cmd =
  let open Cmdliner in
  let doc = "Format Calyx source code" in
  let info = Cmd.info "format" ~doc in
  Cmd.v info Term.(const format $ path)
;;

let step_cmd =
  let open Cmdliner in
  let doc = "Interactively step through type checking" in
  let info = Cmd.info "step" ~doc in
  Cmd.v info Term.(const step $ backend $ path)
;;

let _lsp_command =
  let open Cmdliner in
  let doc = "Start LSP server" in
  let _info = Cmd.info "LSP" ~doc in
  (* TODO: make it work !! *)
  ()
;;

let main_cmd =
  let open Cmdliner in
  let doc = "Calyx compiler and tools" in
  let info = Cmd.info "calyx" ~doc in
  Cmd.group info [ compile_cmd; format_cmd; step_cmd ]
;;

let () = exit (Cmdliner.Cmd.eval main_cmd)
