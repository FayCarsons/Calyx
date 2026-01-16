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
  let info = Cmd.info "calyx" ~doc in
  Cmd.v info Term.(const compile $ backend $ path)
;;

let () = exit (Cmdliner.Cmd.eval compile_cmd)
