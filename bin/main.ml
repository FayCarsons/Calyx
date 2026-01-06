open Cmdliner

type backend =
  | WGSL
  | JS

let impl_of_backend = function
  | WGSL -> (module Fingerpaint.Codegen.WGSL : Fingerpaint.Codegen.S)
  | JS -> (module Fingerpaint.Codegen.Javascript : Fingerpaint.Codegen.S)
;;

let backend_of_string = function
  | s when String.lowercase_ascii s = "wgsl" -> Ok WGSL
  | s when String.lowercase_ascii s = "js" -> Ok JS
  | other -> Error (`Msg (Printf.sprintf "Unsupported backend '%s'" other))
;;

let compile backend path =
  let module_backend = impl_of_backend backend in
  let codegen = Fingerpaint.Runner.compile module_backend path in
  print_endline "COMPILED OUTPUT:";
  print_endline codegen
;;

let path =
  let doc = "The path of the file you would like to compile" in
  Arg.(value & pos 0 string "main.calyx" & info [] ~doc ~docv:"PATH")
;;

let backend =
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
  let doc = "Compile Calyx source to target backend" in
  let info = Cmd.info "fingerpaint" ~doc in
  Cmd.v info Term.(const compile $ backend $ path)
;;

let () = exit (Cmd.eval compile_cmd)
