let () =
  let filename = Sys.argv.(1) in
  let codegen = Fingerpaint.Runner.compile (module Fingerpaint.Codegen.WGSL) filename in
  print_endline "WGSL OUTPUT:";
  print_endline codegen
;;
