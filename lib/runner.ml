open Util

let compile (module Backend : Codegen.S) path =
  let go () =
    let contents = In_channel.open_text path |> In_channel.input_all in
    let* toplevels =
      Result.map_error (fun s -> ([ `Parser s ] :> Error.t list)) $ Parser.parse contents
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
