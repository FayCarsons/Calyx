open Core
module Intern = Ident.Intern

module Javascript : Codegen.M = struct
  let name = Intern.lookup

  let standard_library =
    let open Term in
    Ident.Map.of_alist_exn
      [ Intern.intern "Int", Context.Typed (`Opaque, `Type)
      ; Intern.intern "Bool", Context.Typed (`Opaque, `Type)
      ; Intern.intern "Unit", Context.Typed (`Opaque, `Type)
      ; ( Intern.intern "+"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "x"
                , fun x ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "y"
                         , fun y ->
                             Result.return @@ `App (`App (`Var (Intern.intern "+"), x), y)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Int")) )
                     : (value, Calyx_error.t) result) ) ) )
      ; ( Intern.intern "-"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "x"
                , fun x ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "y"
                         , fun y ->
                             Result.return @@ `App (`App (`Var (Intern.intern "-"), x), y)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Int")) )) )
            ) )
      ; ( Intern.intern "*"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "x"
                , fun x ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "y"
                         , fun y ->
                             Result.return @@ `App (`App (`Var (Intern.intern "*"), x), y)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Int")) )) )
            ) )
      ; ( Intern.intern "/"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "x"
                , fun x ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "y"
                         , fun y ->
                             Result.return @@ `App (`App (`Var (Intern.intern "/"), x), y)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Int")) )) )
            ) )
      ; ( Intern.intern "=="
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "x"
                , fun x ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "y"
                         , fun y ->
                             Result.return @@ `App (`App (`Var (Intern.intern "=="), x), y)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Bool")) )) )
            ) )
      ; ( Intern.intern "succ"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "x"
                , fun x ->
                    Result.return
                    @@ `App (`App (`Var (Intern.intern "+"), x), `Lit (Int 1)) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , fun _ -> Result.return @@ `Var (Intern.intern "Int") ) ) )
      ; ( Intern.intern "<"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "a"
                , fun a ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "b"
                         , fun b ->
                             Result.return @@ `App (`App (`Var (Intern.intern "<"), a), b)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Bool")) )) )
            ) )
      ; ( Intern.intern ">"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "a"
                , fun a ->
                    Result.return
                    @@ `Lam
                         ( Explicit
                         , Intern.intern "b"
                         , fun b ->
                             Result.return @@ `App (`App (`Var (Intern.intern ">"), a), b)
                         ) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const
                    (Result.return
                     @@ `Pi
                          ( Explicit
                          , Intern.underscore
                          , `Var (Intern.intern "Int")
                          , Fun.const (Result.return @@ `Var (Intern.intern "Bool")) )) )
            ) )
      ; ( Intern.intern "print"
        , Context.Typed
            ( `Lam
                ( Explicit
                , Intern.intern "a"
                , fun a -> Result.return @@ `App (`Var (Intern.intern "print"), a) )
            , `Pi
                ( Explicit
                , Intern.underscore
                , `Var (Intern.intern "Int")
                , Fun.const (Result.return @@ `Var (Intern.intern "Unit")) ) ) )
      ]
  ;;

  let execute = Some "node"
  let extension = "js"
  let native_infix = List.map ~f:Intern.intern [ "+"; "-"; "*"; "/"; "<"; ">" ]
  let var = name
  let int = string_of_int
  let uint n = string_of_int n ^ "u"
  let float = string_of_float
  let bool = string_of_bool
  let app f x = Printf.sprintf "%s(%s)" (name f) x
  let let_ id value body = Printf.sprintf "const %s = %s;\n%s" (name id) value body

  (* We can *always* use ternaries in JavaScript *)
  let ternary scrut t f = Printf.sprintf "%s ? %s : %s" scrut t f

  let record_literal (fields : string Ident.Map.t) =
    Map.to_alist fields
    |> List.map ~f:(fun (ident, value) ->
      Printf.sprintf "%s: %s" (Intern.lookup ident) value)
    |> String.concat ~sep:",\n  "
    |> Printf.sprintf "{  %s\n}"
  ;;

  let proj term field = Printf.sprintf "%s.%s" term (name field)
  let emit = String.concat ~sep:"\n"

  (* Add return to the innermost expression in a function body *)
  let rec add_return_to_final_expr : Ir.t -> string = function
    | Let (ident, _, value, body) ->
      let_ ident (compile_expr value) (add_return_to_final_expr body)
    | other -> Printf.sprintf "return %s;" (compile_expr other)

  and compile_expr : Ir.t -> string = function
    | Var n -> var n
    | App (f, x) -> app f (String.concat ~sep:", " @@ List.map ~f:compile_expr x)
    | Ctor (tag, ctor_name, args) ->
      let ctor_str = name ctor_name in
      (match args with
       | [] -> Printf.sprintf "({ _tag: %d } /* %s */)" tag ctor_str
       | _ ->
         let fields =
           List.mapi args ~f:(fun i a -> Printf.sprintf "_%d: %s" i (compile_expr a))
         in
         Printf.sprintf
           "({ _tag: %d, %s } /* %s */)"
           tag
           (String.concat ~sep:", " fields)
           ctor_str)
    | Infix (left, op, right) ->
      let left_expr = compile_expr left in
      let right_expr = compile_expr right in
      Printf.sprintf "%s %s %s" left_expr (name op) right_expr
    | Let (id, _, value, body) -> let_ id (compile_expr value) (compile_expr body)
    | If (scrut, t, f) ->
      let scrut, t, f = compile_expr scrut, compile_expr t, compile_expr f in
      ternary scrut t f
    | Match (scrut, arms) -> compile_match (compile_expr scrut) arms
    | Proj (term, field) -> proj (compile_expr term) field
    | Lit (Int n) -> int n
    | Lit (UInt n) -> uint n
    | Lit (Float x) -> float x
    | Lit (Bool b) -> bool b
    | Lit (Record fields) ->
      let compiled_fields = Map.map ~f:compile_expr fields in
      record_literal compiled_fields

  and compile_with_bindings (scrut : string) (pat : Ir.pattern) (body : Ir.t) : string =
    let bindings = compile_pattern_bindings scrut pat in
    match bindings with
    | [] -> compile_expr body
    | _ ->
      Printf.sprintf
        "((%s) => %s)(%s)"
        (String.concat ~sep:", " @@ List.map ~f:fst bindings)
        (compile_expr body)
        (String.concat ~sep:", " @@ List.map ~f:snd bindings)

  and compile_match (scrut : string) (arms : (Ir.pattern * Ir.t) list) : string =
    match arms with
    | [] -> failwith "Empty match arms"
    | [ (pat, body) ] -> compile_with_bindings scrut pat body
    | (pat, body) :: rest ->
      let cond = compile_pattern_condition scrut pat in
      let body_expr = compile_with_bindings scrut pat body in
      Printf.sprintf "(%s) ? %s : %s" cond body_expr (compile_match scrut rest)

  and compile_pattern_condition (scrut : string) : Ir.pattern -> string = function
    | Ir.PVar _ -> "true"
    | Ir.PWild -> "true"
    | Ir.PCtor (tag, _, _) -> Printf.sprintf "%s._tag === %d" scrut tag
    | Ir.PLit lit ->
      (match lit with
       | Int n -> Printf.sprintf "%s === %d" scrut n
       | UInt n -> Printf.sprintf "%s === %d" scrut n
       | Float x -> Printf.sprintf "%s === %f" scrut x
       | Bool b -> Printf.sprintf "%s === %s" scrut (bool b)
       | Record _ -> failwith "Record literal patterns not supported")

  and compile_pattern_bindings (scrut : string) : Ir.pattern -> (string * string) list
    = function
    | Ir.PVar x -> [ name x, scrut ]
    | Ir.PWild -> []
    | Ir.PCtor (_, _, args) ->
      List.concat_mapi args ~f:(fun i pat ->
        let field_access = Printf.sprintf "%s._%d" scrut i in
        compile_pattern_bindings field_access pat)
    | Ir.PLit _ -> []
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> string = function
    | Function { ident; args; body; _ } ->
      let args_str =
        String.concat ~sep:", " @@ List.map ~f:(Fun.compose Ident.Intern.lookup fst) args
      in
      let body = add_return_to_final_expr body in
      Printf.sprintf "const %s = (%s) => {\n  %s\n}" (name ident) args_str body
    | Constant { ident; value; _ } ->
      Printf.sprintf "const %s = %s;\n" (name ident) (compile_expr value)
    | RecordType _ -> ""
    | SumType { ident = _; constructors; _ } ->
      (* Generate constructor functions that return tagged objects with integer tags *)
      (* Constructors are already sorted alphabetically, so index = tag *)
      List.mapi constructors ~f:(fun tag (ctor_name, args) ->
        let ctor_str = name ctor_name in
        match List.length args with
        | 0 -> Printf.sprintf "const %s = { _tag: %d };" ctor_str tag
        | n ->
          let params = List.init n ~f:(fun i -> Printf.sprintf "_%d" i) in
          let fields =
            List.mapi params ~f:(fun i p -> Printf.sprintf "_%d: %s" i p)
            |> String.concat ~sep:", "
          in
          Printf.sprintf
            "const %s = (%s) => ({ _tag: %d, %s });"
            ctor_str
            (String.concat ~sep:", " params)
            tag
            fields)
      |> String.concat ~sep:"\n"
  ;;

  (* Main compilation entry point *)
  let compile (decls : Ir.declaration list) : string =
    List.map ~f:compile_declaration decls
    |> emit
    |> String.append "const print = x => console.log(x);\n\n"
    |> String.append "\n"
    |> Fun.flip String.append "\n\nprint(main())"
  ;;
end

let%test_module "js emission invariants" =
  (module struct
    let compile_program decls =
      let result, state =
        Context.run ~bindings:Testgen.stdlib (fun () ->
          let inferred = Checker.infer_toplevel decls in
          Solve.solve ();
          List.map ~f:Zonk.zonk_toplevel inferred)
      in
      match result with
      | Ok zonked when List.is_empty state.Context.errors ->
        Some (Javascript.compile (Ir.convert zonked))
      | _ -> None
    ;;

    (* The emitted program is the tagged representation and nothing else: no
       encoding binder (every "$"-prefixed name) survives to the backend, and
       constructors/matches show up as [_tag] objects and tests. *)
    let%test_unit "emitted js is tagged objects only, never encodings" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make ~count:100 ~name:"js-clean" Testgen.arb_adt_with_depth
      @@ fun (spec, depth) ->
      match compile_program (Testgen.program spec depth) with
      | None -> false
      | Some js ->
        (not (String.contains js '$')) && String.is_substring js ~substring:"_tag"
    ;;

    let%test "constructor matches compile to tag tests" =
      let spec = Testgen.{ n_params = 0; ctor_fields = [ []; [ FRec ] ] } in
      match compile_program (Testgen.program spec 2) with
      | None -> false
      | Some js -> String.is_substring js ~substring:"._tag ==="
    ;;
  end)
;;
