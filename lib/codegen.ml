open Util

module type StandardLibrary = sig
  type stage

  val builtins : stage Term.declaration list
end

module type M = sig
  val standard_library : (Ident.t * Env.entry) list
  val map_types : (Ident.t * Ident.t) list
  val native_infix : Ident.t list
  (** Command we can call to run generated code *)
  val run_program : string option
  val extension : string
  val compile : Ir.declaration list -> string
end

module WGSL : M = struct
  let standard_library =
    [ "Int", Env.Typed (`Opaque, `Type)
    ; ( "+"
      , Env.Typed
          ( `Lam ("x", fun x -> `Lam ("y", fun y -> `App (`App (`Var "+", x), y)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Int"))))
          ) )
    ; ( "succ"
      , Env.Typed
          ( `Lam ("x", fun x -> `App (`App (`Var "+", x), `Lit (Int 1)))
          , `Pi ("_", `Var "Int", fun _ -> `Var "Int") ) )
    ]
  ;;

  let run_program = None
  let extension = "wgsl"
  let map_types = [ "Int", "i32"; "UInt", "u32"; "Float", "f32" ]
  let native_infix = [ "+"; "-"; "*"; "/" ]
  let var = Fun.id
  let int = string_of_int
  let uint n = string_of_int n ^ "u"
  let float = string_of_float
  let bool = string_of_bool
  let app f x = Printf.sprintf "%s(%s)" f x

  let let_ ident ty value body =
    Printf.sprintf "let %s : %s = %s;\n%s" ident ty value body
  ;;

  let record_literal type_name fields =
    Printf.sprintf "%s(%s)" type_name (String.concat "," $ List.map snd fields)
  ;;

  let proj term field = Printf.sprintf "%s.%s" term field
  let emit = String.concat "\n"

  let fix_typenames : string -> string =
    let rec go ty =
      match List.assoc_opt ty map_types with
      | Some ty -> go ty
      | None -> ty
    in
    go
  ;;

  (* Compile a type to WGSL type syntax *)
  let rec compile_type : Ir.ty -> string = function
    | TVar "Int" -> "i32"
    | TVar "UInt" -> "u32"
    | TVar "Float" -> "f32"
    | TVar "Bool" -> "bool"
    | TVar name -> fix_typenames name (* Custom types *)
    | Skolem -> Ir.Fresh.get "A"
    | TFunction { returns; _ } -> compile_type returns
    | TApp (t, xs) ->
      Printf.sprintf
        "%s<%s>"
        (compile_type t)
        (String.concat ", " @@ List.map compile_type xs)
  ;;

  (* Add return to the innermost expression in a function body *)
  let rec add_return_to_final_expr : Ir.t -> string = function
    | Let (ident, ty, value, body) ->
      (match ty with
       | TFunction _ ->
         (* WGSL doesn't support first-class functions, so we inline function bindings *)
         let inline_function_calls expr =
           let rec subst : Ir.t -> Ir.t = function
             | Var name when name = ident -> value
             | App (name, args) when name = ident ->
               (match value with
                | Var fn_name -> App (fn_name, List.map subst args)
                | _ -> failwith "Expected function name for inlining")
             | App (fn_name, args) -> App (fn_name, List.map subst args)
             | Let (id, ty, v, b) -> Let (id, ty, subst v, subst b)
             | If (c, t, f) -> If (subst c, subst t, subst f)
             | Proj (e, field) -> Proj (subst e, field)
             | Infix (l, op, r) -> Infix (subst l, op, subst r)
             | other -> other
           in
           subst expr
         in
         add_return_to_final_expr (inline_function_calls body)
       | _ ->
         let wgsl_ty = compile_type ty in
         let compiled_value = compile_expr value in
         let compiled_body = add_return_to_final_expr body in
         Printf.sprintf "let %s : %s = %s;\n%s" ident wgsl_ty compiled_value compiled_body)
    | other -> Printf.sprintf "return %s;" (compile_expr other)

  (* Compile an expression to WGSL *)
  and compile_expr : Ir.t -> string = function
    | Var name -> var name
    | App (f, x) -> app f (String.concat ", " @@ List.map compile_expr x)
    | Infix (left, op, right) ->
      let left_expr = compile_expr left in
      let right_expr = compile_expr right in
      Printf.sprintf "(%s %s %s)" left_expr op right_expr
    | Let (ident, ty_opt, value, body) ->
      (match ty_opt with
       | TFunction _ ->
         let inline_function_calls expr =
           let rec subst : Ir.t -> Ir.t = function
             | Var name when name = ident -> value
             | App (name, args) when name = ident ->
               (match value with
                | Var fn_name -> App (fn_name, List.map subst args)
                | _ -> failwith "Expected function name for inlining")
             | App (fn_name, args) -> App (fn_name, List.map subst args)
             | Let (id, ty, v, b) -> Let (id, ty, subst v, subst b)
             | If (c, t, f) -> If (subst c, subst t, subst f)
             | Proj (e, field) -> Proj (subst e, field)
             | Infix (l, op, r) -> Infix (subst l, op, subst r)
             | other -> other
           in
           subst expr
         in
         compile_expr (inline_function_calls body)
       | _ ->
         let wgsl_ty = compile_type ty_opt in
         let compiled_value = compile_expr value in
         let compiled_body = compile_expr body in
         let_ ident wgsl_ty compiled_value compiled_body)
    | If (scrut, t, f) ->
      Printf.sprintf
        "select(%s, %s, %s)"
        (compile_expr t)
        (compile_expr f)
        (compile_expr scrut)
    | Match _ -> failwith "TODO"
    | Proj (term, field) -> proj (compile_expr term) field
    | Lit (Int n) -> int n
    | Lit (UInt n) -> uint n
    | Lit (Float x) -> float x
    | Lit (Bool b) -> bool b
    | Lit (Record fields) ->
      (* Assume it's a struct - we'd need more context to know the type name *)
      let compiled_fields =
        List.map (fun (name, expr) -> name, compile_expr expr) fields
      in
      record_literal "UnknownStruct" compiled_fields
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> string = function
    | Function { ident; args; returns; body } ->
      let annotation (x, t) = Printf.sprintf "%s: %s" x t in
      let args =
        String.concat ", "
        @@ List.map (Fun.compose annotation (Tuple.second compile_type)) args
      in
      let returns = compile_type returns in
      let body = add_return_to_final_expr body in
      Printf.sprintf "fn %s(%s) -> %s {\n  %s\n}" ident args returns body
    | Constant { ident; ty; value } ->
      Printf.sprintf "const %s: %s = %s;\n" ident (compile_type ty) (compile_expr value)
    | RecordType { ident; params = _; fields } ->
      let fields =
        String.concat ",\n  "
        @@ List.map
             (fun (field, ty) -> Printf.sprintf "%s: %s" field (compile_type ty))
             fields
      in
      Printf.sprintf "struct %s {\n  %s\n}\n" ident fields
  ;;

  (* Main compilation entry point *)
  let compile (decls : Ir.declaration list) : string =
    List.map compile_declaration decls |> emit |> Fun.flip String.cat "\n"
  ;;
end

module Javascript : M = struct
  let standard_library =
    [ "Int", Env.Typed (`Opaque, `Type)
    ; "Bool", Env.Typed (`Opaque, `Type)
    ; "Unit", Env.Typed (`Opaque, `Type)
    ; ( "+"
      , Env.Typed
          ( `Lam ("x", fun x -> `Lam ("y", fun y -> `App (`App (`Var "+", x), y)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Int"))))
          ) )
    ; ( "-"
      , Env.Typed
          ( `Lam ("x", fun x -> `Lam ("y", fun y -> `App (`App (`Var "-", x), y)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Int"))))
          ) )
    ; ( "*"
      , Env.Typed
          ( `Lam ("x", fun x -> `Lam ("y", fun y -> `App (`App (`Var "*", x), y)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Int"))))
          ) )
    ; ( "/"
      , Env.Typed
          ( `Lam ("x", fun x -> `Lam ("y", fun y -> `App (`App (`Var "/", x), y)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Int"))))
          ) )
    ; ( "=="
      , Env.Typed
          ( `Lam ("x", fun x -> `Lam ("y", fun y -> `App (`App (`Var "==", x), y)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Bool"))))
          ) )
    ; ( "succ"
      , Env.Typed
          ( `Lam ("x", fun x -> `App (`App (`Var "+", x), `Lit (Int 1)))
          , `Pi ("_", `Var "Int", fun _ -> `Var "Int") ) )
    ; ( "<"
      , Env.Typed
          ( `Lam ("a", fun a -> `Lam ("b", fun b -> `App (`App (`Var "<", a), b)))
          , `Pi
              ("_", `Var "Int", Fun.const (`Pi ("_", `Var "Int", Fun.const (`Var "Bool"))))
          ) )
    ; ( "print"
      , Env.Typed
          ( `Lam ("a", fun a -> `App (`Var "print", a))
          , `Pi ("_", `Var "Int", Fun.const (`Var "Unit")) ) )
    ]
  ;;

  let run_program = Some "node"
  let extension = "js"
  let map_types = []
  let native_infix = [ "+"; "-"; "*"; "/"; "<" ]
  let var = Fun.id
  let int = string_of_int
  let uint n = string_of_int n ^ "u"
  let float = string_of_float
  let bool = string_of_bool
  let app f x = Printf.sprintf "%s(%s)" f x
  let let_ ident value body = Printf.sprintf "const %s = %s;\n%s" ident value body
  let ternary scrut t f = Printf.sprintf "%s ? %s : %s" scrut t f
  (* We can always use ternaries in JavaScript *)

  let record_literal fields =
    Printf.sprintf
      "{\n%s\n}"
      (String.concat ",\n  "
       $ List.map (fun (ident, expr) -> Printf.sprintf "%s: %s" ident expr) fields)
  ;;

  let proj term field = Printf.sprintf "%s.%s" term field
  let emit = String.concat "\n"

  (* Add return to the innermost expression in a function body *)
  let rec add_return_to_final_expr : Ir.t -> string = function
    | Let (ident, _, value, body) ->
      let_ ident (compile_expr value) (add_return_to_final_expr body)
    | other -> Printf.sprintf "return %s;" (compile_expr other)

  and compile_expr : Ir.t -> string = function
    | Var name -> var name
    | App (f, x) -> app f (String.concat ", " @@ List.map compile_expr x)
    | Infix (left, op, right) ->
      let left_expr = compile_expr left in
      let right_expr = compile_expr right in
      Printf.sprintf "%s %s %s" left_expr op right_expr
    | Let (ident, _, value, body) -> let_ ident (compile_expr value) (compile_expr body)
    | If (scrut, t, f) ->
      let scrut, t, f = compile_expr scrut, compile_expr t, compile_expr f in
      ternary scrut t f
    | Match _ -> failwith "TODO"
    | Proj (term, field) -> proj (compile_expr term) field
    | Lit (Int n) -> int n
    | Lit (UInt n) -> uint n
    | Lit (Float x) -> float x
    | Lit (Bool b) -> bool b
    | Lit (Record fields) ->
      let compiled_fields =
        List.map (fun (name, expr) -> name, compile_expr expr) fields
      in
      record_literal compiled_fields
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> string = function
    | Function { ident; args; body; _ } ->
      let args = String.concat ", " @@ List.map fst args in
      let body = add_return_to_final_expr body in
      Printf.sprintf "const %s = (%s) => {\n  %s\n}" ident args body
    | Constant { ident; value; _ } ->
      Printf.sprintf "const %s = %s;\n" ident (compile_expr value)
    | RecordType _ -> ""
  ;;

  (* Main compilation entry point *)
  let compile (decls : Ir.declaration list) : string =
    List.map compile_declaration decls
    |> emit
    |> String.cat "const print = x => console.log(x);\n\n"
    |> Fun.flip String.cat "\n"
    |> Fun.flip String.cat "\n\nconsole.log(main())"
  ;;
end
