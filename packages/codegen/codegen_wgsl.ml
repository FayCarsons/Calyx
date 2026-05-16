open Core
open Util
module Intern = Ident.Intern

module WGSL : Codegen.M = struct
  let standard_library =
    let open Term in
    Ident.Map.of_alist_exn
      [ Intern.intern "Int", Context.Typed (`Opaque, `Type)
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
                    (Ok
                       (`Pi
                           ( Explicit
                           , Intern.underscore
                           , `Var (Intern.intern "Int")
                           , Fun.const (Ok (`Var (Intern.intern "Int"))) ))
                     : (value, Calyx_error.t) result) ) ) )
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
      ]
  ;;

  let execute = None
  let extension = "wgsl"

  let map_types =
    Ident.Map.of_alist_exn
    @@ List.map
         ~f:(Tuple.both Intern.intern)
         [ "Int", "i32"; "UInt", "u32"; "Float", "f32" ]
  ;;

  let native_infix : Ident.t list = List.map ~f:Intern.intern [ "+"; "-"; "*"; "/" ]
  let var = Intern.lookup
  let int = string_of_int
  let uint n = string_of_int n ^ "u"
  let float = string_of_float
  let bool = string_of_bool
  let app f x = Printf.sprintf "%s(%s)" (Intern.lookup f) x

  let let_ id ty value body =
    Printf.sprintf "let %s : %s = %s;\n%s" (Intern.lookup id) ty value body
  ;;

  let record_literal type_name fields =
    Printf.sprintf "%s(%s)" type_name (String.concat ~sep:"," @@ List.map ~f:snd fields)
  ;;

  let proj term field = Printf.sprintf "%s.%s" term (Intern.lookup field)
  let emit = String.concat ~sep:"\n"

  let fix_typenames : string -> string =
    let rec go ty =
      match Map.find map_types (Intern.intern ty) with
      | Some ty_ident -> go (Intern.lookup ty_ident)
      | None -> ty
    in
    go
  ;;

  (* Compile a type to WGSL type syntax *)
  let rec compile_type : Ir.ty -> string = function
    | TVar ident ->
      let name = Intern.lookup ident in
      (match name with
       | "Int" -> "i32"
       | "UInt" -> "u32"
       | "Float" -> "f32"
       | "Bool" -> "bool"
       | _ -> fix_typenames name)
    | Skolem -> Ir.Fresh.get "A"
    | TFunction { returns; _ } -> compile_type returns
    | TRecord fields -> Printf.sprintf "struct R %s" (Ident.Map.show Ir.show_ty fields)
    | TApp (t, xs) ->
      Printf.sprintf
        "%s<%s>"
        (compile_type t)
        (String.concat ~sep:", " @@ List.map ~f:compile_type xs)
  ;;

  (* Add return to the innermost expression in a function body *)
  let rec add_return_to_final_expr : Ir.t -> string = function
    | Let (ident, ty, value, body) ->
      (match ty with
       | TFunction _ ->
         (* WGSL doesn't support first-class functions, so we inline function bindings *)
         let inline_function_calls expr =
           let rec subst : Ir.t -> Ir.t = function
             | Var var_name when Ident.equal var_name ident -> value
             | App (app_name, args) when Ident.equal app_name ident ->
               (match value with
                | Var fn_name -> App (fn_name, List.map ~f:subst args)
                | _ -> failwith "Expected function name for inlining")
             | App (fn_name, args) -> App (fn_name, List.map ~f:subst args)
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
         Printf.sprintf
           "let %s : %s = %s;\n%s"
           (Intern.lookup ident)
           wgsl_ty
           compiled_value
           compiled_body)
    | other -> Printf.sprintf "return %s;" (compile_expr other)

  (* Compile an expression to WGSL *)
  and compile_expr : Ir.t -> string = function
    | Var n -> var n
    | App (f, x) -> app f (String.concat ~sep:", " @@ List.map ~f:compile_expr x)
    | Ctor _ -> failwith "Sum types not supported in WGSL backend"
    | Infix (left, op, right) ->
      let left_expr = compile_expr left in
      let right_expr = compile_expr right in
      Printf.sprintf "(%s %s %s)" left_expr (Intern.lookup op) right_expr
    | Let (ident, ty_opt, value, body) ->
      (match ty_opt with
       | TFunction _ ->
         let inline_function_calls expr =
           let rec subst : Ir.t -> Ir.t = function
             | Var name when Ident.equal name ident -> value
             | App (name, args) when Ident.equal name ident ->
               (match value with
                | Var fn_name -> App (fn_name, List.map ~f:subst args)
                | _ -> failwith "Expected function name for inlining")
             | App (fn_name, args) -> App (fn_name, List.map ~f:subst args)
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
    | Match _ -> failwith "Pattern matching not supported in WGSL backend"
    | Proj (term, field) -> proj (compile_expr term) field
    | Lit (Int n) -> int n
    | Lit (UInt n) -> uint n
    | Lit (Float x) -> float x
    | Lit (Bool b) -> bool b
    | Lit (Record fields) ->
      (* Assume it's a struct - we'd need more context to know the type name *)
      let compiled_fields =
        Map.to_alist fields |> List.map ~f:(fun (k, v) -> Intern.lookup k, compile_expr v)
      in
      record_literal "UnknownStruct" compiled_fields
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> string = function
    | Function { ident; args; returns; body; _ } ->
      let annotation (x, t) = Printf.sprintf "%s: %s" (Intern.lookup x) t in
      let args_str =
        String.concat ~sep:", "
        @@ List.map ~f:(Fun.compose annotation (Tuple.second compile_type)) args
      in
      let returns = compile_type returns in
      let body = add_return_to_final_expr body in
      Printf.sprintf
        "fn %s(%s) -> %s {\n  %s\n}"
        (Intern.lookup ident)
        args_str
        returns
        body
    | Constant { ident; ty; value; _ } ->
      Printf.sprintf
        "const %s: %s = %s;\n"
        (Intern.lookup ident)
        (compile_type ty)
        (compile_expr value)
    | RecordType { ident; params = _; fields; _ } ->
      let fields_str =
        Map.to_alist fields
        |> List.map ~f:(fun (field, ty) ->
          Printf.sprintf "%s: %s" (Intern.lookup field) (compile_type ty))
        |> String.concat ~sep:",\n  "
      in
      Printf.sprintf "struct %s {\n  %s\n}\n" (Intern.lookup ident) fields_str
    | SumType _ -> failwith "Sum types not supported in WGSL backend"
  ;;

  let compile (decls : Ir.declaration list) : string =
    List.map ~f:compile_declaration decls |> emit |> String.append "\n"
  ;;
end
