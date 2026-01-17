open Core
open Util
module Intern = Ident.Intern

module type StandardLibrary = sig
  type stage

  val builtins : stage Term.declaration list
end

module type M = sig
  (** Standard library functions for this backend. 
      In the future we should be able to write these as S-expressions with access to compiler internals like 'Opaque' 
  *)
  val standard_library : Env.entry Ident.Map.t
  (** Type renaming, i.e. WGSL requires we rename 'Int' to 'i32' *)
  val map_types : Ident.t Ident.Map.t
  (** Native infix functions, do not need to be renamed *)
  val native_infix : Ident.t list
  (** Command we can call to run generated code *)
  val execute : string option
  val extension : string
  val compile : Ir.declaration list -> string
end

module type Render = sig
  type t
  type repr

  val int : t -> repr 
  val uint : t -> repr 
  val float : t -> repr
  val bool : t -> repr
  val app : f:t -> xs:t list -> repr
  val let_ : Ident.t -> typ:t -> value:t -> body:t -> repr
  val projection : t -> Ident.t list -> repr
  val type_rep : t -> repr 
end

module WGSL : M = struct
  let standard_library =
    let open Term in
    Ident.Map.of_alist_exn  [ Intern.intern "Int", Env.Typed (`Opaque, `Type)
    ; ( Intern.intern "+"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `Lam (Intern.intern  "y", fun y -> `App (`App (`Var (Intern.intern "+"), x), y)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Int")))))
          ) )
    ; ( Intern.intern "succ"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `App (`App (`Var (Intern.intern "+"), x), `Lit (Int 1)))
          , `Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), fun _ -> `Var (Intern.intern "Int")) ) )
    ]
  ;;

  let execute = None
  let extension = "wgsl"
  let map_types = Ident.Map.of_alist_exn @@ List.map ~f:(Tuple.both Intern.intern) [ "Int", "i32"; "UInt", "u32"; "Float", "f32" ]
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
         Printf.sprintf "let %s : %s = %s;\n%s" (Intern.lookup ident) wgsl_ty compiled_value compiled_body)
    | other -> Printf.sprintf "return %s;" (compile_expr other)

  (* Compile an expression to WGSL *)
  and compile_expr : Ir.t -> string = function
    | Var n -> var n
    | App (f, x) -> app f (String.concat ~sep:", " @@ List.map ~f:compile_expr x)
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
    | Match _ -> failwith "TODO"
    | Proj (term, field) -> proj (compile_expr term) field
    | Lit (Int n) -> int n
    | Lit (UInt n) -> uint n
    | Lit (Float x) -> float x
    | Lit (Bool b) -> bool b
    | Lit (Record fields) ->
      (* Assume it's a struct - we'd need more context to know the type name *)
      let compiled_fields =
        Map.to_alist  fields |> List.map ~f:(fun (k, v) -> Intern.lookup k, compile_expr v)
      in
      record_literal "UnknownStruct" compiled_fields
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> string = function
    | Function { ident; args; returns; body } ->
      let annotation (x, t) = Printf.sprintf "%s: %s" (Intern.lookup x) t in
      let args_str =
        String.concat ~sep:", "
        @@ List.map ~f:(Fun.compose annotation (Tuple.second compile_type)) args
      in
      let returns = compile_type returns in
      let body = add_return_to_final_expr body in
      Printf.sprintf "fn %s(%s) -> %s {\n  %s\n}" (Intern.lookup ident) args_str returns body
    | Constant { ident; ty; value } ->
      Printf.sprintf "const %s: %s = %s;\n" (Intern.lookup ident) (compile_type ty) (compile_expr value)
    | RecordType { ident; params = _; fields } ->
      let fields_str =
        Map.to_alist fields 
        |> List.map
          ~f:(fun (field, ty) -> Printf.sprintf "%s: %s" (Intern.lookup field) (compile_type ty))
        |> String.concat ~sep:",\n  "
      in
      Printf.sprintf "struct %s {\n  %s\n}\n" (Intern.lookup ident) fields_str
  ;;

  (* Main compilation entry point *)
  let compile (decls : Ir.declaration list) : string =
    List.map ~f:compile_declaration decls |> emit |> String.append "\n"
  ;;
end

module Javascript : M = struct
  let name = Intern.lookup

  let standard_library =
    let open Term in
    Ident.Map.of_alist_exn
    [ Intern.intern "Int", Env.Typed (`Opaque, `Type)
    ; Intern.intern "Bool", Env.Typed (`Opaque, `Type)
    ; Intern.intern "Unit", Env.Typed (`Opaque, `Type)
    ; ( Intern.intern "+"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `Lam (Intern.intern "y", fun y -> `App (`App (`Var (Intern.intern "+"), x), y)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Int")))))
          ) )
    ; ( Intern.intern "-"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `Lam (Intern.intern "y", fun y -> `App (`App (`Var (Intern.intern "-"), x), y)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Int")))))
          ) )
    ; ( Intern.intern "*"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `Lam (Intern.intern "y", fun y -> `App (`App (`Var (Intern.intern "*"), x), y)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Int")))))
          ) )
    ; ( Intern.intern "/"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `Lam (Intern.intern "y", fun y -> `App (`App (`Var (Intern.intern "/"), x), y)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Int")))))
          ) )
    ; ( Intern.intern "=="
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `Lam (Intern.intern "y", fun y -> `App (`App (`Var (Intern.intern "=="), x), y)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Bool")))))
          ) )
    ; ( Intern.intern "succ"
      , Env.Typed
          ( `Lam (Intern.intern "x", fun x -> `App (`App (`Var (Intern.intern "+"), x), `Lit (Int 1)))
          , `Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), fun _ -> `Var (Intern.intern "Int")) ) )
    ; ( Intern.intern "<"
      , Env.Typed
          ( `Lam (Intern.intern "a", fun a -> `Lam (Intern.intern "b", fun b -> `App (`App (`Var (Intern.intern "<"), a), b)))
          , `Pi
              (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Bool")))))
          ) )
    ; ( Intern.intern "print"
      , Env.Typed
          ( `Lam (Intern.intern "a", fun a -> `App (`Var (Intern.intern "print"), a))
          , `Pi (Explicit, Intern.underscore, `Var (Intern.intern "Int"), Fun.const (`Var (Intern.intern "Unit"))) ) )
    ]
  ;;

  let execute = Some "node"
  let extension = "js"
  let map_types = Ident.Map.empty
  let native_infix = List.map ~f:Intern.intern [ "+"; "-"; "*"; "/"; "<" ]
  let var = name
  let int = string_of_int
  let uint n = string_of_int n ^ "u"
  let float = string_of_float
  let bool = string_of_bool
  let app f x = Printf.sprintf "%s(%s)" (name f) x
  let let_ id value body = Printf.sprintf "const %s = %s;\n%s" (name id) value body

  (* We can always use ternaries in JavaScript *)
  let ternary scrut t f = Printf.sprintf "%s ? %s : %s" scrut t f

  let record_literal (fields : string Ident.Map.t) =
    Map.to_alist fields 
    |> List.map ~f:(fun (ident, value) -> 
      Printf.sprintf "%s: %s" (Intern.lookup ident) value
    ) 
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
    | Infix (left, op, right) ->
      let left_expr = compile_expr left in
      let right_expr = compile_expr right in
      Printf.sprintf "%s %s %s" left_expr (name op) right_expr
    | Let (id, _, value, body) -> let_ id (compile_expr value) (compile_expr body)
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
        Map.map ~f:compile_expr fields
      in
      record_literal compiled_fields
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> string = function
    | Function { ident; args; body; _ } ->
      let args_str = String.concat ~sep:", " @@ List.map ~f:(Fun.compose Ident.Intern.lookup fst) args in
      let body = add_return_to_final_expr body in
      Printf.sprintf "const %s = (%s) => {\n  %s\n}" (name ident) args_str body
    | Constant { ident; value; _ } ->
      Printf.sprintf "const %s = %s;\n" (name ident) (compile_expr value)
    | RecordType _ -> ""
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
