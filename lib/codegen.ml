open Util

module type StandardLibrary = sig
  type stage

  val builtins : stage Term.declaration list
end

module type S = sig
  type expr
  type ty
  type declaration

  val standard_library : (Ident.t * Env.entry) list
  val map_types : (Ident.t * Ident.t) list
  val native_infix : Ident.t list
  val var : string -> expr
  val int : int -> expr
  val uint : int -> expr
  val float : float -> expr
  val bool : bool -> expr
  val app : expr -> expr -> expr
  val let_ : Ident.t -> ty -> expr -> expr -> expr
  val if_ : expr -> expr -> expr -> expr
  val record : Ident.t -> (Ident.t * expr) list -> expr
  val proj : expr -> Ident.t -> expr
  val function_declaration : Ident.t -> expr -> expr -> expr -> declaration
  val constant_declaration : Ident.t -> expr -> expr -> declaration
  val emit : declaration list -> string
  val compile : Ir.declaration list -> string
end

module WGSL : S = struct
  type expr = string
  type ty = string
  type declaration = string

  let standard_library =
    [ "Int", Env.Typed (`Var "Int", `Type)
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

  let map_types = [ "Int", "i32"; "UInt", "u32"; "Float", "f32" ]
  let native_infix = [ "+"; "-"; "*"; "/" ]
  let var = Fun.id
  let int = string_of_int
  let uint n = string_of_int n ^ "u"
  let float = string_of_float
  let bool = string_of_bool
  let app f x = Printf.sprintf "%s(%s)" f x
  let let_ ident ty value body = Printf.sprintf "let %s%s = %s;\n%s" ident ty value body
  let if_ cond t f = Printf.sprintf "select(%s, %s, %s)" f t cond

  let record type_name fields =
    Printf.sprintf "%s(%s)" type_name (String.concat "," $ List.map snd fields)
  ;;

  let proj term field = Printf.sprintf "%s.%s" term field

  let function_declaration name args return_type body =
    Printf.sprintf "fn %s(%s) -> %s {\n%s\n}\n" name args return_type body
  ;;

  let constant_declaration name typ value =
    Printf.sprintf "const %s: %s = %s;\n" name typ value
  ;;

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
  let rec compile_type : Ir.ty -> ty = function
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

  (* Compile an expression to WGSL *)
  let rec compile_expr : Ir.t -> expr = function
    | Var name -> var name
    | App (f, x) -> app f (String.concat ", " @@ List.map compile_expr x)
    | Infix (left, op, right) ->
      let left_expr = compile_expr left in
      let right_expr = compile_expr right in
      Printf.sprintf "(%s %s %s)" left_expr op right_expr
    | Let (ident, ty_opt, value, body) ->
      let wgsl_ty = compile_type ty_opt in
      let compiled_value = compile_expr value in
      let compiled_body = compile_expr body in
      let_ ident wgsl_ty compiled_value compiled_body
    | If (scrut, t, f) ->
      Printf.sprintf
        "select(%s, %s, %s)"
        (compile_expr t)
        (compile_expr f)
        (compile_expr scrut)
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
      record "UnknownStruct" compiled_fields
  ;;

  (* Compile a top-level declaration *)
  let compile_declaration : Ir.declaration -> declaration = function
    | Function { ident; args; returns; body } ->
      let annotation (x, t) = Printf.sprintf "%s: %s" x t in
      let args =
        String.concat ", "
        @@ List.map (Fun.compose annotation (Tuple.second compile_type)) args
      in
      let returns = compile_type returns in
      let body = compile_expr body in
      Printf.sprintf "fn %s(%s) -> %s {\n%s\n}" ident args returns body
    | Constant { ident; ty; value } ->
      Printf.sprintf "const %s: %s = %s;" ident (compile_type ty) (compile_expr value)
    | RecordType _fields -> failwith "todo"
  ;;

  (* Main compilation entry point *)
  let compile (decls : Ir.declaration list) : string =
    List.map compile_declaration decls |> emit |> Fun.flip String.cat "\n"
  ;;
end
