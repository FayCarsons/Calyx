open Core

(* TODO: Currently IR primarily serves to flatten function declarations and applications, as well as do lambda lifting. 
  But eventually we want to a CPS -> ANF transform
*)
module Fresh = struct
  type _ Effect.t += Get : string -> string Effect.t

  let get pref = Effect.perform (Get pref)

  let handle : (unit -> 'a) -> 'a =
    fun f ->
    let open Effect.Deep in
    let counter = ref (-1) in
    try_with
      f
      ()
      { effc =
          (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Get prefix ->
              Some
                (fun (k : (c, _) continuation) ->
                  incr counter;
                  continue k (prefix ^ string_of_int !counter))
            | eff ->
              Some (fun (k : (c, _) continuation) -> continue k (Effect.perform eff)))
      }
  ;;
end

type ty =
  | TVar of Ident.t
  | TFunction of
      { args : ty list
      ; returns : ty
      }
  | TApp of ty * ty list
  | Skolem
[@@deriving show, sexp]

type t =
  | Var of Ident.t
  | Lit of literal
  | App of Ident.t * t list
  | Let of Ident.t * ty * t * t
  | If of t * t * t
  | Match of t * (pattern * t) list
  | Proj of t * Ident.t
  | Infix of t * Ident.t * t

and literal =
  | Int of int
  | UInt of int
  | Float of float
  | Bool of bool
  (* TODO: This should hold an optional type name *)
  | Record of t Ident.Map.t

and pattern = string [@@deriving show, sexp]

type declaration =
  | Function of
      { ident : Ident.t
      ; args : (Ident.t * ty) list
      ; returns : ty
      ; body : t
      }
  | Constant of
      { ident : Ident.t
      ; ty : ty
      ; value : t
      }
  | RecordType of
      { ident : Ident.t
      ; params : ty Ident.Map.t
      ; fields : ty Ident.Map.t
      }

module PrettyIR = struct
  (* IR *)
  let rec ir : t -> string = function
    | Var x -> Ident.Intern.lookup x
    | Lit lit -> ir_literal lit
    | App (f, xs) ->
      Printf.sprintf
        "%s %s"
        (Ident.Intern.lookup f)
        (String.concat ~sep:", " @@ List.map ~f:ir xs)
    | Let (ident, ty, value, body) ->
      Printf.sprintf
        "let %s: %s = %s in\n%s"
        (Ident.Intern.lookup ident)
        (ir_ty ty)
        (ir value)
        (ir body)
    | If (scrut, t, f) ->
      Printf.sprintf "if %s then\n\t%s\nelse\n\t%s" (ir scrut) (ir t) (ir f)
    | Match (scrut, arms) ->
      let fmt_arm (pat, exp) = Printf.sprintf "| %s -> %s" pat exp in
      let arms =
        String.concat ~sep:"\n"
        @@ List.map ~f:(Fun.compose fmt_arm (Util.Tuple.bimap ir_pattern ir)) arms
      in
      Printf.sprintf "match %s with\n%s" (ir scrut) arms
    | Proj (tm, field) -> Printf.sprintf "%s.%s" (ir tm) (Ident.Intern.lookup field)
    | Infix (left, op, right) ->
      Printf.sprintf "%s %s %s" (ir left) (Ident.Intern.lookup op) (ir right)

  and ir_literal : literal -> string = function
    | Int n | UInt n -> string_of_int n
    | Float x -> string_of_float x
    | Bool b -> string_of_bool b
    | Record fields ->
      Map.to_alist fields
      |> List.map ~f:(fun (ident, value) ->
        Printf.sprintf "%s = %s" (Ident.Intern.lookup ident) (ir value))
      |> String.concat ~sep:", "
      |> Printf.sprintf "{ %s }"

  and ir_ty : ty -> string = function
    | TVar x -> Ident.Intern.lookup x
    | TFunction { args; returns } ->
      Printf.sprintf
        "(%s) -> %s"
        (String.concat ~sep:", " @@ List.map ~f:ir_ty args)
        (ir_ty returns)
    | TApp (f, xs) -> String.concat ~sep:" " @@ (ir_ty f :: List.map ~f:ir_ty xs)
    | Skolem -> "<Skolem>"

  and ir_pattern : pattern -> string = fun _ -> ""

  let declaration : declaration -> string = function
    | Function { ident; args; returns; body } ->
      let args =
        List.map
          ~f:(fun (ident, ty) ->
            Printf.sprintf "%s: %s" (Ident.Intern.lookup ident) (ir_ty ty))
          args
        |> String.concat ~sep:", "
      in
      let returns = ir_ty returns in
      let body = ir body in
      Printf.sprintf
        "fn %s(%s) -> %s {\n\t%s\n}\n\n"
        (Ident.Intern.lookup ident)
        args
        returns
        body
    | Constant { ident; ty; value } ->
      Printf.sprintf
        "let %s: %s = %s;\n\n"
        (Ident.Intern.lookup ident)
        (ir_ty ty)
        (ir value)
    | RecordType { ident; params; fields } ->
      let params =
        Map.to_alist params
        |> List.map ~f:(fun (ident, ty) ->
          Printf.sprintf "(%s : %s)" (Ident.Intern.lookup ident) (ir_ty ty))
        |> String.concat ~sep:" "
      in
      let fields =
        Map.to_alist fields
        |> List.map ~f:(fun (ident, ty) ->
          Printf.sprintf "%s : %s" (Ident.Intern.lookup ident) (ir_ty ty))
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "data %s %s where\n%s\n\n" (Ident.Intern.lookup ident) params fields
  ;;
end

module Context = struct
  module M = struct
    type t = declaration
  end

  include Writer.Make (M)
end

(* Shared lambda lifting utilities *)

let rec convert_expr : Term.ast -> t = function
  | `Var v -> Var v
  | `Lit literal -> Lit (convert_literal literal)
  | `App (f, x) ->
    let rec go acc = function
      | `App (f', x') -> go (convert_expr x' :: acc) f'
      | `Var ident -> App (ident, acc)
      | `Ann (x, _) -> go acc x
      | other ->
        failwith
        @@ Printf.sprintf "Illegal term in function position '%s'" (Term.show_ast other)
    in
    go [ convert_expr x ] f
  | `Infix Term.{ left; op; right } ->
    (match convert_expr op with
     | Var op -> Infix (convert_expr left, op, convert_expr right)
     | other ->
       failwith
       @@ Printf.sprintf
            "Illegal term in binary operator position '%s'"
            (PrettyIR.ir other))
  | `Proj (tm, field) -> Proj (convert_expr tm, field)
  | `Match (scrut, arms) ->
    let open Util in
    let arms' =
      Tuple.into
      <$> Stdlib.List.assoc_opt (Term.PVar (Ident.Intern.intern "True")) arms
      <*> Stdlib.List.assoc_opt (Term.PVar (Ident.Intern.intern "False")) arms
    in
    (match arms' with
     | Some (then_, else_) ->
       If (convert_expr scrut, convert_expr then_, convert_expr else_)
     | None -> failwith "Arbitrary pattern matching not implemented yet")
  | `Pos (_, tm) -> convert_expr tm
  | `Ann (`Lam (x, body), pi_type) -> convert_lambda_to_function pi_type x body
  | `Ann (x, _type) -> convert_expr x
  | `Lam (x, body) ->
    Printf.printf "Lam (%s, %s)" (Ident.Intern.lookup x) (Term.show_ast body);
    failwith "Fatal!"
  | `Let (ident, Some ty, value, body) ->
    Let (ident, convert_type ty, convert_expr (`Ann (value, ty)), convert_expr body)
  | `Err e ->
    failwith
    @@ Printf.sprintf "Error '%s' should not make it into codegen" (CalyxError.show e)
  | other ->
    failwith @@ Printf.sprintf "Illegal term in IR.convert: '%s'" (Term.show_ast other)

and convert_literal : Term.ast Term.literal -> literal = function
  | Term.Int n -> Int n
  | Term.UInt n -> UInt n
  | Term.Float x -> Float x
  | Term.Bool b -> Bool b
  | Term.Record fields -> Record (Map.map ~f:convert_expr fields)

(* Convert a Term.value type to IR type *)
and convert_type : Term.ast -> ty = function
  | `Var name -> TVar name
  | `Pi (_, dom, cod) ->
    (* Flatten curried functions: Int -> Int -> Int becomes TFunction { args = [Int; Int]; returns = Int } *)
    let rec collect_args acc = function
      | `Pi (_, dom, cod) -> collect_args (convert_type dom :: acc) cod
      | return_type -> List.rev acc, convert_type return_type
    in
    let args, returns = collect_args [ convert_type dom ] cod in
    TFunction { args; returns }
  | `App (f, x) ->
    (* For type applications like Maybe Int *)
    TApp (convert_type f, [ convert_type x ])
  | `Meta _ -> Skolem
  | `Ann (x, _) -> convert_type x
  | `Type -> TVar (Ident.Intern.intern "Type") (* Kind of types *)
  | other ->
    failwith
    @@ Printf.sprintf
         "Illegal term in type position in Ir.convert_type: '%s'"
         (Term.show_ast other)

(* Extract parameter types from nested Pi types *)
and extract_pi_types (pi_type : Term.ast) =
  match pi_type with
  | `Pi (_, dom, cod) ->
    let rest_types = extract_pi_types cod in
    convert_type dom :: rest_types
  | other -> [ convert_type other ]
(* Return type *)

(* Collect nested lambda parameters *)
and collect_lambda_params acc = function
  | `Ann (`Lam (x, body), _) -> collect_lambda_params (x :: acc) body
  | `Lam (x, body) -> collect_lambda_params (x :: acc) body
  | other -> acc, convert_expr other

(* Convert a lambda with its Pi type to a lifted function *)
and convert_lambda_to_function ?(name_prefix = "fn") pi_type first_param body =
  let function_name = Ident.Intern.intern @@ Fresh.get name_prefix in
  let param_types = extract_pi_types pi_type in
  let return_type = List.nth_exn param_types (List.length param_types - 1) in
  let arg_types = List.rev (List.tl_exn (List.rev param_types)) in
  let args, converted_body = collect_lambda_params [ first_param ] body in
  let args = List.rev args in
  (* Put back in correct order *)
  let typed_args = List.zip_exn args arg_types in
  let func_decl =
    Function
      { ident = function_name
      ; args = typed_args
      ; returns = return_type
      ; body = converted_body
      }
  in
  Context.tell func_decl;
  Var function_name
;;

let convert : Term.ast Term.declaration list -> declaration list =
  fun decls ->
  let go = function
    | Term.Function { ident; typ; body } ->
      (* This is a function - use typ (the Pi type) for parameter types *)
      let param_types = extract_pi_types typ in
      let return_type = List.nth_exn param_types (List.length param_types - 1) in
      let arg_types = List.rev (List.tl_exn (List.rev param_types)) in
      let args, converted_body = collect_lambda_params [] body in
      let args = List.rev args in
      (* Put back in correct order *)
      let typed_args = List.zip_exn args arg_types in
      Function { ident; args = typed_args; returns = return_type; body = converted_body }
    | Term.Constant { ident; typ; body } ->
      (* This is a constant declaration *)
      let value = convert_expr body in
      let ty = convert_type typ in
      Constant { ident; ty; value }
    | Term.RecordDecl { ident; params; fields } ->
      let params = Map.map ~f:convert_type params
      and fields = Map.map ~f:convert_type fields in
      RecordType { ident; params; fields }
  in
  let x, xs = Context.handle (fun () -> Fresh.handle (fun () -> List.map ~f:go decls)) in
  List.append x xs
;;
