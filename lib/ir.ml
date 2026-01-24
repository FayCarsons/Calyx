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
  | TRecord of ty Ident.Map.t
  | Skolem
[@@deriving show, sexp]

type t =
  | Var of Ident.t
  | Lit of literal
  | App of Ident.t * t list
  | Ctor of int * Ident.t * t list (* tag, name, args *)
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

and pattern =
  | PVar of Ident.t
  | PWild
  | PCtor of int * Ident.t * pattern list (* tag, name, args *)
  | PLit of literal
[@@deriving show, sexp]

type declaration =
  | Function of
      { ident : Ident.t
      ; args : (Ident.t * ty) list
      ; returns : ty
      ; body : t
      ; position : Pos.pos * Pos.pos
      }
  | Constant of
      { ident : Ident.t
      ; ty : ty
      ; value : t
      ; position : Pos.pos * Pos.pos
      }
  | RecordType of
      { ident : Ident.t
      ; params : ty Ident.Map.t
      ; fields : ty Ident.Map.t
      ; position : Pos.pos * Pos.pos
      }
  | SumType of
      { ident : Ident.t
      ; params : ty Ident.Map.t
      ; constructors : (Ident.t * ty list) list
      ; position : Pos.pos * Pos.pos
      }

module PrettyIR = struct
  (* IR *)
  let rec ir : t -> string = function
    | Var x -> Ident.Intern.lookup x
    | Lit lit -> ir_literal lit
    | App (f, xs) ->
      Printf.sprintf
        "%s(%s)"
        (Ident.Intern.lookup f)
        (String.concat ~sep:", " @@ List.map ~f:ir xs)
    | Ctor (_tag, name, args) ->
      Printf.sprintf
        "%s(%s)"
        (Ident.Intern.lookup name)
        (String.concat ~sep:", " @@ List.map ~f:ir args)
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
      Map.to_alist ~key_order:`Increasing fields
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
    | TRecord fields -> Ident.Map.show show_ty fields
    | Skolem -> "<Skolem>"

  and ir_pattern : pattern -> string = function
    | PVar x -> Ident.Intern.lookup x
    | PWild -> "_"
    | PCtor (_tag, name, args) ->
      Printf.sprintf
        "%s(%s)"
        (Ident.Intern.lookup name)
        (String.concat ~sep:", " @@ List.map ~f:ir_pattern args)
    | PLit lit -> ir_literal lit
  ;;

  let declaration : declaration -> string = function
    | Function { ident; args; returns; body; _ } ->
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
    | Constant { ident; ty; value; _ } ->
      Printf.sprintf
        "let %s: %s = %s;\n\n"
        (Ident.Intern.lookup ident)
        (ir_ty ty)
        (ir value)
    | RecordType { ident; params; fields; _ } ->
      let params =
        Map.to_alist ~key_order:`Increasing params
        |> List.map ~f:(fun (ident, ty) ->
          Printf.sprintf "(%s : %s)" (Ident.Intern.lookup ident) (ir_ty ty))
        |> String.concat ~sep:" "
      in
      let fields =
        Map.to_alist ~key_order:`Increasing fields
        |> List.map ~f:(fun (ident, ty) ->
          Printf.sprintf "%s : %s" (Ident.Intern.lookup ident) (ir_ty ty))
        |> String.concat ~sep:"\n"
      in
      Printf.sprintf "data %s %s where\n%s\n\n" (Ident.Intern.lookup ident) params fields
    | SumType { ident; params; constructors; _ } ->
      let params =
        Map.to_alist ~key_order:`Increasing params
        |> List.map ~f:(fun (ident, ty) ->
          Printf.sprintf "(%s : %s)" (Ident.Intern.lookup ident) (ir_ty ty))
        |> function
        | [] -> " "
        | xs -> String.concat ~sep:" " xs
      in
      let constructors =
        List.map constructors ~f:(fun (ctor, params) ->
          Ident.Intern.lookup ctor :: List.map ~f:ir_ty params)
        |> List.map ~f:(String.concat ~sep:" ")
        |> String.concat ~sep:"\n|  "
      in
      Printf.sprintf
        "data %s%swhere\n  %s\n\n"
        (Ident.Intern.lookup ident)
        params
        constructors
  ;;
end

module Context = struct
  module M = struct
    type t = declaration
  end

  include Writer.Make (M)
end

(* Constructor tag registry - maps constructor names to their alphabetical index *)
module CtorTags = struct
  type _ Effect.t += Lookup : Ident.t -> int option Effect.t

  let lookup name = Effect.perform (Lookup name)

  let handle (ctor_tags : int Ident.Map.t) (f : unit -> 'a) : 'a =
    let open Effect.Deep in
    try_with
      f
      ()
      { effc =
          (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Lookup name ->
              Some (fun (k : (c, _) continuation) -> continue k (Map.find ctor_tags name))
            | eff ->
              Some (fun (k : (c, _) continuation) -> continue k (Effect.perform eff)))
      }
  ;;
end

(* Shared lambda lifting utilities *)

let rec convert_expr : Term.t -> t = function
  | `Var v -> Var v
  | `Lit literal -> Lit (convert_literal literal)
  | `App (f, x) ->
    (* Check if an argument is type-level (its type is Type) and should be erased *)
    let is_type_arg = function
      | `Ann (_, `Type) -> true
      | _ -> false
    in
    let rec go acc = function
      | `App (f', x') ->
        (* Skip type-level arguments (implicit type params) *)
        if is_type_arg x' then go acc f' else go (convert_expr x' :: acc) f'
      | `Var ident -> App (ident, acc)
      | `Ann (x, _) -> go acc x
      | other ->
        failwith
        @@ Printf.sprintf "Illegal term in function position '%s'" (Term.show other)
    in
    (* Also check the outermost argument *)
    if is_type_arg x then go [] f else go [ convert_expr x ] f
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
    (* Special case: if-then-else on True/False *)
    let arms' =
      let open Option.Applicative_infix in
      Option.map
        ~f:Tuple.into
        (Stdlib.List.assoc_opt (Term.PVar (Ident.Intern.intern "True")) arms)
      <*> Stdlib.List.assoc_opt (Term.PVar (Ident.Intern.intern "False")) arms
    in
    (match arms' with
     | Some (then_, else_) ->
       If (convert_expr scrut, convert_expr then_, convert_expr else_)
     | None ->
       let converted_arms =
         List.map arms ~f:(fun (pat, body) -> convert_pattern pat, convert_expr body)
       in
       Match (convert_expr scrut, converted_arms))
  | `Pos (_, tm) -> convert_expr tm
  | `Ann ((`Lam _ as lam), pi_type) -> convert_lambda_to_function pi_type lam
  (* SAFETY: Should be caught in (`Ann (`Lam _)) case *)
  | `Lam _ -> assert false
  | `Ann (x, _type) -> convert_expr x
  | `Let (ident, Some ty, value, body) ->
    Let (ident, convert_type ty, convert_expr (`Ann (value, ty)), convert_expr body)
  | `Err e ->
    failwith
    @@ Printf.sprintf "Error '%s' should not make it into codegen" (CalyxError.show e)
  | other ->
    failwith @@ Printf.sprintf "Illegal term in IR.convert: '%s'" (Term.show other)

and convert_literal : Term.t Term.literal -> literal = function
  | Term.Int n -> Int n
  | Term.UInt n -> UInt n
  | Term.Float x -> Float x
  | Term.Bool b -> Bool b
  | Term.Record fields -> Record (Map.map ~f:convert_expr fields)

and convert_pattern : Term.t Term.pattern -> pattern = function
  | Term.PVar x -> PVar x
  | Term.PWild -> PWild
  | Term.PCtor (name, args) ->
    let tag = CtorTags.lookup name |> Option.value ~default:0 in
    PCtor (tag, name, List.map ~f:convert_pattern args)
  | Term.PLit lit -> PLit (convert_literal lit)
  | Term.PRec _ -> failwith "Record patterns not yet implemented"

(* Convert a Term.value type to IR type *)
and convert_type : Term.t -> ty = function
  | `Var name -> TVar name
  | `Pi { plicity = Explicit; dom; cod; _ } ->
    (* Flatten curried functions: Int -> Int -> Int becomes TFunction { args = [Int; Int]; returns = Int } *)
    let rec collect_args acc = function
      | `Pi Term.{ cod; _ } -> collect_args (convert_type dom :: acc) cod
      | return_type -> List.rev acc, convert_type return_type
    in
    let args, returns = collect_args [ convert_type dom ] cod in
    TFunction { args; returns }
  | `Pi { plicity = Implicit; cod; _ } -> convert_type cod
  | `App (f, x) ->
    (* For type applications like Maybe Int *)
    TApp (convert_type f, [ convert_type x ])
  | `RecordType Term.{ fields; tail } ->
    let open Term in
    let rec go (fields : t Ident.Map.t) : t option -> t Ident.Map.t = function
      | Some (`RecordType { fields = fields'; tail }) ->
        let fields : t Ident.Map.t =
          Map.merge fields fields' ~f:(fun ~key:_ data ->
            match data with
            | `Left v | `Right v | `Both (v, _) -> Some (v : t))
        in
        go fields tail
      | Some other ->
        failwith @@ Printf.sprintf "Expected record tail, got %s" (Term.show other)
      | None -> fields
    in
    let fields : ty Ident.Map.t = Map.map ~f:convert_type @@ go fields tail in
    TRecord fields
  | `Meta _ -> Skolem
  | `Ann (x, _) -> convert_type x
  | `Type -> TVar (Ident.Intern.intern "Type") (* Kind of types *)
  | `SumType { ident; _ } -> TVar ident (* Sum types become type variables in IR *)
  | other ->
    failwith
    @@ Printf.sprintf
         "Illegal term in type position in Ir.convert_type: '%s'"
         (Term.show other)

(* Extract parameter types from nested Pi types *)
and extract_pi_types (pi_type : Term.t) =
  match pi_type with
  | `Pi { plicity = Implicit; cod; _ } -> extract_pi_types cod
  | `Pi { dom; cod; _ } ->
    let rest_types = extract_pi_types cod in
    convert_type dom :: rest_types
  | other -> [ convert_type other ]
(* Return type *)

(* Collect nested lambda parameters *)
and collect_lambda_params (acc : Ident.t list) = function
  | `Ann (term, _) -> collect_lambda_params acc term
  | `Lam (Term.Implicit, _, body) -> collect_lambda_params acc body
  | `Lam (Term.Explicit, x, body) -> collect_lambda_params (x :: acc) body
  | other -> acc, convert_expr other

(* Convert a lambda with its Pi type to a lifted function *)
and convert_lambda_to_function : ?name_prefix:string -> Term.t -> Term.t -> t =
  fun ?(name_prefix = "fn") pi_type body ->
  let function_name = Ident.Intern.intern @@ Fresh.get name_prefix in
  let param_types = extract_pi_types pi_type in
  let return_type = List.nth_exn param_types (List.length param_types - 1) in
  let arg_types = List.rev (List.tl_exn (List.rev param_types)) in
  let args, converted_body = collect_lambda_params [] body in
  let args = List.rev args in
  (* Put back in correct order *)
  let typed_args = List.zip_exn args arg_types in
  let func_decl =
    Function
      { ident = function_name
      ; args = typed_args
      ; returns = return_type
      ; body = converted_body
      ; position = Pos.pos_empty, Pos.pos_empty
      }
  in
  Context.tell func_decl;
  Var function_name
;;

let convert : Term.t Term.declaration list -> declaration list =
  fun decls ->
  let go = function
    | Term.Function { ident; typ; body; position } ->
      (* This is a function - use typ (the Pi type) for parameter types *)
      let param_types = extract_pi_types typ in
      let return_type = List.nth_exn param_types (List.length param_types - 1) in
      let arg_types = List.rev (List.tl_exn (List.rev param_types)) in
      let args, converted_body = collect_lambda_params [] body in
      let args = List.rev args in
      (* Put back in correct order *)
      let args = List.zip_exn args arg_types in
      Function { ident; args; returns = return_type; body = converted_body; position }
    | Term.Constant { ident; typ; body; position } ->
      (* This is a constant declaration *)
      let value = convert_expr body in
      let ty = convert_type typ in
      Constant { ident; ty; value; position }
    | Term.RecordDecl { ident; params; fields; position } ->
      let params = Map.map ~f:convert_type params
      and fields = Map.map ~f:convert_type fields in
      RecordType { ident; params; fields; position }
    | Term.SumDecl { ident; params; constructors; position } ->
      let params = Map.map ~f:convert_type params in
      let constructors =
        Map.to_alist ~key_order:`Increasing constructors
        |> List.map ~f:(fun (name, args) -> name, List.map ~f:convert_type args)
      in
      SumType { ident; params; constructors; position }
  in
  let is_decl = function
    | Term.RecordDecl _ | Term.SumDecl _ -> -1
    | _ -> 1
  in
  (* FIXME: Currently we are just putting type declarations in the front as a poor replacement of topo sort
    This should be replaced with actual topo sort
  *)
  let sorted_declarations =
    List.sort ~compare:(fun a b -> Int.compare (is_decl a) (is_decl b)) decls
  in
  (* Build constructor tag map from all sum types *)
  let ctor_tags =
    List.fold sorted_declarations ~init:Ident.Map.empty ~f:(fun acc decl ->
      match decl with
      | Term.SumDecl { constructors; _ } ->
        let sorted_ctors = Map.to_alist ~key_order:`Increasing constructors in
        List.foldi sorted_ctors ~init:acc ~f:(fun idx acc (ctor_name, _) ->
          Map.set acc ~key:ctor_name ~data:idx)
      | _ -> acc)
  in
  let x, xs =
    Context.handle (fun () ->
      Fresh.handle (fun () ->
        CtorTags.handle ctor_tags (fun () -> List.map ~f:go sorted_declarations)))
  in
  List.append x xs
;;
