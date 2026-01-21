open Core
open Util
open Term
module Intern = Ident.Intern

let over_literal (f : 'a -> 'b) : 'a Term.literal -> 'b Term.literal = function
  | Int n -> Int n
  | UInt n -> UInt n
  | Float x -> Float x
  | Bool b -> Bool b
  | Record fields -> Record (Map.map ~f fields)
;;

let rec eval : Term.ast -> Term.value = function
  | `Var name ->
    (match Env.lookup_value name with
     | Some `Opaque -> `Neutral (NVar (0, name))
     | Some other -> other
     | None -> `Neutral (NVar (0, name)))
  | `Ann (e, _) -> eval e
  | `Type -> `Type
  | `Pi { plicity; ident; dom; cod } ->
    let dom_val = eval dom in
    (* Capture current environment for lexical scoping *)
    let snapshot = Env.ask () in
    `Pi
      ( plicity
      , ident
      , dom_val
      , fun v ->
          Env.with_snapshot
            (Env.with_binding ident ~value:v ~typ:dom_val snapshot)
            (fun () -> eval cod) )
  | `Lam (plicity, x, body) ->
    let snapshot = Env.ask () in
    `Lam
      ( plicity
      , x
      , fun v ->
          Env.with_snapshot (Env.with_binding x ~value:v snapshot) (fun () -> eval body)
      )
  | `App (f, x) -> app (eval f) (eval x)
  | `Infix { left; op; right } ->
    let op_val = eval op
    and left_val = eval left
    and right_val = eval right in
    app (app op_val left_val) right_val
  | `Let (ident, Some ty, value, body) ->
    let typ = eval ty
    and value = eval value in
    Env.local ~f:(Env.with_binding ident ~value ~typ) (fun () -> eval body)
  | `Let (ident, None, value, body) ->
    let value = eval value in
    Env.local ~f:(Env.with_binding ident ~value) (fun () -> eval body)
  | `Match (scrut, arms) ->
    `Match (eval scrut, List.map ~f:(Tuple.bimap pattern eval) arms)
  | `Pos (pos, exp) -> Env.local ~f:(Env.with_pos pos) (fun () -> eval exp)
  | `Lit lit -> `Lit (over_literal eval lit)
  | `Meta m -> `Neutral (NMeta m)
  | `Err e -> `Err e
  | `RecordType { fields; tail } ->
    `RecordType { fields = Map.map ~f:eval fields; tail = Option.map tail ~f:eval }
  | `SumType { ident; params; constructors; position } ->
    let params = Map.map params ~f:eval
    and constructors = Map.map constructors ~f:(List.map ~f:eval) in
    `SumType { ident; params; constructors; position }
  | `Proj (term, field) -> proj field (eval term)

and app f x =
  match f with
  (* TODO: Should differentiate between implicit and explicit app? *)
  | `Lam (_, _, body) -> body x
  | `Neutral n -> `Neutral (NApp (n, x))
  | `SumType { ident; params; constructors; position } ->
    (* Apply a sum type to a type argument - substitute the first parameter *)
    (match Map.to_alist ~key_order:`Increasing params with
     | [] -> failwith "Cannot apply saturated sum type"
     | (param_name, _) :: rest_params ->
       (* Substitute param_name with x in the constructors *)
       let subst_value v =
         match v with
         | `Neutral (NVar (_, name)) when Ident.equal name param_name -> x
         | other -> other
       in
       let constructors = Map.map constructors ~f:(List.map ~f:subst_value) in
       let params = Ident.Map.of_alist_exn rest_params in
       `SumType { ident; params; constructors; position })
  | other -> failwith @@ Printf.sprintf "Apply non-function: '%s'" (Term.show_value other)

and proj (field : Ident.t) : Term.value -> Term.value = function
  | `Lit (Record fields) -> Map.find_exn fields field
  | `Neutral n -> `Neutral (NProj (n, field))
  | _ -> failwith "Projection from non-record"

and pattern : Term.ast Term.pattern -> Term.value Term.pattern = function
  | PVar x -> PVar x
  | PWild -> PWild
  | PCtor (ctor, args) -> PCtor (ctor, List.map ~f:pattern args)
  | PLit lit -> PLit (over_literal eval lit)
  | PRec fields -> PRec (List.map ~f:(Tuple.second pattern) fields)

(* Resolve a type to a SumType, looking up neutral variables in the environment *)
and resolve_sum_type : Term.value -> Term.value Term.sum_type option = function
  | `SumType s -> Some s
  | `Neutral (NVar (_, name)) ->
    (* Look up the name in the environment to find the actual SumType *)
    (match Env.lookup_value name with
     | Some (`SumType s) -> Some s
     | _ -> None)
  | _ -> None

(* Extract bindings from a pattern given the scrutinee type *)
and pattern_bindings
  : Term.value Term.pattern -> Term.value -> (Ident.t * Term.value) list
  =
  fun pat scrut_ty ->
  match pat with
  | PVar x -> [ x, scrut_ty ]
  | PWild -> []
  | PLit _ -> []
  (* TODO: Record pattern bindings *)
  | PRec _ -> []
  | PCtor (ctor_name, args) ->
    (* Extract constructor argument types from the scrutinee's sum type *)
    (match resolve_sum_type scrut_ty with
     | Some { constructors; _ } ->
       (* If this isn't found in the [constructors] map then something has really gone wrong *)
       Map.find_exn constructors ctor_name
       |> List.map2_exn args ~f:pattern_bindings
       |> List.concat
     | None -> [])
;;

let rec quote (lvl : int) : Term.value -> Term.ast = function
  | `App (f, x) -> `App (quote lvl f, quote lvl x)
  | `Neutral n -> quote_neutral lvl n
  | `Type -> `Type
  | `Pi (plicity, ident, dom, cod) ->
    (* level shouldn't matter here because [var] is just used to access the body of [cod] *)
    let var = `Neutral (NVar (0, ident)) in
    `Pi { plicity; ident; dom = quote lvl dom; cod = quote (succ lvl) (cod var) }
  | `Lam (plicity, x, b) ->
    let var = `Neutral (NVar (0, x)) in
    `Lam (plicity, x, quote (succ lvl) (b var))
  | `Lit lit -> `Lit (Term.over_literal (quote lvl) lit)
  | `RecordType { fields; tail } ->
    let fields = Map.map ~f:(quote lvl) fields
    and tail = Option.map tail ~f:(quote lvl) in
    `RecordType { fields; tail }
  | `SumType { ident; params; constructors; position } ->
    `SumType
      { ident
      ; params = Map.map ~f:(quote lvl) params
      ; constructors = Map.map ~f:(List.map ~f:(quote lvl)) constructors
      ; position
      }
  | `Err e -> `Err e
  | `Opaque -> failwith "Cannot quote opaque values, they should not appear here"
  | #base as b -> (Term.map_base (quote lvl) b :> Term.ast)

and quote_neutral (lvl : int) : neutral -> Term.ast = function
  | NVar (_, x) -> `Var x
  | NApp (f, x) -> `App (quote_neutral lvl f, quote lvl x)
  | NMeta m -> `Meta m
  | NProj (term, field) -> `Proj (quote_neutral lvl term, field)
;;

let rec infer : Term.ast -> (Term.value * Term.ast, CalyxError.t) result = function
  | `Var i ->
    let* ty = Env.lookup_type i |> Result.of_option ~error:(`NotFound i) in
    Ok (ty, `Ann (`Var i, quote 0 ty))
  | `Pi { plicity; ident; dom; cod } ->
    let* value = Result.map ~f:eval @@ check dom `Type in
    let* _ = Env.local ~f:(Env.with_binding ident ~value) (fun () -> check cod `Type) in
    Ok (`Type, `Pi { plicity; ident; dom; cod })
  | `Lam (plicity, x, body) ->
    let dom = `Neutral (NMeta (Meta.fresh (Env.level ()))) in
    let* body_ty, body = Env.fresh_var x dom (fun _ -> infer body) in
    let ty = `Pi (plicity, x, dom, Fun.const body_ty) in
    Ok (ty, `Ann (`Lam (plicity, x, body), quote 0 ty))
  | `App (f, x) ->
    let* tf, f = infer f in
    (* Insert meta-variables for implicit parameters until we hit an explicit one *)
    let rec insert_implicits tf f =
      match Solve.force tf with
      | `Pi (Implicit, _ident, dom, cod) ->
        (* Insert a meta for implicit argument *)
        let meta = Meta.fresh @@ Env.level () in
        let meta_val = `Neutral (NMeta meta) in
        let new_f = `App (f, `Ann (`Meta meta, quote 0 dom)) in
        insert_implicits (cod meta_val) new_f
      | `Pi (Explicit, _, dom, cod) ->
        (* Normal explicit application *)
        let* x' = check x dom in
        let result_ty = cod (`Neutral (NVar (0, Intern.underscore))) in
        Ok (result_ty, `Ann (`App (f, x'), quote 0 result_ty))
      | `Neutral (NMeta _) ->
        (* Unknown function type, create constraints *)
        let dom = `Neutral (NMeta (Meta.fresh @@ Env.level ())) in
        let cod = `Neutral (NMeta (Meta.fresh @@ Env.level ())) in
        Solve.Constraints.(
          tell @@ Equals (tf, `Pi (Explicit, Intern.underscore, dom, Fun.const cod)));
        let* x' = check x dom in
        Ok (cod, `Ann (`App (f, x'), quote 0 cod))
      | otherwise -> Error (`Expected ("function", Term.show_value otherwise))
    in
    insert_implicits tf f
  | `Infix { left; op; right } ->
    (* Treat infix like nested application *)
    let app_expr = `App (`App (op, left), right) in
    let* ty, checked_app = infer app_expr in
    (match checked_app with
     | `Ann (`App (`App (op, left), right), result_ty) ->
       let infix_expr = `Infix { left; op; right } in
       Ok (ty, `Ann (infix_expr, result_ty))
     | _ ->
       let* _, left = infer left in
       let* _, op = infer op in
       let* _, right = infer right in
       Ok (ty, `Infix { left; op; right }))
  | `Let (ident, typ, value, body) ->
    let* typ =
      match typ with
      | Some t ->
        let* _ = check t `Type in
        Ok (eval t)
      | None -> Ok (`Neutral (NMeta (Meta.fresh @@ Env.level ())))
    in
    let* value' = check value typ in
    let* body_ty, body =
      Env.local
        ~f:(Env.with_binding ident ~value:(eval value') ~typ)
        (fun () -> infer body)
    in
    Ok (body_ty, `Ann (`Let (ident, Some (quote 0 typ), value', body), quote 0 body_ty))
  | `Ann (e, a) ->
    let* _ = check a `Type in
    let vt = eval a in
    let* e = check e vt in
    Ok (vt, `Ann (e, a))
  | `Type ->
    (* print_endline "infer.Type"; *)
    Ok (`Type, `Type)
  | `Proj (term, field) ->
    (* print_endline "infer.Proj"; *)
    infer_proj term field
  | `Pos (p, term) ->
    (* print_endline "infer.Pos"; *)
    Env.local ~f:(Env.with_pos p) (fun () -> infer term)
  | `Match (scrut, arms) ->
    (* print_endline "infer.Match"; *)
    let* scrut_ty, scrut = infer scrut in
    let infer_arm (pat, body) =
      let val_pat = pattern pat in
      let bindings = pattern_bindings val_pat scrut_ty in
      (* Introduce pattern bindings into environment *)
      Env.local
        ~f:(fun env ->
          List.fold_right bindings ~init:env ~f:(fun (ident, typ) ->
            Env.with_binding ident ~value:(`Neutral (NVar (Env.level (), ident))) ~typ))
        (fun () ->
           let* body_ty, body = infer body in
           Ok ((pat, `Ann (body, quote 0 body_ty)), body_ty))
    in
    let* arms_and_types = sequence @@ List.map ~f:infer_arm arms in
    let annotated_arms = List.map ~f:fst arms_and_types in
    let arm_types = List.map ~f:snd arms_and_types in
    (match arm_types with
     | [] -> Error (`Expected ("non-empty match", Term.show_ast (`Match (scrut, arms))))
     | first_ty :: rest_types ->
       let unify_first ty = Solve.Constraints.(tell @@ Equals (first_ty, ty)) in
       List.iter ~f:unify_first rest_types;
       Ok
         ( first_ty
         , `Ann (`Match (`Ann (scrut, quote 0 scrut_ty), annotated_arms), quote 0 first_ty)
         ))
  | `Lit lit ->
    (* print_endline "infer.Lit"; *)
    let* ty, lit' = infer_lit lit in
    Ok (ty, `Ann (`Lit lit', quote 0 ty))
  | `Meta m ->
    (* print_endline "infer.Meta"; *)
    Ok (`Neutral (NMeta m), `Meta m)
  | `Err e ->
    (* print_endline "infer.Err"; *)
    Ok (`Err e, `Err e)
  | `RecordType { fields; tail } ->
    let record_val : Term.value =
      let fields = Map.map fields ~f:eval
      and tail = Option.map tail ~f:eval in
      `RecordType { fields; tail }
    in
    Ok (`Type, `Ann (`RecordType { fields; tail }, quote 0 record_val))
  | `SumType { ident; params; constructors; position } ->
    (* Sum types have type Type *)
    let sum_val =
      `SumType
        { ident
        ; params = Map.map ~f:eval params
        ; constructors = Map.map ~f:(List.map ~f:eval) constructors
        ; position
        }
    in
    Ok (`Type, `Ann (`SumType { ident; params; constructors; position }, quote 0 sum_val))

and infer_lit
  : Term.ast Term.literal -> (Term.value * Term.ast Term.literal, CalyxError.t) result
  = function
  | Int n -> Ok (`Neutral (NVar (0, Intern.intern "Int")), Int n)
  | UInt n -> Ok (`Neutral (NVar (0, Intern.intern "UInt")), UInt n)
  | Float x -> Ok (`Neutral (NVar (0, Intern.intern "Float")), Float x)
  | Bool b -> Ok (`Neutral (NVar (0, Intern.intern "Bool")), Bool b)
  | Record fields ->
    let* fields : (Ident.t * (Term.value * Term.ast)) list =
      Map.to_alist fields
      |> List.map ~f:(fun (field, data) ->
        let* ty, value = infer data in
        Ok (field, (ty, value)))
      |> sequence
    in
    let field_values : (Ident.t * Term.ast) list = List.map ~f:(Tuple.second snd) fields
    and field_types : (Ident.t * Term.value) list =
      List.map ~f:(Tuple.second fst) fields
    in
    let record_type : Term.value =
      let fields = Ident.Map.of_alist_exn field_types in
      `RecordType { fields; tail = None }
    in
    let fields = Ident.Map.of_alist_exn field_values in
    Ok (record_type, Record fields)

(** Infer the type of [field] in [term] *)
and infer_proj : Term.ast -> Ident.t -> (Term.value * Term.ast, CalyxError.t) result =
  fun term field ->
  let* typ, annotated = infer term in
  match typ with
  | `RecordType row ->
    (match Map.find row.fields field with
     | Some field_type -> Ok (field_type, `Proj (annotated, field))
     | None ->
       (match row.tail with
        | Some tail ->
          let field_type = `Neutral (NMeta (Meta.fresh @@ Env.level ())) in
          Solve.Constraints.(tell @@ has_field ~record:tail ~field_name:field ~field_type);
          Ok (field_type, `Proj (annotated, field))
        | None ->
          Error (`NoField (field, Map.to_alist @@ Map.map ~f:Term.show_value row.fields))))
  | `Neutral n ->
    let field_type = `Neutral (NMeta (Meta.fresh @@ Env.level ())) in
    let partial : Term.value =
      let fields = Ident.Map.singleton field field_type
      and tail = Some (`Neutral (NMeta (Meta.fresh @@ Env.level ()))) in
      `RecordType { fields; tail }
    in
    Solve.Constraints.(tell @@ equals (`Neutral n) partial);
    Ok (field_type, `Proj (annotated, field))
  | other -> Error (`Expected ("Record", Term.show_value other))

and check : Term.ast -> Term.value -> (Term.ast, CalyxError.t) result =
  fun term expected ->
  match term, expected with
  | `Lam (plicity, x, body), `Pi (plicity', _, dom, cod) ->
    if Term.equal_plicity plicity plicity'
    then
      let* body' = Env.fresh_var x dom (fun var -> check body (cod var)) in
      Ok (`Ann (`Lam (plicity, x, body'), quote 0 expected))
    else
      Error
        (`Expected
            ( "terms to have matching plicity"
            , Printf.sprintf "%s\nvs.\n%s" (Term.show_ast term) (Term.show_value expected)
            ))
  | `Lam (plicity, x, body), `Neutral (NMeta _ as m) ->
    let dom = `Neutral (NMeta (Meta.fresh @@ Env.level ())) in
    let cod = `Neutral (NMeta (Meta.fresh @@ Env.level ())) in
    Solve.Constraints.(tell @@ Equals (`Neutral m, `Pi (plicity, x, dom, Fun.const cod)));
    let* body' = Env.fresh_var x dom (fun _ -> check body cod) in
    Ok (`Ann (`Lam (plicity, x, body'), quote 0 expected))
  | `Let (ident, ty, value, body), expected ->
    let* vty =
      match ty with
      | Some t ->
        let* _ = check t `Type in
        Ok (eval t)
      | None -> Ok (`Neutral (NMeta (Meta.fresh @@ Env.level ())))
    in
    let* value = check value vty in
    let value' = eval value in
    let* body =
      Env.local ~f:(Env.with_binding ident ~value:value' ~typ:vty) (fun () ->
        check body expected)
    in
    Ok (`Let (ident, Some (quote 0 vty), value, body))
  | `Pos (pos, term), expected ->
    Env.local ~f:(Env.with_pos pos) (fun () ->
      let* term = check term expected in
      Ok (`Pos (pos, term)))
  | `Ann (expression, typ), expected ->
    let* _ = check typ `Type in
    let typ' = eval typ in
    Solve.Constraints.(tell @@ Equals (typ', expected));
    let* x = check expression typ' in
    Ok (`Ann (x, typ))
  | term, expected ->
    let* inferred, term' = infer term in
    Solve.Constraints.(tell @@ Equals (inferred, expected));
    Ok term'

and row_extract : Ident.t -> Term.value Ident.Map.t -> (Term.value, CalyxError.t) result =
  fun field fields ->
  Map.find fields field
  |> Result.of_option
       ~error:
         (`NoField
             ( field
             , List.map ~f:(fun (label, field) -> label, Term.show_value field)
               @@ Map.to_alist fields ))
;;

let infer_toplevel
  : ast declaration list -> (ast declaration list, CalyxError.t list) result
  =
  fun program ->
  let rec go
    :  Term.ast Term.declaration list
    -> (Term.ast Term.declaration list, CalyxError.t list) result
    = function
    | Function { ident; typ; body; position } :: rest ->
      let vty = eval typ in
      (* Printf.printf *)
      (*   "infer_toplevel.Function { ident = %s; typ = %s; body = %s }\n" *)
      (*   (Intern.lookup ident) *)
      (*   (Term.show_value vty) *)
      (*   (Term.show_ast body); *)
      let placeholder = `Neutral (NVar (Env.level (), ident)) in
      Env.local ~f:(Env.with_binding ident ~value:placeholder ~typ:vty) (fun () ->
        let* body = Result.map_error ~f:Core.List.singleton @@ check body vty in
        let typ = quote 0 vty in
        Result.map ~f:(List.cons (Function { ident; typ; body; position })) @@ go rest)
    | Constant { ident; typ; body; position } :: rest ->
      let* typ, value =
        Result.map_error
          ~f:Core.List.singleton
          (let vty = eval typ in
           let* body_ast = check body vty in
           Ok (vty, body_ast))
      in
      let value = eval value in
      Env.local ~f:(Env.with_binding ident ~typ ~value) (fun () ->
        let typ = quote 0 typ in
        Result.map ~f:(List.cons (Constant { ident; typ; body; position })) @@ go rest)
    | RecordDecl { ident; params = _; fields; position } :: rest ->
      (* Evaluate field types and construct the record type *)
      let field_types = Map.map fields ~f:eval in
      let record_type : Term.value = `RecordType { fields = field_types; tail = None } in
      (* Bind the record type name to the record type value *)
      Env.local ~f:(Env.with_binding ident ~value:record_type ~typ:`Type) (fun () ->
        Result.map
          ~f:
            (List.cons (RecordDecl { ident; params = Ident.Map.empty; fields; position }))
        @@ go rest)
    (* Sum Types *)
    | SumDecl { ident; params; constructors; position } :: rest ->
      (* Evaluate parameters to get their kinds *)
      let param_list = Map.to_alist ~key_order:`Increasing params in
      let eval_params = List.map param_list ~f:(fun (name, kind) -> name, eval kind) in
      (* Evaluate constructor arg types - recursive refs become Neutral(NVar(_, ident))
         which pattern_bindings resolves via environment lookup *)
      let ctor_types = Map.map constructors ~f:(List.map ~f:eval) in
      let sum_type =
        `SumType
          { ident
          ; params = Ident.Map.of_alist_exn eval_params
          ; constructors = ctor_types
          ; position
          }
      in
      (* Build constructor type using proper HOAS - fields are ASTs, evaluated inside closures *)
      let build_ctor_type (fields : Term.ast list) : Term.value =
        let rec build param_vars remaining_params =
          match remaining_params with
          | [] ->
            (* All params bound in env - now evaluate fields *)
            let eval_fields = List.map fields ~f:eval in
            let applied =
              List.fold_left param_vars ~init:sum_type ~f:(fun acc var -> app acc var)
            in
            List.fold_right eval_fields ~init:applied ~f:(fun field acc ->
              `Pi (Explicit, Ident.Intern.underscore, field, Fun.const acc))
          | (param_name, kind) :: rest_params ->
            let snapshot = Env.ask () in
            `Pi
              ( Implicit
              , param_name
              , kind
              , fun var ->
                  Env.with_snapshot
                    (Env.with_binding param_name ~value:var ~typ:kind snapshot)
                    (fun () -> build (param_vars @ [ var ]) rest_params) )
        in
        build [] eval_params
      in
      (* Build constructor bindings with sum_type in scope for recursive references *)
      let constructor_bindings =
        Env.local ~f:(Env.with_binding ident ~value:sum_type ~typ:`Type) (fun () ->
          Map.to_alist ~key_order:`Increasing constructors
          |> List.map ~f:(fun (ctor, fields) ->
            let ctor_pi = build_ctor_type fields in
            ctor, `Opaque, ctor_pi))
      in
      let type_binding = ident, sum_type, `Type in
      Env.local
        ~f:(fun env ->
          List.fold_right
            (type_binding :: constructor_bindings)
            ~init:env
            ~f:(fun (ident, value, typ) -> Env.with_binding ident ~value ~typ))
        (fun () ->
           Result.map
             ~f:(List.cons (SumDecl { ident; params; constructors; position }))
             (go rest))
    | [] -> Ok []
  in
  let program, constraints = Solve.Constraints.handle (fun () -> go program) in
  match Solve.solve constraints with
  | Ok () -> program
  | Error es ->
    print_endline (Solve.pretty_solver_error es);
    raise (Failure "Failed to type program")
;;
