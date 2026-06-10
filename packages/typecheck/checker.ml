open Core
open Term
module Intern = Ident.Intern

let rec eval : Term.t -> Term.value =
  fun tm ->
  Context.trace Trace.Eval tm [%here]
  @@ fun () ->
  match tm with
  | `Var name ->
    (match Context.lookup_value name with
     | Some `Opaque -> `Neutral (NVar (0, name))
     | Some other -> other
     | None -> `Neutral (NVar (0, name)))
  | `Ann (e, _) -> eval e
  | `Type -> `Type
  | `Pi { plicity; ident; dom; cod } ->
    let dom_val = eval dom in
    (* Capture current environment for lexical scoping *)
    let cod =
      Context.close ~f:(fun value ->
        Context.with_binding ident ~value ~typ:dom_val (fun () -> eval cod))
    in
    `Pi (plicity, ident, dom_val, cod)
  | `Lam (plicity, x, body) ->
    let body =
      Context.close ~f:(fun value -> Context.with_binding x ~value (fun () -> eval body))
    in
    `Lam (plicity, x, body)
  | `App (f, x) ->
    let f = eval f in
    let x = eval x in
    app f x
  | `Infix { left; op; right } ->
    let op_val = eval op in
    let left_val = eval left in
    let right_val = eval right in
    app (app op_val left_val) right_val
  | `Let (ident, Some ty, value, body) ->
    let typ = eval ty in
    let value = eval value in
    Context.with_binding ident ~value ~typ (fun () -> eval body)
  | `Let (ident, None, value, body) ->
    let value = eval value in
    Context.with_binding ident ~value (fun () -> eval body)
  | `Match (scrut, arms) ->
    let scrut = eval scrut in
    let arms = List.map arms ~f:(fun (pat, body) -> pattern pat, eval body) in
    `Match (scrut, arms)
  | `Pos (pos, exp) -> Context.with_pos pos (fun () -> eval exp)
  | `Lit lit ->
    (match lit with
     | Record fields -> `Lit (Record (Map.map ~f:eval fields))
     | Int n -> `Lit (Int n)
     | UInt n -> `Lit (UInt n)
     | Float x -> `Lit (Float x)
     | Bool b -> `Lit (Bool b))
  | `Meta m -> `Neutral (NMeta m)
  | `Err e -> `Err e
  | `RecordType { fields; tail } ->
    let fields = Map.map ~f:eval fields in
    let tail = Option.map ~f:eval tail in
    (`RecordType { fields; tail } : Term.value)
  | `SumType { ident; params; constructors; position } ->
    let params = Map.map ~f:eval params in
    let constructors = Map.map ~f:(List.map ~f:eval) constructors in
    `SumType { ident; params; constructors; position }
  | `Proj (term, field) -> proj field (eval term)

and app : value -> value -> value =
  fun f x ->
  match f with
  | `Lam (_, _, body) -> Context.lift_r (body x : (value, Calyx_error.t) result)
  | `Neutral n -> `Neutral (NApp (n, x))
  | `SumType { ident; params; constructors; position } ->
    (* Apply a sum type to a type argument
       For example, Option (a : Type) becomes Type -> Option
       This is probably not how we want to do this in the longterm but it works right now
    *)
    (match Map.to_alist ~key_order:`Increasing params with
     | [] -> assert false
     | (param, _) :: params ->
       let subst_value v =
         match v with
         | `Neutral (NVar (_, name)) when Ident.equal name param -> x
         | other -> other
       in
       let constructors = Map.map constructors ~f:(List.map ~f:subst_value) in
       let params = Ident.Map.of_alist_exn params in
       `SumType { ident; params; constructors; position })
  | other ->
    Context.fail @@ `Expected ("Function", Sexp.to_string_hum @@ Term.sexp_of_value other)

and proj (field : Ident.t) : Term.value -> Term.value = function
  | `Lit (Record fields) -> Map.find_exn fields field
  | `Neutral n -> `Neutral (NProj (n, field))
  | other ->
    Context.fail (`Expected ("record", Sexp.to_string_hum @@ Term.sexp_of_value other))

and pattern : Term.t Term.pattern -> Term.value Term.pattern = function
  | PVar x -> PVar x
  | PWild -> PWild
  | PCtor (ctor, args) -> PCtor (ctor, List.map ~f:pattern args)
  | PLit lit ->
    (match lit with
     | Record fields -> PLit (Record (Map.map ~f:eval fields))
     | Int n -> PLit (Int n)
     | UInt n -> PLit (UInt n)
     | Float x -> PLit (Float x)
     | Bool b -> PLit (Bool b))
  | PRec fields -> PRec (List.map fields ~f:(fun (ident, pat) -> ident, pattern pat))

(* Resolve a type to a SumType, looking up neutral variables in the environment *)
and resolve_sum_type : Term.value -> Term.value Term.sum_type option = function
  | `SumType s -> Some s
  | `Neutral (NVar (_, name)) ->
    (* Look up the name in the environment to find the actual SumType *)
    (match Context.lookup_value name with
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
  (* TODO: Record pattern bindings (also appear inside of [literal]) *)
  | PLit _ -> []
  | PRec _ -> []
  | PCtor (ctor_name, args) ->
    (* Extract constructor argument types from the scrutinee's sum type *)
    (match resolve_sum_type scrut_ty with
     | Some { constructors; _ } ->
       (* If this isn't found in the [constructors] map then something has really gone wrong *)
       Map.find_exn constructors ctor_name
       |> List.zip_exn args
       |> List.concat_map ~f:(Tuple2.uncurry pattern_bindings)
     | None -> [])
;;

let rec quote : int -> Term.value -> Term.t =
  fun lvl tm ->
  Context.trace Trace.Quote tm [%here]
  @@ fun () ->
  match tm with
  | `App (f, x) -> `App (quote lvl f, quote lvl x)
  | `Neutral n -> quote_neutral lvl n
  | `Type -> `Type
  | `Pi (plicity, ident, dom, cod) ->
    (* level shouldn't matter here because [var] is just used to access the body of [cod] *)
    let var = `Neutral (NVar (lvl, ident)) in
    let dom = quote lvl dom in
    let cod = quote (succ lvl) (Context.lift_r (cod var)) in
    `Pi { plicity; ident; dom; cod }
  | `Lam (plicity, x, b) ->
    let var = `Neutral (NVar (0, x)) in
    let body = quote (succ lvl) (Context.lift_r (b var)) in
    `Lam (plicity, x, body)
  | `Lit lit ->
    (match lit with
     | Record fields -> `Lit (Record (Map.map ~f:(quote lvl) fields))
     | Int n -> `Lit (Int n)
     | UInt n -> `Lit (UInt n)
     | Float x -> `Lit (Float x)
     | Bool b -> `Lit (Bool b))
  | `RecordType { fields; tail } ->
    let fields = Map.map ~f:(quote lvl) fields in
    let tail = Option.map ~f:(quote lvl) tail in
    (`RecordType { fields; tail } : Term.t)
  | `SumType { ident; params; constructors; position } ->
    let params = Map.map ~f:(quote lvl) params in
    let constructors = Map.map ~f:(List.map ~f:(quote lvl)) constructors in
    `SumType { ident; params; constructors; position }
  | `Err e -> `Err e
  | `Opaque -> failwith "Cannot quote opaque values, they should not appear here"
  | `Infix { left; op; right } ->
    let left = quote lvl left in
    let op = quote lvl op in
    let right = quote lvl right in
    `Infix { left; op; right }
  | `Var i -> `Var i
  | `Match (scrut, arms) ->
    let scrut = quote lvl scrut in
    let arms =
      List.map arms ~f:(fun (ptn, arm) -> quote_pattern lvl ptn, quote lvl arm)
    in
    `Match (scrut, arms)
  | `Ann (tm, typ) -> `Ann (quote lvl tm, quote lvl typ)
  | `Proj (tm, field) -> `Proj (quote lvl tm, field)

and quote_pattern (lvl : int) : Term.value Term.pattern -> Term.t Term.pattern = function
  | PLit (Record fields) -> PLit (Record (Map.map ~f:(quote lvl) fields))
  | PLit lit ->
    PLit
      (match lit with
       | Int n -> Int n
       | UInt n -> UInt n
       | Float x -> Float x
       | Bool b -> Bool b
       | _ -> assert false)
  | PWild -> PWild
  | PCtor (ident, args) -> PCtor (ident, List.map ~f:(quote_pattern lvl) args)
  | PVar ident -> PVar ident
  | PRec _ -> Context.fail `Todo

and quote_neutral (lvl : int) : neutral -> Term.t = function
  | NVar (_, x) -> `Var x
  | NApp (f, x) -> `App (quote_neutral lvl f, quote lvl x)
  | NMeta m -> `Meta m
  | NProj (term, field) -> `Proj (quote_neutral lvl term, field)
;;

let rec infer : Term.t -> Term.value * Term.t =
  fun tm ->
  Context.trace Trace.Infer tm [%here]
  @@ fun () ->
  match tm with
  | `Var i ->
    let ty = Context.lookup_type i |> Context.lift_o_or_fail ~error:(`NotFound i) in
    (* Create and insert metas for implicit params
       Something about this being in the 'Var' case feels gross, doing this
       sort of elaboration here feels wrong
    *)
    let rec insert_var_implicits (ty : Term.value) (term : Term.t) : Term.value * Term.t =
      match Solve.force ty with
      | `Pi (Implicit, _ident, dom, cod) ->
        let meta = Context.fresh_meta () in
        let meta_val = `Neutral (NMeta meta) in
        let dom = quote 0 dom in
        let new_term = `App (term, `Ann (`Meta meta, dom)) in
        let cod = Context.lift_r (cod meta_val) in
        insert_var_implicits cod new_term
      | _ -> ty, term
    in
    let result_ty, annotated_term = insert_var_implicits ty (`Var i) in
    let typ = quote 0 result_ty in
    result_ty, `Ann (annotated_term, typ)
  | `Pi { plicity; ident; dom; cod } ->
    let dom_val = eval (check dom `Type) in
    let (_ : Term.t) =
      Context.with_var ident ~typ:dom_val ~f:(fun _var -> check cod `Type)
    in
    `Type, `Pi { plicity; ident; dom; cod }
  | `Lam (plicity, x, body) ->
    let meta = Context.fresh_meta () in
    let dom = `Neutral (NMeta meta) in
    let body_ty, body = Context.with_var x ~typ:dom ~f:(fun _ -> infer body) in
    let ty = `Pi (plicity, x, dom, fun _ -> Ok body_ty) in
    let quoted = quote 0 ty in
    ty, `Ann (`Lam (plicity, x, body), quoted)
  | `App (f, x) ->
    let tf, f = infer f in
    let rec insert_implicits tf f =
      match Solve.force tf with
      | `Pi (Implicit, _ident, dom, cod) ->
        let meta = Context.fresh_meta () in
        let meta_val = `Neutral (NMeta meta) in
        let quoted_dom = quote 0 dom in
        let new_f = `App (f, `Ann (`Meta meta, quoted_dom)) in
        let cod = Context.lift_r (cod meta_val) in
        insert_implicits cod new_f
      | `Pi (Explicit, _, dom, cod) ->
        let x' = check x dom in
        let result_ty = Context.lift_r (cod (`Neutral (NVar (0, Intern.underscore)))) in
        let quoted_ty = quote 0 result_ty in
        result_ty, `Ann (`App (f, x'), quoted_ty)
      | `Neutral (NMeta _) as tf ->
        let dom = `Neutral (NMeta (Context.fresh_meta ())) in
        let cod = `Neutral (NMeta (Context.fresh_meta ())) in
        Context.tell_constraint
          (Constraint.equals tf (`Pi (Explicit, Intern.underscore, dom, fun _ -> Ok cod)));
        let x' = check x dom in
        let quoted_cod = quote 0 cod in
        cod, `Ann (`App (f, x'), quoted_cod)
      | otherwise -> Context.fail (`Expected ("function", Term.show_value otherwise))
    in
    insert_implicits tf f
  | `Infix { left; op; right } ->
    (* Treat infix like nested application *)
    let app_expr = `App (`App (op, left), right) in
    let ty, checked_app = infer app_expr in
    (match checked_app with
     | `Ann (`App (`App (op, left), right), result_ty) ->
       let infix_expr = `Infix { left; op; right } in
       ty, `Ann (infix_expr, result_ty)
     | _ ->
       let _, left = infer left in
       let _, op = infer op in
       let _, right = infer right in
       ty, `Infix { left; op; right })
  | `Let (ident, typ, value, body) ->
    let typ =
      match typ with
      | Some t ->
        let (_ : Term.t) = check t `Type in
        eval t
      | None -> `Neutral (NMeta (Context.fresh_meta ()))
    in
    let value' = check value typ in
    let value'' = eval value' in
    let body_ty, body =
      Context.with_binding ident ~value:value'' ~typ (fun () -> infer body)
    in
    let typ = quote 0 typ in
    let body_typ = quote 0 body_ty in
    body_ty, `Ann (`Let (ident, Some typ, value', body), body_typ)
  | `Ann (e, a) ->
    let (_ : Term.t) = check a `Type in
    let vt = eval a in
    let e = check e vt in
    vt, `Ann (e, a)
  | `Type -> `Type, `Type
  | `Proj (term, field) -> infer_proj term field
  | `Pos (p, term) -> Context.with_pos p (fun () -> infer term)
  | `Match (scrut, arms) ->
    let scrut_ty, scrut = infer scrut in
    let infer_arm (pat, body) =
      let val_pat = pattern pat in
      let bindings = pattern_bindings val_pat scrut_ty in
      let level = Context.level () in
      (* Introduce pattern bindings into arm's environment *)
      let entries =
        List.map bindings ~f:(fun (ident, typ) ->
          ident, Context.Typed (`Neutral (NVar (level, ident)), typ))
      in
      Context.with_bindings entries (fun () ->
        let body_ty, body = infer body in
        let body_ty' = quote 0 body_ty in
        (pat, `Ann (body, body_ty')), body_ty)
    in
    let arms_and_types = List.map ~f:infer_arm arms in
    let annotated_arms = List.map ~f:fst arms_and_types in
    let arm_types = List.map ~f:snd arms_and_types in
    (match arm_types with
     | [] ->
       Context.fail (`Expected ("non-empty match", Term.show (`Match (scrut, arms))))
     | first_ty :: rest_types ->
       List.iter rest_types ~f:(fun ty ->
         Context.tell_constraint (Constraint.equals first_ty ty));
       let scrut_typ = quote 0 scrut_ty in
       let first_typ = quote 0 first_ty in
       ( first_ty
       , (`Ann (`Match (`Ann (scrut, scrut_typ), annotated_arms), first_typ) : Term.t) ))
  | `Lit lit ->
    let ty, lit' = infer_lit lit in
    let ty' = quote 0 ty in
    ty, `Ann (`Lit lit', ty')
  | `Meta m -> `Neutral (NMeta m), `Meta m
  | `Err e -> `Err e, `Err e
  | `RecordType { fields; tail } ->
    let record_val : Term.value =
      let fields = Map.map ~f:eval fields in
      let tail = Option.map ~f:eval tail in
      `RecordType { fields; tail }
    in
    let record_val' = quote 0 record_val in
    `Type, `Ann ((`RecordType { fields; tail } : Term.t), record_val')
  | `SumType { ident; params; constructors; position } ->
    (* Sum types have type Type *)
    let sum_val =
      let params = Map.map ~f:eval params in
      let constructors = Map.map ~f:(List.map ~f:eval) constructors in
      quote 0 @@ `SumType { ident; params; constructors; position }
    in
    `Type, `Ann (`SumType { ident; params; constructors; position }, sum_val)

and infer_lit : Term.t Term.literal -> Term.value * Term.t Term.literal = function
  | Int n -> `Neutral (NVar (0, Intern.intern "Int")), Int n
  | UInt n -> `Neutral (NVar (0, Intern.intern "UInt")), UInt n
  | Float x -> `Neutral (NVar (0, Intern.intern "Float")), Float x
  | Bool b -> `Neutral (NVar (0, Intern.intern "Bool")), Bool b
  | Record fields ->
    let fields = Map.map fields ~f:infer in
    let field_values = Map.map ~f:snd fields in
    let field_types = Map.map ~f:fst fields in
    let record_type : Term.value = `RecordType { fields = field_types; tail = None } in
    record_type, Record field_values

(** Infer the type of [field] in [term] *)
and infer_proj : Term.t -> Ident.t -> Term.value * Term.t =
  fun term field ->
  let typ, annotated = infer term in
  match typ with
  | `RecordType row ->
    (match Map.find row.fields field with
     | Some field_type -> field_type, `Proj (annotated, field)
     | None ->
       (match row.tail with
        | Some tail ->
          let field_type = `Neutral (NMeta (Context.fresh_meta ())) in
          Context.tell_constraint
            (Constraint.has_field ~record:tail ~field_name:field ~field_type);
          field_type, `Proj (annotated, field)
        | None ->
          Context.fail
            (`NoField (field, Map.to_alist @@ Map.map ~f:Term.show_value row.fields))))
  | `Neutral n ->
    let field_type = `Neutral (NMeta (Context.fresh_meta ())) in
    let partial : Term.value =
      let fields : Term.value Ident.Map.t = Ident.Map.singleton field field_type in
      let tail : Term.value option = Some (`Neutral (NMeta (Context.fresh_meta ()))) in
      `RecordType ({ fields; tail } : Term.value Term.row)
    in
    Context.tell_constraint (Constraint.equals (`Neutral n) partial);
    field_type, `Proj (annotated, field)
  | other -> Context.fail (`Expected ("Record", Term.show_value other))

and check : Term.t -> Term.value -> Term.t =
  fun term expected ->
  Context.trace (Trace.Check expected) term [%here]
  @@ fun () ->
  match term, expected with
  | `Lam (plicity, x, body), `Pi (plicity', _, dom, cod) ->
    if Term.equal_plicity plicity plicity'
    then (
      let body' =
        Context.with_var x ~typ:dom ~f:(fun var -> check body (Context.lift_r (cod var)))
      in
      let expected = quote 0 expected in
      `Ann (`Lam (plicity, x, body'), expected))
    else
      Context.fail
        (`Expected
            ( "terms to have matching plicity"
            , Printf.sprintf "%s\nvs.\n%s" (Term.show term) (Term.show_value expected) ))
  | `Lam (plicity, x, body), `Neutral (NMeta _ as m) ->
    let dom = `Neutral (NMeta (Context.fresh_meta ())) in
    let cod = `Neutral (NMeta (Context.fresh_meta ())) in
    Context.tell_constraint
      (Constraint.equals (`Neutral m) (`Pi (plicity, x, dom, Fun.const (Ok cod))));
    let body' = Context.with_var x ~typ:dom ~f:(fun _ -> check body cod) in
    let expected = quote 0 expected in
    `Ann (`Lam (plicity, x, body'), expected)
  | `Let (ident, ty, value, body), expected ->
    let vty =
      match ty with
      | Some t ->
        let (_ : Term.t) = check t `Type in
        eval t
      | None -> `Neutral (NMeta (Context.fresh_meta ()))
    in
    let value = check value vty in
    let value' = eval value in
    let body =
      Context.with_binding ident ~value:value' ~typ:vty (fun () -> check body expected)
    in
    let vty = quote 0 vty in
    `Let (ident, Some vty, value, body)
  | `Pos (pos, term), expected ->
    Context.with_pos pos (fun () ->
      let term = check term expected in
      `Pos (pos, term))
  | `Ann (expression, typ), expected ->
    let (_ : Term.t) = check typ `Type in
    let typ' = eval typ in
    Context.tell_constraint (Constraint.equals typ' expected);
    let x = check expression typ' in
    `Ann (x, typ)
  | term, expected ->
    let inferred, term' = infer term in
    Context.tell_constraint (Constraint.equals inferred expected);
    term'
;;

let infer_toplevel : Term.t Term.declaration list -> Term.t Term.declaration list =
  fun program ->
  let rec go = function
    | Function { ident; typ; body; position } :: rest ->
      let vty = eval typ in
      let level = Context.level () in
      let placeholder = `Neutral (NVar (level, ident)) in
      Context.with_binding ident ~value:placeholder ~typ:vty (fun () ->
        let body = check body vty in
        let typ = quote 0 vty in
        (Function { ident; typ; body; position } : Term.t Term.declaration) :: go rest)
    | Constant { ident; typ; body; position } :: rest ->
      let typ, value =
        let vty = eval typ in
        let body_ast = check body vty in
        vty, body_ast
      in
      let value = eval value in
      Context.with_binding ident ~typ ~value (fun () ->
        let typ = quote 0 typ in
        Constant { ident; typ; body; position } :: go rest)
    | RecordDecl { ident; params = _; fields; position } :: rest ->
      (* Evaluate field types and construct the record type *)
      let field_types = Map.map fields ~f:eval in
      let record_type : Term.value = `RecordType { fields = field_types; tail = None } in
      (* Bind the record type name to the record type value *)
      Context.with_binding ident ~value:record_type ~typ:`Type (fun () ->
        RecordDecl { ident; params = Ident.Map.empty; fields; position } :: go rest)
    (* Sum Types *)
    | SumDecl { ident; params; constructors; position } :: rest ->
      let eval_params = Map.map ~f:eval params in
      (* Evaluate constructor arg types - recursive refs become Neutral(NVar(_, ident))
         which pattern_bindings resolves via environment lookup *)
      let eval_constructors = Map.map constructors ~f:(List.map ~f:eval) in
      let sum_type =
        `SumType
          { ident; params = eval_params; constructors = eval_constructors; position }
      in
      (* Build constructor type - fields are ASTs, evaluated inside closures *)
      let build_ctor_type (fields : Term.t list) : Term.value =
        let rec build param_vars remaining_params =
          match remaining_params with
          | [] ->
            let eval_fields = List.map fields ~f:eval in
            let applied = List.fold param_vars ~init:sum_type ~f:app in
            List.fold eval_fields ~init:applied ~f:(fun acc field ->
              `Pi (Explicit, Ident.Intern.underscore, field, Fun.const (Ok acc)))
          | (param_name, kind) :: rest_params ->
            let cod =
              Context.close ~f:(fun value ->
                Context.with_binding param_name ~value ~typ:kind (fun () ->
                  build (param_vars @ [ value ]) rest_params))
            in
            `Pi (Implicit, param_name, kind, cod)
        in
        build [] @@ Map.to_alist eval_params
      in
      (* Build constructor bindings with sum_type in scope for recursive references *)
      let constructor_bindings =
        Context.with_binding ident ~value:sum_type ~typ:`Type (fun () ->
          Map.to_alist ~key_order:`Increasing constructors
          |> List.map ~f:(fun (ctor, fields) ->
            let ctor_pi = build_ctor_type fields in
            ctor, Context.Typed (`Opaque, ctor_pi)))
      in
      let type_binding = ident, Context.Typed (sum_type, `Type) in
      Context.with_bindings (type_binding :: constructor_bindings) (fun () ->
        SumDecl { ident; params; constructors; position } :: go rest)
    | [] -> []
  in
  go program
;;
