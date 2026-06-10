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
  | `Self (x, body) ->
    let body =
      Context.close ~f:(fun value -> Context.with_binding x ~value (fun () -> eval body))
    in
    `Self (x, body)
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
  | `Proj (term, field) -> proj field (eval term)

and app : value -> value -> value =
  fun f x ->
  match f with
  | `Lam (_, _, body) -> Context.lift_r (body x : (value, Calyx_error.t) result)
  | `Neutral n -> `Neutral (NApp (n, x))
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
  | PCtor _ -> Context.fail (`Unsupported "constructor patterns")
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
  | `Self (x, b) ->
    let var = `Neutral (NVar (lvl, x)) in
    let body = quote (succ lvl) (Context.lift_r (b var)) in
    `Self (x, body)
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
  | `Self (x, body) ->
    (* Formation: Γ, x : ($x. T) ⊢ T : Type  ⟹  Γ ⊢ $x. T : Type *)
    let self_ty = eval (`Self (x, body)) in
    let (_ : Term.t) =
      Context.with_var x ~typ:self_ty ~f:(fun _var -> check body `Type)
    in
    `Type, `Self (x, body)
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
        (* Dependent application: the result type is the codomain at the
           actual argument, computed by NbE. *)
        let result_ty = Context.lift_r (cod (eval x')) in
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
    let is_ctor_pat (pat, _) =
      match pat with
      | PCtor _ -> true
      | _ -> false
    in
    if List.exists arms ~f:is_ctor_pat
    then infer_ctor_match scrut_ty scrut arms
    else (
      (* No constructor arms: var/wildcard/literal patterns only. This path
         also covers desugared [if] (PVar True / PVar False arms), whose
         output shape ir.ml re-fuses; arm patterns must stay untouched. *)
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
      match arm_types with
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

(* Typing for matches with constructor arms, as the constant-motive
   specialization of Scott elimination. For scrutinee [s : D v̄] and result
   type [R], unfolding [D v̄] and instantiating at [s] with motive [λ_. R]
   gives

     s : (F̄_0[p̄ := v̄] -> R) -> ... -> (F̄_m[p̄ := v̄] -> R) -> R

   so arm [C_i x̄ -> e_i] is well-typed iff [e_i : R] under
   [x̄ : F̄_i[p̄ := v̄]] — computed by NbE-applying the constructor's Pi
   closures to v̄, never by substitution. The elimination spine itself is not
   reified: the Match node stays in the elaborated AST for the IR's
   tag-dispatch path. *)
and infer_ctor_match
  : Term.value -> Term.t -> (Term.t Term.pattern * Term.t) list -> Term.value * Term.t
  =
  fun scrut_ty scrut arms ->
  let data =
    let first_ctor =
      List.find_map arms ~f:(fun (pat, _) ->
        match pat with
        | PCtor (c, _) -> Some c
        | _ -> None)
    in
    match first_ctor with
    | None -> assert false (* caller guarantees at least one PCtor arm *)
    | Some c ->
      let info =
        Context.lookup_ctor c |> Context.lift_o_or_fail ~error:(`UnknownConstructor c)
      in
      Context.lookup_data info.Context.datatype
      |> Context.lift_o_or_fail ~error:(`UnknownConstructor c)
  in
  (* Datatype parameter instances: decompose a nominal scrutinee-type spine
     directly, otherwise constrain fresh metas. *)
  let param_args : Term.value list =
    let forced = Solve.force scrut_ty in
    let nominal_spine =
      match forced with
      | `Neutral n ->
        (match Solve.spine n with
         | Some (head, args) when Ident.equal head data.Context.data_name -> Some args
         | _ -> None)
      | _ -> None
    in
    match nominal_spine with
    | Some args -> args
    | None ->
      let metas =
        List.init data.Context.param_arity ~f:(fun _ ->
          (`Neutral (NMeta (Context.fresh_meta ())) : Term.value))
      in
      let applied =
        List.fold
          metas
          ~init:(NVar (0, data.Context.data_name))
          ~f:(fun acc m -> NApp (acc, m))
      in
      Context.tell_constraint (Constraint.equals forced (`Neutral applied));
      metas
  in
  let infer_arm (pat, body) =
    let finish pat body =
      let body_ty, body' = infer body in
      (pat, (`Ann (body', quote 0 body_ty) : Term.t)), body_ty
    in
    match pat with
    | PCtor (c, ps) ->
      let info =
        Context.lookup_ctor c |> Context.lift_o_or_fail ~error:(`UnknownConstructor c)
      in
      if not (Ident.equal info.Context.datatype data.Context.data_name)
      then Context.fail (`ConstructorMismatch (c, data.Context.data_name));
      if List.length ps <> info.Context.arity
      then Context.fail (`CtorArity (c, info.Context.arity, List.length ps));
      let rec instantiate ty = function
        | [] -> ty
        | v :: rest ->
          (match Solve.force ty with
           | `Pi (Implicit, _, _, cod) -> instantiate (Context.lift_r (cod v)) rest
           | other ->
             Context.fail
               (`Expected
                   ("constructor type with implicit parameters", Term.show_value other)))
      in
      let rec bind_fields ty = function
        | [] -> finish pat body
        | p :: rest ->
          (match Solve.force ty with
           | `Pi (Explicit, _, dom, cod) ->
             let bind_ident =
               match p with
               | PVar x -> x
               | PWild -> Intern.underscore
               | _ -> Context.fail (`Unsupported "nested constructor patterns")
             in
             Context.with_var bind_ident ~typ:dom ~f:(fun var ->
               bind_fields (Context.lift_r (cod var)) rest)
           | other ->
             Context.fail (`Expected ("constructor field type", Term.show_value other)))
      in
      bind_fields (instantiate info.Context.ctor_type param_args) ps
    | PVar x -> Context.with_var x ~typ:scrut_ty ~f:(fun _ -> finish pat body)
    | PWild -> finish pat body
    | PLit _ | PRec _ ->
      Context.fail (`Unsupported "literal or record patterns on datatype scrutinees")
  in
  let arms_and_types = List.map ~f:infer_arm arms in
  let has_catch_all =
    List.exists arms ~f:(fun (pat, _) ->
      match pat with
      | PVar _ | PWild -> true
      | _ -> false)
  in
  if not has_catch_all
  then (
    let matched =
      List.filter_map arms ~f:(fun (pat, _) ->
        match pat with
        | PCtor (c, _) -> Some c
        | _ -> None)
    in
    let missing =
      List.filter_map data.Context.ctors ~f:(fun ci ->
        if List.mem matched ci.Context.ctor_name ~equal:Ident.equal
        then None
        else Some ci.Context.ctor_name)
    in
    if not (List.is_empty missing) then Context.tell_error (`NonExhaustiveMatch missing));
  match arms_and_types with
  | [] -> Context.fail (`Expected ("non-empty match", Term.show (`Match (scrut, arms))))
  | (first_arm, first_ty) :: rest ->
    List.iter rest ~f:(fun (_, ty) ->
      Context.tell_constraint (Constraint.equals first_ty ty));
    let annotated_arms = first_arm :: List.map ~f:fst rest in
    let scrut_typ = quote 0 scrut_ty in
    let first_typ = quote 0 first_ty in
    first_ty, `Ann (`Match (`Ann (scrut, scrut_typ), annotated_arms), first_typ)

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
  | (`Lam _ as term), `Neutral n when Option.is_some (Solve.unfold n) ->
    (* A lambda checked against a nominal datatype head: unfold one step so
       the Self-intro rule below can fire. Only encoding validation hits
       this; elaborated user code never checks lambdas against datatypes. *)
    check term (Option.value_exn (Solve.unfold n))
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
  | term, `Self (_, closure) ->
    (* Intro (SelfGen): Γ ⊢ t ⇐ T[x := t]  ⟹  Γ ⊢ t ⇐ $x. T.
       The "substitution" is NbE application of the HOAS closure. *)
    let term_val = eval term in
    check term (Context.lift_r (closure term_val))
  | term, expected ->
    let inferred, term' = infer term in
    Context.tell_constraint (Constraint.equals inferred expected);
    term'
;;

(* Re-check each generated constructor encoding against its nominal Pi type
   (a live test of the Self-intro rule). Settable by tests; enable on the CLI
   with CALYX_VALIDATE_ENCODINGS=1. *)
let validate_encodings : bool ref =
  ref (Option.is_some (Sys.getenv "CALYX_VALIDATE_ENCODINGS"))
;;

(* Elaborate a [data] declaration per design/type_system.md §3: the type and
   each constructor become nominal constants (bound to their own neutrals, so
   conversion treats them as sealed heads), each backed by a self-typed Scott
   encoding registered in the definitions store for delta unfolding.

   Encodings are evaluated only after the type and all constructors are bound
   to neutrals: every recursive reference inside an encoding is then a stuck
   nominal neutral, so nothing diverges at eval or quote time. *)
let elaborate_sum_decl (decl : Term.t Term.sum_type) (k : unit -> 'a) : 'a =
  let ({ ident = data_name; params; constructors; _ } : Term.t Term.sum_type) = decl in
  let pi ?(plicity = Explicit) ident dom cod : Term.t =
    `Pi { plicity; ident; dom; cod }
  in
  let lam ?(plicity = Explicit) ident body : Term.t = `Lam (plicity, ident, body) in
  let app_spine (f : Term.t) (xs : Term.t list) : Term.t =
    List.fold xs ~init:f ~f:(fun acc x -> `App (acc, x))
  in
  let kind_syntax =
    List.fold_right params ~init:(`Type : Term.t) ~f:(fun (p, ty) cod -> pi p ty cod)
  in
  let (_ : Term.t) = check kind_syntax `Type in
  let kind = eval kind_syntax in
  Context.with_binding
    data_name
    ~value:(`Neutral (NVar (0, data_name)))
    ~typ:kind
    (fun () ->
       let applied_data =
         app_spine (`Var data_name) (List.map params ~f:(fun (p, _) -> `Var p))
       in
       let ctor_alist = Map.to_alist ~key_order:`Increasing constructors in
       let ctor_types =
         List.map ctor_alist ~f:(fun (ctor, fields) ->
           let ty_syntax =
             List.fold_right fields ~init:applied_data ~f:(fun field cod ->
               pi Intern.underscore field cod)
             |> fun fields_pi ->
             List.fold_right params ~init:fields_pi ~f:(fun (p, pty) cod ->
               pi ~plicity:Implicit p pty cod)
           in
           let (_ : Term.t) = check ty_syntax `Type in
           ctor, fields, eval ty_syntax)
       in
       let ctor_bindings =
         List.map ctor_types ~f:(fun (ctor, _, ty) ->
           ctor, Context.Typed (`Neutral (NVar (0, ctor)), ty))
       in
       Context.with_bindings ctor_bindings (fun () ->
         (* "$"-prefixed binders cannot collide with user identifiers. *)
         let self_id = Intern.intern "$self"
         and motive_id = Intern.intern "$P" in
         let field_ids fields =
           List.mapi fields ~f:(fun i _ -> Intern.intern (Printf.sprintf "$x%d" i))
         in
         (* K_i = (x1 : F1) -> ... -> (xn : Fn) -> P (C_i p̄ x̄) *)
         let branch (ctor, fields) =
           let xs = field_ids fields in
           let ctor_applied =
             app_spine
               (`Var ctor)
               (List.map params ~f:(fun (p, _) -> `Var p)
                @ List.map xs ~f:(fun x -> `Var x))
           in
           List.fold_right
             (List.zip_exn xs fields)
             ~init:(`App (`Var motive_id, ctor_applied))
             ~f:(fun (x, field) cod -> pi x field cod)
         in
         (* D = λ p̄. $self. {P : D p̄ -> Type} -> K_0 -> ... -> K_m -> P self *)
         let data_encoding_syntax =
           let motive_ty = pi Intern.underscore applied_data `Type in
           let body =
             List.fold_right
               ctor_alist
               ~init:(`App (`Var motive_id, `Var self_id))
               ~f:(fun ctor cod -> pi Intern.underscore (branch ctor) cod)
           in
           let self_body : Term.t =
             `Self (self_id, pi ~plicity:Implicit motive_id motive_ty body)
           in
           List.fold_right params ~init:self_body ~f:(fun (p, _) acc -> lam p acc)
         in
         (* C_i = λ {p̄} x̄. λ {P} k_0 ... k_m. k_i x̄ *)
         let cont_ids =
           List.mapi ctor_alist ~f:(fun i _ -> Intern.intern (Printf.sprintf "$k%d" i))
         in
         let ctor_encoding_syntax tag fields =
           let xs = field_ids fields in
           let body =
             app_spine
               (`Var (List.nth_exn cont_ids tag))
               (List.map xs ~f:(fun x -> `Var x))
           in
           List.fold_right cont_ids ~init:body ~f:(fun kid acc -> lam kid acc)
           |> lam ~plicity:Implicit motive_id
           |> fun acc ->
           List.fold_right xs ~init:acc ~f:(fun x acc -> lam x acc)
           |> fun acc ->
           List.fold_right params ~init:acc ~f:(fun (p, _) acc ->
             lam ~plicity:Implicit p acc)
         in
         let ctor_infos =
           List.mapi ctor_types ~f:(fun tag (ctor, fields, ctor_type) ->
             { Context.ctor_name = ctor
             ; datatype = data_name
             ; tag
             ; arity = List.length fields
             ; ctor_type
             ; encoding = eval (ctor_encoding_syntax tag fields)
             })
         in
         let data_info =
           { Context.data_name
           ; param_arity = List.length params
           ; kind
           ; ctors = ctor_infos
           ; encoding = eval data_encoding_syntax
           }
         in
         Context.define data_name (Context.Data data_info);
         List.iter ctor_infos ~f:(fun info ->
           Context.define info.Context.ctor_name (Context.Ctor info));
         if !validate_encodings
         then
           List.iteri ctor_types ~f:(fun tag (_, fields, ctor_type) ->
             let (_ : Term.t) = check (ctor_encoding_syntax tag fields) ctor_type in
             ());
         k ()))
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
    | SumDecl decl :: rest ->
      (match Positivity.check decl with
       | Error e ->
         Context.tell_error e;
         SumDecl decl :: go rest
       | Ok () -> elaborate_sum_decl decl (fun () -> SumDecl decl :: go rest))
    | (RecordDecl _ as decl) :: rest ->
      Context.tell_error (`Unsupported "record type declarations");
      decl :: go rest
    | [] -> []
  in
  go program
;;

let%test "self type round-trips through eval and quote" =
  let x = Intern.intern "self" in
  let p = Intern.intern "P" in
  let tm : Term.t = `Self (x, `App (`Var p, `Var x)) in
  Context.run (fun () -> quote 0 (eval tm))
  |> fst
  |> function
  | Ok (`Self (x', `App (`Var p', `Var x''))) ->
    Ident.equal x x' && Ident.equal p p' && Ident.equal x x''
  | _ -> false
;;

let%test_module "datatype invariants" =
  (module struct
    (* Infer and solve a whole program against the test stdlib. *)
    let check_program decls =
      Context.run ~bindings:Testgen.stdlib (fun () ->
        let inferred = infer_toplevel decls in
        Solve.solve ();
        inferred)
    ;;

    let clean_run = function
      | Ok _, state -> List.is_empty state.Context.errors
      | Error _, _ -> false
    ;;

    (* Elaborate one generated declaration and probe the context. *)
    let with_adt spec ~f =
      let adt = Testgen.adt_of_spec spec in
      Context.run ~bindings:Testgen.stdlib (fun () -> elaborate_sum_decl adt f)
      |> function
      | Ok b, state -> b && List.is_empty state.Context.errors
      | Error _, _ -> false
    ;;

    let%test_unit "every generated encoding checks against its constructor type" =
      let saved = !validate_encodings in
      validate_encodings := true;
      Exn.protect
        ~finally:(fun () -> validate_encodings := saved)
        ~f:(fun () ->
          QCheck.Test.check_exn
          @@ QCheck.Test.make ~count:150 ~name:"encoding-validation" Testgen.arb_adt_spec
          @@ fun spec -> clean_run (check_program [ SumDecl (Testgen.adt_of_spec spec) ]))
    ;;

    (* The zero-cost core: applying an unfolded constructor application to a
       motive and per-constructor continuations must select exactly the
       continuation at the constructor's tag, passing the fields in order.
       This is the agreement between tagged dispatch and encoding application
       that the backend's [{_tag}] representation relies on. *)
    let%test_unit "scott encoding application implements tagged dispatch" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make ~count:200 ~name:"tag-dispatch" Testgen.arb_adt_with_index
      @@ fun (spec, pick) ->
      with_adt spec ~f:(fun () ->
        let c = Testgen.ctor_name pick in
        let info = Option.value_exn (Context.lookup_ctor c) in
        let data = Option.value_exn (Context.lookup_data Testgen.data_name) in
        let int_v : Term.value = `Neutral (NVar (0, Testgen.int_name)) in
        let args =
          List.init info.Context.arity ~f:(fun i ->
            (`Neutral (NVar (0, Intern.intern (Printf.sprintf "DISPATCH_ARG%d" i)))
             : Term.value))
        in
        let spine =
          List.fold
            (List.init spec.Testgen.n_params ~f:(Fn.const int_v) @ args)
            ~init:(NVar (0, c))
            ~f:(fun acc v -> NApp (acc, v))
        in
        let unfolded = Option.value_exn (Solve.unfold spine) in
        let motive : Term.value = `Neutral (NVar (0, Intern.intern "DISPATCH_MOTIVE")) in
        let cont j : Term.value =
          `Neutral (NVar (0, Intern.intern (Printf.sprintf "DISPATCH_CONT%d" j)))
        in
        let conts = List.init (List.length data.Context.ctors) ~f:cont in
        let result = List.fold conts ~init:(Solve.vapp unfolded motive) ~f:Solve.vapp in
        let expected = List.fold args ~init:(cont info.Context.tag) ~f:Solve.vapp in
        Testgen.term_eq (quote 0 result) (quote 0 expected))
    ;;

    let%test_unit "encodings are stable under quote-eval round-trips" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make ~count:100 ~name:"encoding-round-trip" Testgen.arb_adt_spec
      @@ fun spec ->
      with_adt spec ~f:(fun () ->
        let round v =
          let q = quote 0 v in
          Testgen.term_eq q (quote 0 (eval q))
        in
        let data = Option.value_exn (Context.lookup_data Testgen.data_name) in
        round data.Context.encoding
        && List.for_all data.Context.ctors ~f:(fun ci -> round ci.Context.encoding))
    ;;

    let%test_unit "matches missing constructors are flagged non-exhaustive" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make ~count:150 ~name:"match-coverage" Testgen.arb_adt_with_subset
      @@ fun (spec, mask) ->
      let n = Testgen.n_ctors spec in
      let all = List.init n ~f:Fn.id in
      let idxs = List.filter all ~f:(fun i -> mask land (1 lsl i) <> 0) in
      let expected_missing =
        List.filter all ~f:(fun i -> mask land (1 lsl i) = 0)
        |> List.map ~f:Testgen.ctor_name
      in
      let decl = Term.SumDecl (Testgen.adt_of_spec spec) in
      let names l =
        List.map l ~f:Intern.lookup |> List.sort ~compare:String.compare
      in
      let flagged =
        match check_program [ decl; Testgen.match_fn spec idxs ] with
        | Ok _, state ->
          (match state.Context.errors with
           | [ `NonExhaustiveMatch missing ] ->
             List.equal String.equal (names missing) (names expected_missing)
           | _ -> false)
        | Error _, _ -> false
      in
      let repaired =
        clean_run (check_program [ decl; Testgen.match_fn ~catch_all:true spec idxs ])
      in
      let total = clean_run (check_program [ decl; Testgen.consume_fn spec ]) in
      flagged && repaired && total
    ;;

    let%test "nat-shaped data encoding has the documented self/motive structure" =
      let spec = Testgen.{ n_params = 0; ctor_fields = [ []; [ FRec ] ] } in
      with_adt spec ~f:(fun () ->
        let data = Option.value_exn (Context.lookup_data Testgen.data_name) in
        match quote 0 data.Context.encoding with
        | `Self
            ( self_id
            , `Pi
                { plicity = Implicit
                ; ident = motive
                ; dom = `Pi { dom = `Var d; cod = `Type; _ }
                ; cod
                ; _
                } ) ->
          let rec walk n : Term.t -> bool = function
            | `Pi { plicity = Explicit; cod; _ } -> walk (n + 1) cod
            | `App (`Var p, `Var s) ->
              n = 2 && Ident.equal p motive && Ident.equal s self_id
            | _ -> false
          in
          Ident.equal d Testgen.data_name && walk 0 cod
        | _ -> false)
    ;;

    let%test "structurally identical datatypes stay nominally distinct" =
      let adt name ctor : Term.t Term.sum_type =
        { ident = Intern.intern name
        ; params = []
        ; constructors =
            Ident.Map.of_alist_exn [ Intern.intern ctor, [ (`Var Testgen.int_name : Term.t) ] ]
        ; position = Testgen.no_pos
        }
      in
      let coerce : Term.t Term.declaration =
        let x = Intern.intern "x" in
        Function
          { ident = Intern.intern "coerce"
          ; typ =
              `Pi
                { plicity = Explicit
                ; ident = x
                ; dom = `Var (Intern.intern "SealA")
                ; cod = `Var (Intern.intern "SealB")
                }
          ; body = `Lam (Explicit, x, `Var x)
          ; position = Testgen.no_pos
          }
      in
      let program =
        [ Term.SumDecl (adt "SealA" "MkSealA")
        ; Term.SumDecl (adt "SealB" "MkSealB")
        ; coerce
        ]
      in
      not (clean_run (check_program program))
    ;;

    let%test "datatype parameters keep source order" =
      let source =
        "data PairT (b : Type) (a : Type) where\n\
         | MkPairT b a\n\n\
         def firstT (p : PairT Bool Int) -> Bool do\n\
        \  match p with\n\
        \  | MkPairT x y -> x\n"
      in
      match Parse.run source with
      | Error _ -> false
      | Ok toplevels ->
        let desugared = List.map toplevels ~f:Term.desugar_toplevel in
        let resolved, _ = Resolve.resolve_program desugared in
        clean_run (check_program resolved)
    ;;
  end)
;;
