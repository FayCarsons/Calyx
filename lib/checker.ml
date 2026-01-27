open Core
open Context.Syntax
open Term
module Intern = Ident.Intern

let over_literal (f : 'a -> 'b) : 'a Term.literal -> 'b Term.literal = function
  | Int n -> Int n
  | UInt n -> UInt n
  | Float x -> Float x
  | Bool b -> Bool b
  | Record fields -> Record (Map.map ~f fields)
;;

let rec eval : Term.t -> Term.value Context.t =
  fun tm ->
  Context.trace Trace.Eval tm [%here]
  @@
  match tm with
  | `Var name ->
    Context.lookup_value name
    >>= (function
     | Some `Opaque -> Context.pure @@ `Neutral (NVar (0, name))
     | Some other -> Context.pure other
     | None -> Context.pure @@ `Neutral (NVar (0, name)))
  | `Ann (e, _) -> eval e
  | `Type -> Context.pure `Type
  | `Pi { plicity; ident; dom; cod } ->
    let* dom_val = eval dom in
    (* Capture current environment for lexical scoping *)
    let* cod =
      Context.close ~f:(fun value ->
        Context.local ~f:(Context.with_binding ident ~value ~typ:dom_val) (eval cod))
    in
    Context.pure @@ `Pi (plicity, ident, dom_val, cod)
  | `Lam (plicity, x, body) ->
    let* body =
      Context.close ~f:(fun value ->
        Context.local ~f:(Context.with_binding x ~value) (eval body))
    in
    Context.pure @@ `Lam (plicity, x, body)
  | `App (f, x) ->
    let* f = eval f in
    let* x = eval x in
    app f x
  | `Infix { left; op; right } ->
    let* op_val = eval op in
    let* left_val = eval left in
    let* right_val = eval right in
    app op_val left_val >>= Fun.flip app right_val
  | `Let (ident, Some ty, value, body) ->
    let* typ = eval ty in
    let* value = eval value in
    Context.local ~f:(Context.with_binding ident ~value ~typ) (eval body)
  | `Let (ident, None, value, body) ->
    let* value = eval value in
    Context.local ~f:(Context.with_binding ident ~value) (eval body)
  | `Match (scrut, arms) ->
    let* scrut = eval scrut in
    let* arms =
      Context.traverse
        ~f:(fun (pat, body) ->
          let* pat = pattern pat in
          let* body = eval body in
          Context.pure (pat, body))
        arms
    in
    Context.pure @@ `Match (scrut, arms)
  | `Pos (pos, exp) -> Context.local ~f:(Context.with_pos pos) (eval exp)
  | `Lit lit ->
    (match lit with
     | Record fields ->
       let* fields =
         Context.traverse
           ~f:(fun (ident, value) ->
             let* value = eval value in
             Context.pure (ident, value))
           (Map.to_alist fields)
       in
       Context.pure @@ `Lit (Record (Ident.Map.of_alist_exn fields))
     | Int n -> Context.pure @@ `Lit (Int n)
     | UInt n -> Context.pure @@ `Lit (UInt n)
     | Float x -> Context.pure @@ `Lit (Float x)
     | Bool b -> Context.pure @@ `Lit (Bool b))
  | `Meta m -> Context.pure @@ `Neutral (NMeta m)
  | `Err e -> Context.pure @@ `Err e
  | `RecordType { fields; tail } ->
    let* fields = Context.traverse_map ~f:eval fields in
    let* tail : Term.value option =
      match tail with
      | Some tail -> Context.map ~f:Option.some @@ eval tail
      | None -> Context.pure None
    in
    Context.pure (`RecordType { fields; tail } : Term.value)
  | `SumType { ident; params; constructors; position } ->
    let* params = Context.traverse_map ~f:eval params in
    let* constructors = Context.traverse_map ~f:(Context.traverse ~f:eval) constructors in
    Context.pure @@ `SumType { ident; params; constructors; position }
  | `Proj (term, field) -> eval term >>= proj field

and app : value -> value -> value Context.t =
  fun f x ->
  match f with
  | `Lam (_, _, body) -> Context.liftR @@ (body x : (value, CalyxError.t) result)
  | `Neutral n -> Context.pure @@ `Neutral (NApp (n, x))
  | `SumType { ident; params; constructors; position } ->
    (* Apply a sum type to a type argument
       For example, Option (a : Type) becomes Type -> Option
       This is probably not how we want to do this in the longterm but it works right now
    *)
    Context.pure
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

and proj (field : Ident.t) : Term.value -> Term.value Context.t = function
  | `Lit (Record fields) -> Context.pure @@ Map.find_exn fields field
  | `Neutral n -> Context.pure @@ `Neutral (NProj (n, field))
  | other ->
    Context.fail (`Expected ("record", Sexp.to_string_hum @@ Term.sexp_of_value other))

and pattern : Term.t Term.pattern -> Term.value Term.pattern Context.t = function
  | PVar x -> Context.pure @@ PVar x
  | PWild -> Context.pure @@ PWild
  | PCtor (ctor, args) ->
    let* args = Context.traverse ~f:pattern args in
    Context.pure @@ PCtor (ctor, args)
  | PLit lit ->
    (match lit with
     | Record fields ->
       let* fields =
         Context.traverse
           ~f:(fun (ident, value) ->
             let* value = eval value in
             Context.pure (ident, value))
           (Map.to_alist fields)
       in
       Context.pure @@ PLit (Record (Ident.Map.of_alist_exn fields))
     | Int n -> Context.pure @@ PLit (Int n)
     | UInt n -> Context.pure @@ PLit (UInt n)
     | Float x -> Context.pure @@ PLit (Float x)
     | Bool b -> Context.pure @@ PLit (Bool b))
  | PRec fields ->
    let* fields =
      Context.traverse
        ~f:(fun (ident, pat) ->
          let* pat = pattern pat in
          Context.pure (ident, pat))
        fields
    in
    Context.pure @@ PRec fields

(* Resolve a type to a SumType, looking up neutral variables in the environment *)
and resolve_sum_type : Term.value -> Term.value Term.sum_type option Context.t = function
  | `SumType s -> Context.pure @@ Some s
  | `Neutral (NVar (_, name)) ->
    (* Look up the name in the environment to find the actual SumType *)
    Context.lookup_value name
    >>= (function
     | Some (`SumType s) -> Context.pure @@ Some s
     | _ -> Context.pure @@ None)
  | _ -> Context.pure @@ None

(* Extract bindings from a pattern given the scrutinee type *)
and pattern_bindings
  : Term.value Term.pattern -> Term.value -> (Ident.t * Term.value) list Context.t
  =
  fun pat scrut_ty ->
  match pat with
  | PVar x -> Context.pure [ x, scrut_ty ]
  | PWild -> Context.pure []
  (* TODO: Record pattern bindings (also appear inside of [literal]) *)
  | PLit _ -> Context.pure []
  | PRec _ -> Context.pure []
  | PCtor (ctor_name, args) ->
    (* Extract constructor argument types from the scrutinee's sum type *)
    resolve_sum_type scrut_ty
    >>= (function
     | Some { constructors; _ } ->
       (* If this isn't found in the [constructors] map then something has really gone wrong *)
       Map.find_exn constructors ctor_name
       |> List.zip_exn args
       |> Context.traverse ~f:(Tuple2.uncurry pattern_bindings)
       >|= List.concat
     | None -> Context.pure [])
;;

let rec quote : int -> Term.value -> Term.t Context.t =
  fun lvl tm ->
  Context.trace Trace.Quote tm [%here]
  @@
  match tm with
  | `App (f, x) ->
    let* f = quote lvl f in
    let* x = quote lvl x in
    Context.pure @@ `App (f, x)
  | `Neutral n -> quote_neutral lvl n
  | `Type -> Context.pure `Type
  | `Pi (plicity, ident, dom, cod) ->
    (* level shouldn't matter here because [var] is just used to access the body of [cod] *)
    let var = `Neutral (NVar (lvl, ident)) in
    let* dom = quote lvl dom in
    let* cod = Context.liftR (cod var) >>= quote (succ lvl) in
    Context.pure @@ `Pi { plicity; ident; dom; cod }
  | `Lam (plicity, x, b) ->
    let var = `Neutral (NVar (0, x)) in
    let* body = Context.liftR (b var) >>= quote (succ lvl) in
    Context.pure @@ `Lam (plicity, x, body)
  | `Lit lit ->
    (match lit with
     | Record fields ->
       let* fields = Context.traverse_map ~f:(quote lvl) fields in
       Context.pure @@ `Lit (Record fields)
     | Int n -> Context.pure @@ `Lit (Int n)
     | UInt n -> Context.pure @@ `Lit (UInt n)
     | Float x -> Context.pure @@ `Lit (Float x)
     | Bool b -> Context.pure @@ `Lit (Bool b))
  | `RecordType { fields; tail } ->
    let* fields = Context.traverse_map ~f:(quote lvl) fields in
    let* tail =
      match tail with
      | Some tail -> Context.map ~f:Option.some @@ quote lvl tail
      | None -> Context.pure None
    in
    Context.pure @@ (`RecordType { fields; tail } : Term.t)
  | `SumType { ident; params; constructors; position } ->
    let* params = Context.traverse_map ~f:(quote lvl) params in
    let* constructors =
      Context.traverse_map ~f:(Context.traverse ~f:(quote lvl)) constructors
    in
    Context.pure @@ `SumType { ident; params; constructors; position }
  | `Err e -> Context.pure @@ `Err e
  | `Opaque -> failwith "Cannot quote opaque values, they should not appear here"
  | `Infix { left; op; right } ->
    let* left = quote lvl left in
    let* op = quote lvl op in
    let* right = quote lvl right in
    Context.pure @@ `Infix { left; op; right }
  | `Var i -> Context.pure @@ `Var i
  | `Match (scrut, arms) ->
    let* scrut = quote lvl scrut in
    let* arms =
      Context.traverse
        ~f:(fun (ptn, arm) ->
          let* ptn = quote_pattern lvl ptn in
          let* arm = quote lvl arm in
          Context.pure (ptn, arm))
        arms
    in
    Context.pure @@ `Match (scrut, arms)
  | `Ann (tm, typ) ->
    let* tm = quote lvl tm in
    let* typ = quote lvl typ in
    Context.pure @@ `Ann (tm, typ)
  | `Proj (tm, field) ->
    let* tm = quote lvl tm in
    Context.pure @@ `Proj (tm, field)

and quote_pattern (lvl : int) : Term.value Term.pattern -> Term.t Term.pattern Context.t
  = function
  | PLit (Record fields) ->
    let* fields = Context.traverse_map ~f:(quote lvl) fields in
    Context.pure @@ PLit (Record fields)
  | PLit lit ->
    Context.pure
    @@ PLit
         (match lit with
          | Int n -> Int n
          | UInt n -> UInt n
          | Float x -> Float x
          | Bool b -> Bool b
          | _ -> assert false)
  | PWild -> Context.pure PWild
  | PCtor (ident, args) ->
    let* args = Context.traverse ~f:(quote_pattern lvl) args in
    Context.pure @@ PCtor (ident, args)
  | PVar ident -> Context.pure @@ PVar ident
  | PRec _ -> Context.fail `Todo

and quote_neutral (lvl : int) : neutral -> Term.t Context.t = function
  | NVar (_, x) -> Context.pure @@ `Var x
  | NApp (f, x) ->
    let* f = quote_neutral lvl f in
    let* x = quote lvl x in
    Context.pure @@ `App (f, x)
  | NMeta m -> Context.pure @@ `Meta m
  | NProj (term, field) ->
    let* term = quote_neutral lvl term in
    Context.pure @@ `Proj (term, field)
;;

let rec infer : Term.t -> (Term.value * Term.t) Context.t =
  fun tm ->
  Context.trace Trace.Infer tm [%here]
  @@
  match tm with
  | `Var i ->
    let* ty = Context.lookup_type i >>= Context.liftO_or_fail ~error:(`NotFound i) in
    (* Create and insert metas for implicit params
       Something about this being in the 'Var' case feels gross, doing this
       sort of elaboration here feels wrong
    *)
    let rec insert_var_implicits : Term.value -> Term.t -> (Term.value * Term.t) Context.t
      =
      fun ty term ->
      Solve.force ty
      >>= function
      | `Pi (Implicit, _ident, dom, cod) ->
        let* meta = Context.fresh_meta in
        let meta_val = `Neutral (NMeta meta) in
        let* dom = quote 0 dom in
        let new_term = `App (term, `Ann (`Meta meta, dom)) in
        let* cod = Context.liftR (cod meta_val) in
        insert_var_implicits cod new_term
      | _ -> Context.pure (ty, term)
    in
    let* result_ty, annotated_term = insert_var_implicits ty (`Var i) in
    let* typ = quote 0 result_ty in
    Context.pure (result_ty, `Ann (annotated_term, typ))
  | `Pi { plicity; ident; dom; cod } ->
    let* value = check dom `Type >>= eval in
    let* _ = Context.local ~f:(Context.with_binding ident ~value) (check cod `Type) in
    Context.pure (`Type, `Pi { plicity; ident; dom; cod })
  | `Lam (plicity, x, body) ->
    let* meta = Context.fresh_meta in
    let dom = `Neutral (NMeta meta) in
    let* body_ty, body = Context.with_var x ~typ:dom ~f:(fun _ -> infer body) in
    let ty = `Pi (plicity, x, dom, fun _ -> Ok body_ty) in
    let* quoted = quote 0 ty in
    Context.pure (ty, `Ann (`Lam (plicity, x, body), quoted))
  | `App (f, x) ->
    let* tf, f = infer f in
    let rec insert_implicits tf f =
      let* tf = Solve.force tf in
      match tf with
      | `Pi (Implicit, _ident, dom, cod) ->
        let* level = Context.level in
        let meta = Meta.fresh level in
        let meta_val = `Neutral (NMeta meta) in
        let* quoted_dom = quote 0 dom in
        let new_f = `App (f, `Ann (`Meta meta, quoted_dom)) in
        let* cod = Context.liftR (cod meta_val) in
        insert_implicits cod new_f
      | `Pi (Explicit, _, dom, cod) ->
        let* x' = check x dom in
        let* result_ty = Context.liftR (cod (`Neutral (NVar (0, Intern.underscore)))) in
        let* quoted_ty = quote 0 result_ty in
        Context.pure (result_ty, `Ann (`App (f, x'), quoted_ty))
      | `Neutral (NMeta _) ->
        let* level = Context.level in
        let dom = `Neutral (NMeta (Meta.fresh level)) in
        let cod = `Neutral (NMeta (Meta.fresh level)) in
        let* _ =
          Context.tell_constraint
            (Constraint.equals
               tf
               (`Pi (Explicit, Intern.underscore, dom, fun _ -> Ok cod)))
        in
        let* x' = check x dom in
        let* quoted_cod = quote 0 cod in
        Context.pure (cod, `Ann (`App (f, x'), quoted_cod))
      | otherwise -> Context.fail (`Expected ("function", Term.show_value otherwise))
    in
    insert_implicits tf f
  | `Infix { left; op; right } ->
    (* Treat infix like nested application *)
    let app_expr = `App (`App (op, left), right) in
    let* ty, checked_app = infer app_expr in
    (match checked_app with
     | `Ann (`App (`App (op, left), right), result_ty) ->
       let infix_expr = `Infix { left; op; right } in
       Context.pure (ty, `Ann (infix_expr, result_ty))
     | _ ->
       let* _, left = infer left in
       let* _, op = infer op in
       let* _, right = infer right in
       Context.pure (ty, `Infix { left; op; right }))
  | `Let (ident, typ, value, body) ->
    let* typ =
      match typ with
      | Some t ->
        let* _ = check t `Type in
        eval t
      | None ->
        let* meta = Context.fresh_meta in
        Context.pure @@ `Neutral (NMeta meta)
    in
    let* value' = check value typ in
    let* value'' = eval value' in
    let* body_ty, body =
      Context.local ~f:(Context.with_binding ident ~value:value'' ~typ) (infer body)
    in
    let* typ = quote 0 typ in
    let* body_typ = quote 0 body_ty in
    Context.pure (body_ty, `Ann (`Let (ident, Some typ, value', body), body_typ))
  | `Ann (e, a) ->
    let* _ = check a `Type in
    let* vt = eval a in
    let* e = check e vt in
    Context.pure (vt, `Ann (e, a))
  | `Type ->
    (* print_endline "infer.Type"; *)
    Context.pure (`Type, `Type)
  | `Proj (term, field) ->
    (* print_endline "infer.Proj"; *)
    infer_proj term field
  | `Pos (p, term) ->
    (* print_endline "infer.Pos"; *)
    Context.local ~f:(Context.with_pos p) (infer term)
  | `Match (scrut, arms) ->
    (* print_endline "infer.Match"; *)
    let* scrut_ty, scrut = infer scrut in
    let infer_arm (pat, body) =
      let* val_pat = pattern pat in
      let* bindings = pattern_bindings val_pat scrut_ty in
      let* level = Context.level in
      (* Introduce pattern bindings into environment *)
      Context.local
        ~f:(fun ctx ->
          List.fold_right bindings ~init:ctx ~f:(fun (ident, typ) ctx ->
            Context.with_binding ident ~value:(`Neutral (NVar (level, ident))) ~typ ctx))
        (let* body_ty, body = infer body in
         let* body_ty' = quote 0 body_ty in
         Context.pure ((pat, `Ann (body, body_ty')), body_ty))
    in
    let* arms_and_types = Context.traverse ~f:infer_arm arms in
    let annotated_arms = List.map ~f:fst arms_and_types in
    let arm_types = List.map ~f:snd arms_and_types in
    (match arm_types with
     | [] ->
       Context.fail (`Expected ("non-empty match", Term.show (`Match (scrut, arms))))
     | first_ty :: rest_types ->
       let* _ =
         Context.traverse
           rest_types
           ~f:(Fun.compose Context.tell_constraint (Constraint.equals first_ty))
       in
       let* scrut_typ = quote 0 scrut_ty in
       let* first_typ = quote 0 first_ty in
       Context.pure
         ( first_ty
         , (`Ann (`Match (`Ann (scrut, scrut_typ), annotated_arms), first_typ) : Term.t)
         ))
  | `Lit lit ->
    (* print_endline "infer.Lit"; *)
    let* ty, lit' = infer_lit lit in
    let* ty' = quote 0 ty in
    Context.pure (ty, `Ann (`Lit lit', ty'))
  | `Meta m ->
    (* print_endline "infer.Meta"; *)
    Context.pure (`Neutral (NMeta m), `Meta m)
  | `Err e ->
    (* print_endline "infer.Err"; *)
    Context.pure (`Err e, `Err e)
  | `RecordType { fields; tail } ->
    let* record_val =
      let* fields = Context.traverse_map fields ~f:eval in
      let* tail =
        match tail with
        | Some tail -> eval tail >|= Option.some
        | None -> Context.pure None
      in
      Context.pure @@ (`RecordType { fields; tail } : Term.value)
    in
    let* record_val' = quote 0 record_val in
    Context.pure (`Type, `Ann ((`RecordType { fields; tail } : Term.t), record_val'))
  | `SumType { ident; params; constructors; position } ->
    (* Sum types have type Type *)
    let* sum_val =
      let* params = Context.traverse_map ~f:eval params in
      let* constructors =
        Context.traverse_map ~f:(Context.traverse ~f:eval) constructors
      in
      quote 0 @@ `SumType { ident; params; constructors; position }
    in
    Context.pure
      (`Type, `Ann (`SumType { ident; params; constructors; position }, sum_val))

and infer_lit : Term.t Term.literal -> (Term.value * Term.t Term.literal) Context.t =
  function
  | Int n -> Context.pure (`Neutral (NVar (0, Intern.intern "Int")), Int n)
  | UInt n -> Context.pure (`Neutral (NVar (0, Intern.intern "UInt")), UInt n)
  | Float x -> Context.pure (`Neutral (NVar (0, Intern.intern "Float")), Float x)
  | Bool b -> Context.pure (`Neutral (NVar (0, Intern.intern "Bool")), Bool b)
  | Record fields ->
    let* fields =
      Context.traverse_map
        ~f:(fun data ->
          let* ty, value = infer data in
          Context.pure (ty, value))
        fields
    in
    let field_values = Map.map ~f:snd fields in
    let field_types = Map.map ~f:fst fields in
    let record_type : Term.value =
      let fields = field_types in
      `RecordType { fields; tail = None }
    in
    let fields = field_values in
    Context.pure (record_type, Record fields)

(** Infer the type of [field] in [term] *)
and infer_proj : Term.t -> Ident.t -> (Term.value * Term.t) Context.t =
  fun term field ->
  let* typ, annotated = infer term in
  match typ with
  | `RecordType row ->
    (match Map.find row.fields field with
     | Some field_type -> Context.pure (field_type, `Proj (annotated, field))
     | None ->
       (match row.tail with
        | Some tail ->
          let* meta = Context.fresh_meta in
          let field_type = `Neutral (NMeta meta) in
          let* _ =
            Context.tell_constraint
              (Constraint.has_field ~record:tail ~field_name:field ~field_type)
          in
          Context.pure (field_type, `Proj (annotated, field))
        | None ->
          Context.fail
            (`NoField (field, Map.to_alist @@ Map.map ~f:Term.show_value row.fields))))
  | `Neutral n ->
    let* meta = Context.fresh_meta in
    let field_type = `Neutral (NMeta meta) in
    let* partial =
      let fields : Term.value Ident.Map.t = Ident.Map.singleton field field_type in
      let* meta = Context.fresh_meta in
      let tail : Term.value option = Some (`Neutral (NMeta meta)) in
      Context.pure @@ `RecordType ({ fields; tail } : Term.value Term.row)
    in
    let* _ = Context.tell_constraint (Constraint.equals (`Neutral n) partial) in
    Context.pure (field_type, `Proj (annotated, field))
  | other -> Context.fail (`Expected ("Record", Term.show_value other))

and check : Term.t -> Term.value -> Term.t Context.t =
  fun term expected ->
  Context.trace (Trace.Check expected) term [%here]
  @@
  match term, expected with
  | `Lam (plicity, x, body), `Pi (plicity', _, dom, cod) ->
    if Term.equal_plicity plicity plicity'
    then
      let* body' =
        Context.with_var x ~typ:dom ~f:(fun var -> Context.liftR (cod var) >>= check body)
      in
      let* expected = quote 0 expected in
      Context.pure (`Ann (`Lam (plicity, x, body'), expected))
    else
      Context.fail
        (`Expected
            ( "terms to have matching plicity"
            , Printf.sprintf "%s\nvs.\n%s" (Term.show term) (Term.show_value expected) ))
  | `Lam (plicity, x, body), `Neutral (NMeta _ as m) ->
    let* dom_meta = Context.fresh_meta in
    let* cod_meta = Context.fresh_meta in
    let dom = `Neutral (NMeta dom_meta) in
    let cod = `Neutral (NMeta cod_meta) in
    let* _ =
      Context.tell_constraint
        (Constraint.equals (`Neutral m) (`Pi (plicity, x, dom, Fun.const (Ok cod))))
    in
    let* body' = Context.with_var x ~typ:dom ~f:(fun _ -> check body cod) in
    let* expected = quote 0 expected in
    Context.pure (`Ann (`Lam (plicity, x, body'), expected))
  | `Let (ident, ty, value, body), expected ->
    let* vty =
      match ty with
      | Some t ->
        let* _ = check t `Type in
        eval t
      | None ->
        let* meta = Context.fresh_meta in
        Context.pure (`Neutral (NMeta meta))
    in
    let* value = check value vty in
    let* value' = eval value in
    let* body =
      Context.local
        ~f:(Context.with_binding ident ~value:value' ~typ:vty)
        (check body expected)
    in
    let* vty = quote 0 vty in
    Context.pure (`Let (ident, Some vty, value, body))
  | `Pos (pos, term), expected ->
    Context.local
      ~f:(Context.with_pos pos)
      (let* term = check term expected in
       Context.pure (`Pos (pos, term)))
  | `Ann (expression, typ), expected ->
    let* _ = check typ `Type in
    let* typ' = eval typ in
    let* _ = Context.tell_constraint (Constraint.equals typ' expected) in
    let* x = check expression typ' in
    Context.pure (`Ann (x, typ))
  | term, expected ->
    let* inferred, term' = infer term in
    let* _ = Context.tell_constraint (Constraint.equals inferred expected) in
    Context.pure term'

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
  : Term.t Term.declaration list -> Term.t Term.declaration list Context.t
  =
  fun program ->
  let rec go = function
    | Function { ident; typ; body; position } :: rest ->
      let* vty = eval typ in
      let* level = Context.level in
      let placeholder = `Neutral (NVar (level, ident)) in
      Context.local
        ~f:(Context.with_binding ident ~value:placeholder ~typ:vty)
        (let* body = check body vty in
         let* typ = quote 0 vty in
         Context.map
           (go rest)
           ~f:
             (List.cons
                (Function { ident; typ; body; position } : Term.t Term.declaration)))
    | Constant { ident; typ; body; position } :: rest ->
      let* typ, value =
        let* vty = eval typ in
        let* body_ast = check body vty in
        Context.pure (vty, body_ast)
      in
      let* value = eval value in
      Context.local
        ~f:(Context.with_binding ident ~typ ~value)
        (let* typ = quote 0 typ in
         Context.map (go rest) ~f:(List.cons (Constant { ident; typ; body; position })))
    | RecordDecl { ident; params = _; fields; position } :: rest ->
      (* Evaluate field types and construct the record type *)
      let* field_types = Context.traverse_map fields ~f:eval in
      let record_type : Term.value = `RecordType { fields = field_types; tail = None } in
      (* Bind the record type name to the record type value *)
      Context.local
        ~f:(Context.with_binding ident ~value:record_type ~typ:`Type)
        (Context.map
           ~f:
             (List.cons
                (RecordDecl { ident; params = Ident.Map.empty; fields; position }))
         @@ go rest)
    (* Sum Types *)
    | SumDecl { ident; params; constructors; position } :: rest ->
      let* eval_params = Context.traverse_map ~f:eval params in
      (* Evaluate constructor arg types - recursive refs become Neutral(NVar(_, ident))
         which pattern_bindings resolves via environment lookup *)
      let* eval_constructors =
        Context.traverse_map constructors ~f:(Context.traverse ~f:eval)
      in
      let sum_type =
        `SumType
          { ident; params = eval_params; constructors = eval_constructors; position }
      in
      (* Build constructor type - fields are ASTs, evaluated inside closures *)
      let build_ctor_type (fields : Term.t list) : Term.value Context.t =
        let rec build param_vars remaining_params =
          match remaining_params with
          | [] ->
            let* eval_fields = Context.traverse fields ~f:eval in
            let* applied = Context.fold_left param_vars ~init:sum_type ~f:app in
            Context.fold_left eval_fields ~init:applied ~f:(fun acc field ->
              Context.pure
              @@ `Pi (Explicit, Ident.Intern.underscore, field, Fun.const (Ok acc)))
          | (param_name, kind) :: rest_params ->
            let* cod =
              Context.close ~f:(fun value ->
                Context.local
                  ~f:(Context.with_binding param_name ~value ~typ:kind)
                  (build (param_vars @ [ value ]) rest_params))
            in
            Context.pure @@ `Pi (Implicit, param_name, kind, cod)
        in
        build [] @@ Map.to_alist eval_params
      in
      (* Build constructor bindings with sum_type in scope for recursive references *)
      let* constructor_bindings =
        Context.local
          ~f:(Context.with_binding ident ~value:sum_type ~typ:`Type)
          (Map.to_alist ~key_order:`Increasing constructors
           |> Context.traverse ~f:(fun (ctor, fields) ->
             let* ctor_pi = build_ctor_type fields in
             Context.pure (ctor, `Opaque, ctor_pi)))
      in
      let type_binding = ident, sum_type, `Type in
      Context.local
        ~f:(fun ctx ->
          List.fold_right
            (type_binding :: constructor_bindings)
            ~init:ctx
            ~f:(fun (ident, value, typ) -> Context.with_binding ident ~value ~typ))
        (Context.map
           ~f:(List.cons (SumDecl { ident; params; constructors; position }))
           (go rest))
    | [] -> Context.pure []
  in
  go program
;;
