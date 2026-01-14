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
     | None -> failwith @@ Printf.sprintf "No variable '%s' in scope" (Intern.lookup name))
  | `Ann (e, _) -> eval e
  | `Type -> `Type
  | `Pi (x, dom, cod) ->
    let cod = fun v -> Env.local ~f:(Env.with_binding x ~value:v) (fun () -> eval cod) in
    `Pi (x, eval dom, cod)
  | `Lam (x, body) ->
    let body =
      fun v -> Env.local ~f:(Env.with_binding x ~value:v) (fun () -> eval body)
    in
    `Lam (x, body)
  | `App (f, x) -> app (eval f) (eval x)
  | `Proj (term, field) -> proj field (eval term)
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
  | `Infix { left; op; right } ->
    let op_val = eval op
    and left_val = eval left
    and right_val = eval right in
    app (app op_val left_val) right_val

and app f x =
  match f with
  | `Lam (_, body) -> body x
  | `Neutral n -> `Neutral (NApp (n, x))
  | _ -> failwith "Apply non-function"

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
;;

let rec quote (lvl : int) : Term.value -> Term.ast = function
  | `App (f, x) -> `App (quote lvl f, quote lvl x)
  | `Neutral n -> quote_neutral lvl n
  | `Type -> `Type
  | `Pi (x, a, b) ->
    let var = `Neutral (NVar (0, x)) in
    `Pi (x, quote lvl a, quote (succ lvl) (b var))
  | `Lam (x, b) ->
    let var = `Neutral (NVar (0, x)) in
    `Lam (x, quote (succ lvl) (b var))
  | `Lit lit -> `Lit (Term.over_literal (quote lvl) lit)
  | `Err e -> `Err e
  | `Rec r -> quote lvl r
  | `Row { fields; tail = None } ->
    let fields = Map.map ~f:(quote lvl) fields in
    `Lit (Record fields)
  | `Row { fields; tail = Some (`Neutral (NMeta _)) } ->
    let fields = Map.map ~f:(quote lvl) fields in
    `Lit (Record fields)
  | `Opaque ->
    failwith "Cannot quote opaque values - they should not appear in quotable contexts"
  | #base as b -> (Term.over_base (quote lvl) b :> Term.ast)
  | otherwise -> failwith (Printf.sprintf "Cannot handle '%s'" $ Term.show_value otherwise)

and quote_neutral (lvl : int) : neutral -> Term.ast = function
  | NVar (_, x) -> `Var x
  | NApp (f, x) -> `App (quote_neutral lvl f, quote lvl x)
  | NMeta m -> `Meta m
  | NProj (term, field) -> `Proj (quote_neutral lvl term, field)
;;

let rec infer : Term.ast -> (Term.value * Term.ast, CalyxError.t) result = function
  | `Var i ->
    print_endline "infer.Var";
    let* ty = Env.lookup_type i |> Result.of_option ~error:(`NotFound i) in
    Ok (ty, `Ann (`Var i, quote 0 ty))
  (* PI : (a : A) -> B *)
  | `Pi (x, dom, cod) ->
    print_endline "infer.Pi";
    let* value = Result.map ~f:eval $ check dom `Type in
    let* _ = Env.local ~f:(Env.with_binding x ~value) (fun () -> check cod `Type) in
    Ok (`Type, `Pi (x, dom, cod))
  (* LAM : \x -> x *)
  | `Lam (x, body) ->
    print_endline "infer.Lam";
    let dom = `Neutral (NMeta (Meta.fresh ())) in
    let* body_ty, body = Env.fresh_var x dom (fun _ -> infer body) in
    let ty = `Pi (x, dom, Fun.const body_ty) in
    Ok (ty, `Ann (`Lam (x, body), quote 0 ty))
  (* APP : f x *)
  | `App (f, x) ->
    print_endline "infer.App";
    let* tf, f = infer f in
    let* dom, cod =
      match Solve.force tf with
      | `Pi (_, dom, cod) -> Ok (dom, cod (`Neutral (NVar (0, Intern.underscore))))
      | `Neutral (NMeta _) ->
        let dom = `Neutral (NMeta (Meta.fresh ())) in
        let cod = `Neutral (NMeta (Meta.fresh ())) in
        Solve.Constraints.(tell $ Unify (tf, `Pi (Intern.underscore, dom, Fun.const cod)));
        Ok (dom, cod)
      | otherwise -> Error (`Expected ("function", Term.show_value otherwise))
    in
    let* x' = check x dom in
    Ok (cod, `Ann (`App (f, x'), quote 0 cod))
  (* LET : let x : t in x *)
  | `Let (ident, typ, value, body) ->
    print_endline "infer.Let";
    let* typ =
      match typ with
      | Some t ->
        let* _ = check t `Type in
        Ok (eval t)
      | None -> Ok (`Neutral (NMeta (Meta.fresh ())))
    in
    let* value' = check value typ in
    let* body_ty, body =
      Env.local
        ~f:(Env.with_binding ident ~value:(eval value') ~typ)
        (fun () -> infer body)
    in
    Ok (body_ty, `Ann (`Let (ident, Some (quote 0 typ), value', body), quote 0 body_ty))
  (* ANN : (x : T) *)
  | `Ann (e, a) ->
    print_endline "infer.Ann";
    let* _ = check a `Type in
    let vt = eval a in
    let* e = check e vt in
    Ok (vt, `Ann (e, a))
  (* TYPE : Type *)
  | `Type ->
    print_endline "infer.Type";
    Ok (`Type, `Type)
  | `Proj (term, field) ->
    print_endline "infer.Proj";
    let* te, e' = infer term in
    let field_ty = `Neutral (NMeta (Meta.fresh ())) in
    Solve.Constraints.(tell $ HasField (field, te, field_ty));
    Ok (field_ty, `Ann (`Proj (e', field), quote 0 field_ty))
  | `Pos (p, term) ->
    print_endline "infer.Pos";
    Env.local ~f:(Env.with_pos p) (fun () -> infer term)
  | `Match (scrut, arms) ->
    print_endline "infer.Match";
    let* scrut_ty, scrut' = infer scrut in
    let infer_arm (pattern, body) =
      let* body_ty, body' = infer body in
      Ok ((pattern, `Ann (body', quote 0 body_ty)), body_ty)
    in
    let* arms_and_types = sequence @@ List.map ~f:infer_arm arms in
    let annotated_arms = List.map ~f:fst arms_and_types in
    let arm_types = List.map ~f:snd arms_and_types in
    (match arm_types with
     | [] -> Error (`Expected ("non-empty match", "empty match"))
     | first_ty :: rest_types ->
       let unify_first ty = Solve.Constraints.(tell $ Unify (first_ty, ty)) in
       List.iter ~f:unify_first rest_types;
       Ok
         ( first_ty
         , `Ann
             (`Match (`Ann (scrut', quote 0 scrut_ty), annotated_arms), quote 0 first_ty)
         ))
  | `Lit lit ->
    print_endline "infer.Lit";
    let* ty, lit' = infer_lit lit in
    Ok (ty, `Ann (`Lit lit', quote 0 ty))
  | `Meta m ->
    print_endline "infer.Meta";
    Ok (`Neutral (NMeta m), `Meta m)
  | `Err e ->
    print_endline "infer.Err";
    Ok (`Err e, `Err e)
  | `Infix { left; op; right } ->
    print_endline "infer.Infix";
    (* Convert infix to nested application for type checking *)
    let app_expr = `App (`App (op, left), right) in
    let* ty, checked_app = infer app_expr in
    (* Extract the typed sub-expressions and reconstruct infix *)
    (match checked_app with
     | `Ann (`App (`App (op', left'), right'), result_ty) ->
       let infix_expr = `Infix { left = left'; op = op'; right = right' } in
       Ok (ty, `Ann (infix_expr, result_ty))
     | _ ->
       (* This shouldn't happen, but if it does, try to preserve structure *)
       let* _, left' = infer left in
       let* _, op' = infer op in
       let* _, right' = infer right in
       Ok (ty, `Infix { left = left'; op = op'; right = right' }))

and infer_lit
  : Term.ast Term.literal -> (Term.value * Term.ast Term.literal, CalyxError.t) result
  = function
  | Int n -> Ok (`Var (Intern.intern "Int"), Int n)
  | UInt n -> Ok (`Var (Intern.intern "UInt"), UInt n)
  | Float x -> Ok (`Var (Intern.intern "Float"), Float x)
  | Bool b -> Ok (`Var (Intern.intern "Bool"), Bool b)
  | Record _structure ->
    (* TODO: We should attempt to find the type name here and return an 
      `Ann (record, type_name). We cannot do this without refactoring the 
      record system in general. Term.value's `Rec and `Row shouldn't exist,
      we should just have one Record literal and if it's at the type level
      then that's a row type 100% of the time. If a record is referred to by 
      name, that's nominal. EzPz
    *)
    failwith "TODO"

and check : Term.ast -> Term.value -> (Term.ast, CalyxError.t) result =
  fun term expected ->
  match term, expected with
  | `Lam (x, body), `Pi (_, dom, cod) ->
    let* body' = Env.fresh_var x dom (fun var -> check body (cod var)) in
    Ok (`Lam (x, body'))
  | `Lam (x, body), `Neutral (NMeta _ as m) ->
    let dom = `Neutral (NMeta (Meta.fresh ())) in
    let cod = `Neutral (NMeta (Meta.fresh ())) in
    Solve.Constraints.(tell @@ Unify (`Neutral m, `Pi (x, dom, Fun.const cod)));
    let* body' = Env.fresh_var x dom $ fun _ -> check body cod in
    Ok (`Lam (x, body'))
  | `Let (ident, ty, value, body), expected ->
    print_endline "Computing vty";
    let* vty =
      match ty with
      | Some t ->
        let* _ = check t `Type in
        Ok (eval t)
      | None -> Ok (`Neutral (NMeta (Meta.fresh ())))
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
    Solve.Constraints.(tell @@ Unify (typ', expected));
    let* x = check expression typ' in
    Ok (`Ann (x, typ))
  | term, expected ->
    let* inferred, term' = infer term in
    (* FIXME: Crash happens here *)
    Solve.Constraints.(tell @@ Unify (inferred, expected));
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
  :  ast declaration list
  -> (ast declaration list * Term.value Meta.gen, CalyxError.t list) result
  =
  fun program ->
  let rec go
    :  Term.ast Term.declaration list
    -> (Term.ast Term.declaration list, CalyxError.t list) result
    = function
    | Function { ident; typ; body } :: rest ->
      let vty = eval typ in
      Printf.printf
        "infer_toplevel.Function { ident = %s; typ = %s; body = %s }\n"
        (Intern.lookup ident)
        (Term.show_value vty)
        (Term.show_ast body);
      let placeholder = `Neutral (NVar (Env.level (), ident)) in
      print_endline "ENTERING ENV.LOCAL";
      Env.local ~f:(Env.with_binding ident ~value:placeholder ~typ:vty) (fun () ->
        let* body = Result.map_error ~f:Core.List.singleton @@ check body vty in
        let typ = quote 0 vty in
        Result.map ~f:(List.cons (Function { ident; typ; body })) @@ go rest)
    | Constant { ident; typ; body } :: rest ->
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
        Result.map ~f:(List.cons (Constant { ident; typ; body })) @@ go rest)
      (* TODO: Handle records *)
    | RecordDecl _ :: rest -> go rest
    | [] -> Ok []
  in
  let result, gen =
    let meta_gen = Meta.default () in
    Meta.handle meta_gen (fun () ->
      Solve.Solution.handle meta_gen (fun () ->
        let program, constraints = Solve.Constraints.handle (fun () -> go program) in
        match program with
        | Ok program ->
          (match Solve.solve constraints with
           | Ok () -> Ok program
           | Error es ->
             print_endline (Solve.pretty_solver_error es);
             failwith "Failed to type program")
        | Error es -> Error es))
  in
  Result.map ~f:(Tuple.intoRev gen) result
;;
