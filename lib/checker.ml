open Util
open Term

let over_literal (f : 'a -> 'b) : 'a Term.literal -> 'b Term.literal = function
  | Int n -> Int n
  | UInt n -> UInt n
  | Float x -> Float x
  | Bool b -> Bool b
  | Record fields -> Record (List.map (fun (i, x) -> i, f x) fields)
;;

let rec eval : Term.ast -> Term.value = function
  | `Var name -> Env.lookup_value name
  | `Ann (e, _) -> eval e
  | `Type -> `Type
  | `Pi (x, dom, cod) ->
    let cod = fun v -> Env.local_untyped x ~value:v @@ fun () -> eval cod in
    `Pi (x, eval dom, cod)
  | `Lam (x, body) ->
    let body = fun v -> Env.local_untyped x ~value:v @@ fun () -> eval body in
    `Lam (x, body)
  | `App (f, x) -> app (eval f) (eval x)
  | `Proj (term, field) -> proj (eval term) field
  | `Let (ident, Some ty, value, body) ->
    let ty = eval ty
    and value = eval value in
    Env.local_typed ident ~value ~ty @@ fun () -> eval body
  | `Let (ident, None, value, body) ->
    let value = eval value in
    Env.local_untyped ident ~value @@ fun () -> eval body
  | `Match (_scrut, _arms) -> failwith "TODO"
  | `Pos (pos, exp) ->
    Env.set_pos pos;
    eval exp
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

and proj (term : Term.value) (field : Ident.t) : Term.value =
  match term with
  | `Lit (Record fields) -> List.assoc field fields
  | `Neutral n -> `Neutral (NProj (n, field))
  | _ -> failwith "Projection from non-record"
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
  | #base as b -> (Term.over_base (quote lvl) b :> Term.ast)
  | otherwise -> failwith (Printf.sprintf "Cannot handle '%s'" $ Pretty.value otherwise)

and quote_neutral (lvl : int) : neutral -> Term.ast = function
  | NVar (_, x) -> `Var x
  | NApp (f, x) -> `App (quote_neutral lvl f, quote lvl x)
  | NMeta m -> `Meta m
  | NProj (term, field) -> `Proj (quote_neutral lvl term, field)
;;

let rec infer : Term.ast -> (Term.value * Term.ast, Error.t) result = function
  | `Var i ->
    print_endline "infer.Var";
    let* ty = Env.lookup_type i |> Option.to_result ~none:(`NotFound i) in
    Ok (ty, `Ann (`Var i, quote 0 ty))
  (* PI : (a : A) -> B *)
  | `Pi (x, dom, cod) ->
    print_endline "infer.Pi";
    let* value = Result.map eval $ check dom `Type in
    let* _ = Env.local_untyped x ~value (fun () -> check cod `Type) in
    Ok (`Type, `Pi (x, dom, cod))
  (* LAM : \x -> x *)
  | `Lam (x, body) ->
    print_endline "infer.Lam";
    let dom = `Neutral (NMeta (Meta.fresh ())) in
    let cod = `Neutral (NMeta (Meta.fresh ())) in
    let* _, body = Env.fresh_var x dom $ fun _ -> infer body in
    let ty = `Pi (x, dom, Fun.const cod) in
    Ok (ty, `Ann (`Lam (x, body), quote 0 ty))
  (* APP : f x *)
  | `App (f, x) ->
    print_endline "infer.App";
    let* tf, f = infer f in
    let* dom, cod =
      match Solve.force tf with
      | `Pi (_, dom, cod) -> Ok (dom, cod (`Neutral (NVar (0, "_"))))
      | `Neutral (NMeta _) ->
        let dom = `Neutral (NMeta (Meta.fresh ())) in
        let cod = `Neutral (NMeta (Meta.fresh ())) in
        Solve.Constraints.(tell $ Unify (tf, `Pi ("_", dom, Fun.const cod)));
        Ok (dom, cod)
      | otherwise -> Error (`Expected ("function", Pretty.value otherwise))
    in
    let* x' = check x dom in
    Ok (cod, `Ann (`App (f, x'), quote 0 cod))
  (* LET : let x : t in x *)
  | `Let (ident, ty, value, body) ->
    print_endline "infer.Let";
    let* ty =
      match ty with
      | Some t ->
        let* _ = check t `Type in
        Ok (eval t)
      | None -> Ok (`Neutral (NMeta (Meta.fresh ())))
    in
    let* value' = check value ty in
    let* body_ty, body =
      Env.local_typed ident ~value:(eval value') ~ty $ fun () -> infer body
    in
    Ok (body_ty, `Ann (`Let (ident, Some (quote 0 ty), value', body), quote 0 body_ty))
  (* ANN : (x : T) *)
  | `Ann (e, a) ->
    print_endline "infer.Ann";
    let* _ = check a `Type in
    let vt = eval a in
    let* e = check e vt in
    Ok (vt, `Ann (e, a))
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
    Env.set_pos p;
    infer term
  | `Match (_x, _arms) ->
    print_endline "infer.Match";
    failwith "TODO"
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
    (* Type check as function application but preserve infix structure *)
    let* left_type, left = infer left in
    let* op_type, op = infer op in
    let* right_type, right = infer right in
    (match op_type with
     | `Pi (_, dom, cod) ->
       let var = `Neutral (NVar (0, "_")) in
       let cod = cod var in
       (match cod with
        | `Pi (_, dom', _) ->
          let left_type = quote 0 left_type
          and right_type = quote 0 right_type
          and op_type = quote 0 op_type in
          let* _ = check left_type dom in
          let* _ = check right_type dom' in
          let result =
            `Infix
              { left = `Ann (left, left_type)
              ; op = `Ann (op, op_type)
              ; right = `Ann (right, right_type)
              }
          in
          Ok (cod, result)
        | other -> Error (`Expected ("function", Pretty.value other)))
     | other -> Error (`Expected ("function", Pretty.value other)))

and infer_lit
  : Term.ast Term.literal -> (Term.value * Term.ast Term.literal, Error.t) result
  = function
  | Int n -> Ok (`Var "Int", Int n)
  | UInt n -> Ok (`Var "UInt", UInt n)
  | Float x -> Ok (`Var "Float", Float x)
  | Bool b -> Ok (`Var "Bool", Bool b)
  | Record structure ->
    let rec sequence = function
      | Ok x :: xs -> Result.map (List.cons x) $ sequence xs
      | [] -> Ok []
      | Error e :: _ -> Error e
    in
    let* fields =
      sequence
      $ List.map
          (fun (l, e) ->
             let* ty, e' = infer e in
             Ok ((l, ty), (l, e')))
          structure
    in
    let row_fields = List.map fst fields in
    let ast_fields = List.map snd fields in
    let tail = `Neutral (NMeta (Meta.fresh ())) in
    Ok (`Rec (`Row { fields = row_fields; tail }), Record ast_fields)

and check : Term.ast -> Term.value -> (Term.ast, Error.t) result =
  fun term expected ->
  match term, expected with
  | `Lam (x, body), `Pi (_, dom, cod) ->
    let* body' = Env.fresh_var x dom $ fun var -> check body (cod var) in
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
      Env.local_typed ident ~value:value' ~ty:vty @@ fun () -> check body expected
    in
    Ok (`Let (ident, Some (quote 0 vty), value, body))
  | `Pos (pos, term), expected ->
    Env.set_pos pos;
    let* term = check term expected in
    Ok (`Pos (pos, term))
  | `Ann (expression, typ), expected ->
    let* _ = check typ `Type in
    let typ' = eval typ in
    Solve.Constraints.(tell @@ Unify (typ', expected));
    let* x = check expression typ' in
    Ok (`Ann (x, typ))
  | term, expected ->
    let* inferred, term' = infer term in
    Solve.Constraints.(tell @@ Unify (inferred, expected));
    Ok term'

and row_extract : Ident.t -> (Ident.t * Term.value) list -> (Term.value, Error.t) result =
  fun field fields ->
  List.assoc_opt field fields
  |> Option.to_result
       ~none:
         (`NoField
             (field, List.map (fun (label, field) -> label, Pretty.value field) fields))
;;

let singleton x = [ x ]

let infer_toplevel
  :  ast declaration list
  -> (ast declaration list * Term.value Meta.gen, Error.t list) result
  =
  fun program ->
  let meta_gen = Meta.default () in
  print_endline "created meta_gen";
  let rec go
    :  Term.ast Term.declaration list
    -> (Term.ast Term.declaration list, Error.t list) result
    = function
    | Function { ident; typ; body } :: rest ->
      let* typ_value, body_ast =
        Result.map_error singleton
        $
        let vty = eval typ in
        let* body_ast = check body vty in
        Ok (vty, body_ast)
      in
      Env.local_typed ident ~ty:typ_value ~value:(eval body_ast) (fun () ->
        let typ_ast = quote 0 typ_value in
        Result.map (List.cons (Function { ident; typ = typ_ast; body = body_ast }))
        $ go rest)
    | Constant { ident; typ; body } :: rest ->
      let* ty, value =
        Result.map_error singleton
        $
        let vty = eval typ in
        let* body_ast = check body vty in
        Ok (vty, body_ast)
      in
      let value = eval value in
      Env.local_typed ident ~ty ~value (fun () ->
        let typ = quote 0 ty in
        Result.map (List.cons (Constant { ident; typ; body })) $ go rest)
    | record :: rest -> Result.map (List.cons record) (go rest)
    | [] -> Ok []
  in
  let result, gen =
    Meta.handle meta_gen (fun () ->
      Solve.Solution.handle meta_gen (fun () ->
        let program, constraints = Solve.Constraints.handle $ fun () -> go program in
        match program with
        | Ok program ->
          (match Solve.solve constraints with
           | Solve.Stuck { errors; _ } when errors <> [] -> Error errors
           | _ -> Ok program)
        | Error es -> Error es))
  in
  Result.map (Tuple.intoRev gen) result
;;
