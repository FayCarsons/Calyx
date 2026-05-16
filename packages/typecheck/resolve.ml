(** This is where we will handle building a dependency graph from and inserting 
    implicit pi terms into the ast pre-inference. 

    For instance, with type Option:
    ```calyx
    data Option (a : Type) where 
    | Some a 
    | None
    ```

    `Option` itself is given the signature `Type -> Type`
    `Some` is given the signature `{a : Type} -> a -> Option a`
    `None` is given the signature `{a : Type} -> Option a`

    And for functions we add implicit lambdas:
    ```calyx
    def map {t, a, b} (m : t a) (f : a -> b) -> t a 
    ```

    Where some signature is reified as `{t a b : Type} -> t a -> (a -> b) -> t b`,
    Its body becomes `\{t} {a} {b} m f -> ...` where `{a}` is an 'implicit lambda',
    a parameter which is inferred and eventually totally erased
*)

open Term
open Core
module IdentSet = Set.Make (Ident)

module InsertImplicits = struct
  (** This transformation matches up implicit Pi types, like `{a : Type} -> ...` to lambdas, 
      ensuring that they share the same number of implicit arguments *)

  (* Extract the plicity spine from a type - sequence of (plicity, ident) from nested Pis *)
  let rec pi_spine : t -> (plicity * Ident.t) list = function
    | `Pi { plicity; ident; cod; _ } -> (plicity, ident) :: pi_spine cod
    | `Pos (_, t) -> pi_spine t
    | _ -> []
  ;;

  (* Extract the plicity spine from a lambda - sequence of (plicity, ident) from nested Lams *)
  let rec lam_spine : t -> (plicity * Ident.t) list = function
    | `Lam (plicity, ident, body) -> (plicity, ident) :: lam_spine body
    | `Pos (_, t) -> lam_spine t
    | _ -> []
  ;;

  (* Find implicit parameters at the front of the type spine that are missing from the term spine.
   For `{a : Type} -> a -> a` with `\x -> x`, returns [a] since the implicit is missing. *)
  let rec find_missing_implicits type_spine term_spine =
    match type_spine, term_spine with
    | (Implicit, _) :: type_rest, (Implicit, _) :: term_rest ->
      (* Both have implicit at front, consume both *)
      find_missing_implicits type_rest term_rest
    | (Implicit, ident) :: type_rest, _ ->
      (* Type has implicit but term doesn't *)
      ident :: find_missing_implicits type_rest term_spine
    | (Explicit, _) :: type_rest, (Explicit, _) :: term_rest ->
      (* Both explicit, continue matching *)
      find_missing_implicits type_rest term_rest
    | _ -> []
  ;;

  (* Insert implicit lambdas at the front of a term based on the type's Pi spine *)
  let run (typ : t) (term : t) : t =
    let type_spine = pi_spine typ in
    let term_spine = lam_spine term in
    let missing = find_missing_implicits type_spine term_spine in
    List.fold_right missing ~init:term ~f:(fun ident acc -> `Lam (Implicit, ident, acc))
  ;;
end

type dependencies = IdentSet.t Ident.Map.t

type traversal =
  { scope : IdentSet.t
  ; dependencies : dependencies
  ; toplevel : Ident.t
  }

let empty =
  { scope = IdentSet.empty
  ; dependencies = Ident.Map.empty
  ; toplevel = Ident.Intern.underscore
  }
;;

let merge : traversal -> traversal -> traversal =
  fun a b ->
  let scope = Set.union a.scope b.scope
  and dependencies =
    Map.merge a.dependencies b.dependencies ~f:(fun ~key:_ entry ->
      match entry with
      | `Left x | `Right x -> Some x
      | `Both (a, b) -> Option.some @@ Set.union a b)
  in
  { scope; dependencies; toplevel = a.toplevel }
;;

let traverse_map
  :  (traversal -> 'a -> 'a * traversal)
  -> traversal
  -> 'a Ident.Map.t
  -> 'a Ident.Map.t * traversal
  =
  fun go state map ->
  let state, alist =
    Map.to_alist map
    |> List.fold_map ~init:state ~f:(fun state (k, v) ->
      let v, state = go state v in
      state, (k, v))
  in
  Ident.Map.of_alist_exn alist, state
;;

let resolve_expr : traversal -> t -> t * traversal =
  fun state tree ->
  let rec go ({ scope; dependencies; toplevel } as state) : t -> t * traversal = function
    | `Var ident ->
      let state =
        if Set.mem scope ident
        then state
        else (
          let dependencies =
            Map.update dependencies toplevel ~f:(function
              | Some deps -> Set.add deps ident
              | None -> IdentSet.singleton ident)
          in
          { state with dependencies })
      in
      `Var ident, state
    | `App (f, x) ->
      let f, statef = go state f
      and x, statex = go state x in
      `App (f, x), merge statef statex
    | `Infix { left; op; right } ->
      let left, state_left = go state left
      and op, state_op = go state op
      and right, state_right = go state right in
      `Infix { left; op; right }, merge state_left (merge state_op state_right)
    | `Ann (x, t) ->
      let x, statex = go state x
      and t, statet = go state t in
      `Ann (x, t), merge statex statet
    | `Lit l ->
      let l, state = literal state l in
      `Lit l, state
    | `Proj (tm, field) ->
      let tm, state = go state tm in
      `Proj (tm, field), state
    | `Match (scrut, arms) ->
      let scrut, state_scrut = go state scrut
      and state_arms, arms =
        List.fold_map arms ~init:state ~f:(fun state (pattern, expr) ->
          let expr, state = go state expr in
          state, (pattern, expr))
      in
      `Match (scrut, arms), merge state_scrut state_arms
    | `Pos (p, term) ->
      let term, state = go state term in
      `Pos (p, term), state
    | `Lam (plicity, x, body) ->
      let new_scope = Set.add scope x in
      let body, state = go { state with scope = new_scope } body in
      `Lam (plicity, x, body), { state with scope }
    | `Pi { plicity; ident; dom; cod } ->
      let new_scope =
        if Ident.equal ident Ident.Intern.underscore then scope else Set.add scope ident
      in
      let dom, state_dom = go state dom
      and cod, state_cod = go { state with scope = new_scope } cod in
      let state = merge state_dom state_cod in
      `Pi { plicity; ident; dom; cod }, { state with scope }
    | `Let (ident, typ, value, body) ->
      let new_scope = Set.add scope ident in
      let typ, state_typ =
        match typ with
        | Some typ ->
          let typ, state = go state typ in
          Some typ, state
        | None -> None, state
      in
      (* Insert implicit lambdas if we have a type annotation *)
      let value =
        match typ with
        | Some typ -> InsertImplicits.run typ value
        | None -> value
      in
      let value, state_value = go { state with scope = new_scope } value in
      let body, state_body = go { state with scope = new_scope } body in
      let new_state = merge state_typ (merge state_value state_body) in
      `Let (ident, typ, value, body), { new_state with scope }
    | `RecordType { fields; tail } ->
      let fields, state = traverse_map go state fields in
      let tail, state =
        match tail with
        | Some t ->
          let t, state = go state t in
          Some t, state
        | None -> None, state
      in
      `RecordType { fields; tail }, state
    | `SumType { ident; params; constructors; position } ->
      let params, state = traverse_map go state params in
      let constructors, state =
        traverse_map
          (fun state args ->
             let state, args =
               List.fold_map args ~init:state ~f:(fun state ty ->
                 let ty, state = go state ty in
                 state, ty)
             in
             args, state)
          state
          constructors
      in
      `SumType { ident; params; constructors; position }, state
    | other -> other, state
  and literal state = function
    | Int n -> Int n, state
    | UInt n -> UInt n, state
    | Float x -> Float x, state
    | Bool b -> Bool b, state
    | Record fields ->
      let state, fields =
        Map.to_alist fields
        |> List.fold_map ~init:state ~f:(fun state (k, v) ->
          let v, state = go state v in
          state, (k, v))
      in
      let fields = Ident.Map.of_alist_exn fields in
      Record fields, state
  in
  let tree, state = go state tree in
  tree, state
;;

let resolve_toplevel : traversal -> t declaration -> t declaration * traversal =
  fun state decl ->
  match decl with
  | Function { ident; typ; body; position } ->
    let new_state = { state with toplevel = ident; scope = Set.add state.scope ident } in
    let typ, state_typ = resolve_expr new_state typ in
    (* Insert implicit lambdas based on the type signature *)
    let body = InsertImplicits.run typ body in
    let body, state_body = resolve_expr new_state body in
    let state = merge state_typ state_body in
    let state = { state with scope = state.scope } in
    Function { ident; typ; body; position }, state
  | _ -> decl, state
;;

let resolve_program : t declaration list -> t declaration list * dependencies =
  fun decls ->
  let init_state = empty in
  let state, resolved =
    List.fold_map decls ~init:init_state ~f:(fun state decl ->
      let decl', state' = resolve_toplevel state decl in
      state', decl')
  in
  resolved, state.dependencies
;;
