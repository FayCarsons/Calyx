(** This is where we will handle building a dependency graph from and inserting 
    implicit pi terms into the ast pre-inference. 
*)

open Term
open Core
module IdentSet = Set.Make (Ident)

type traversal =
  { scope : IdentSet.t
  ; dependencies : IdentSet.t Ident.Map.t
  ; toplevel : Ident.t
  }

let empty toplevel = { scope = IdentSet.empty; dependencies = Ident.Map.empty; toplevel }

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

let resolve_expr : traversal -> ast -> ast * traversal =
  fun state tree ->
  let rec go ({ scope; dependencies; toplevel } as state) : ast -> ast * traversal
    = function
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

let resolve_toplevel : traversal -> ast declaration -> ast declaration * traversal =
  fun state decl ->
  match decl with
  | Function { ident; typ; body; position } ->
    let new_state = { state with toplevel = ident; scope = Set.add state.scope ident } in
    let typ, state_typ = resolve_expr new_state typ
    and body, state_body = resolve_expr new_state body in
    let state = merge state_typ state_body in
    let state = { state with scope = state.scope } in
    Function { ident; typ; body; position }, state
  | _ -> decl, state
;;
