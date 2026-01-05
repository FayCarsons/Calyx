open Util

let rec zonk : Term.ast -> Term.ast = function
  | `Meta m ->
    (match Solve.Solution.solution m with
     | Some v -> Checker.quote 0 $ zonk_value v
     | None -> `Meta m)
  | `App (f, x) -> `App (zonk f, zonk x)
  | `Lam (x, body) -> `Lam (x, zonk body)
  | `Pi (x, dom, cod) -> `Pi (x, zonk dom, zonk cod)
  | `Let (ident, ty, value, body) ->
    `Let (ident, Option.map zonk ty, zonk value, zonk body)
  | `Ann (x, t) -> `Ann (zonk x, zonk t)
  | `Lit lit -> `Lit (zonk_lit lit)
  | `Proj (tm, field) -> `Proj (zonk tm, field)
  | `Match (scrut, arms) -> `Match (zonk scrut, List.map (fun (p, e) -> p, zonk e) arms)
  | `Pos (p, tm) -> `Pos (p, zonk tm)
  | `Infix { left; op; right } ->
    `Infix { left = zonk left; op = zonk op; right = zonk right }
  | t -> t

and zonk_lit lit = Term.over_literal zonk lit

and zonk_value : Term.value -> Term.value = function
  | `Neutral (NMeta m) ->
    (match Solve.Solution.solution m with
     | Some v -> zonk_value v
     | None -> `Neutral (NMeta m))
  | `App (f, x) -> `App (zonk_value f, zonk_value x)
  | `Lam (x, body) -> `Lam (x, Fun.compose zonk_value body)
  | `Pi (x, dom, cod) -> `Pi (x, zonk_value dom, Fun.compose zonk_value cod)
  | `Ann (x, t) -> `Ann (zonk_value x, zonk_value t)
  | `Lit lit -> `Lit (Term.over_literal zonk_value lit)
  | `Proj (tm, field) -> `Proj (zonk_value tm, field)
  | `Match (scrut, arms) ->
    `Match (zonk_value scrut, List.map (fun (p, e) -> p, zonk_value e) arms)
  | `Row r -> `Row (zonk_row r)
  | `Rec v -> `Rec (zonk_value v)
  | `RowNil -> `RowNil
  | t -> t

and zonk_row : Term.row -> Term.row =
  fun row ->
  { fields = List.map (fun (l, v) -> l, zonk_value v) row.fields
  ; tail = zonk_value row.tail
  }
;;

let zonk_toplevel : Term.ast Term.declaration -> Term.ast Term.declaration = function
  | Function { ident; typ; body } ->
    let typ = zonk typ
    and body = zonk body in
    Function { ident; typ; body }
  | Constant { ident; typ; body } ->
    let typ = zonk typ
    and body = zonk body in
    Constant { ident; typ; body }
  | record -> record
;;
