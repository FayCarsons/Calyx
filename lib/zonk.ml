open Core
open Context.Syntax

let rec zonk : Term.t -> Term.t Context.t = function
  | `Meta m ->
    (match m.solution with
     | Some solution -> zonk_value solution >>= Checker.quote 0
     | None -> Context.pure (`Meta m))
  | `App (f, x) ->
    let* f = zonk f in
    let* x = zonk x in
    Context.pure @@ `App (f, x)
  | `Lam (plicity, x, body) ->
    let* body = zonk body in
    Context.pure @@ `Lam (plicity, x, body)
  | `Pi { plicity; ident; dom; cod } ->
    let* dom = zonk dom in
    let* cod = zonk cod in
    Context.pure @@ `Pi Term.{ plicity; ident; dom; cod }
  | `Let (ident, ty, value, body) ->
    let* ty =
      match ty with
      | Some ty -> zonk ty >|= Option.some
      | None -> Context.pure None
    in
    let* value = zonk value in
    let* body = zonk body in
    Context.pure @@ `Let (ident, ty, value, body)
  | `Ann (x, t) ->
    let* x = zonk x in
    let* t = zonk t in
    Context.pure @@ `Ann (x, t)
  | `Lit lit ->
    let* lit = zonk_lit zonk lit in
    Context.pure @@ `Lit lit
  | `Proj (tm, field) ->
    let* tm = zonk tm in
    Context.pure @@ `Proj (tm, field)
  | `Match (scrut, arms) ->
    let* scrut = zonk scrut in
    let* arms =
      Context.traverse ~f:(fun (p, e) -> Context.map (zonk e) ~f:(Tuple2.create p)) arms
    in
    Context.pure @@ `Match (scrut, arms)
  | `Pos (p, tm) ->
    let* tm = zonk tm in
    Context.pure @@ `Pos (p, tm)
  | `Infix { left; op; right } ->
    let* left = zonk left in
    let* op = zonk op in
    let* right = zonk right in
    Context.pure @@ `Infix Term.{ left; op; right }
  | `RecordType { fields; tail } ->
    let* fields = Context.traverse_map fields ~f:zonk in
    let* tail =
      match tail with
      | Some t -> zonk t >|= Option.some
      | None -> Context.pure None
    in
    Context.pure @@ (`RecordType Term.{ fields; tail } : Term.t)
  | `SumType { ident; params; constructors; position } ->
    let* params = Context.traverse_map params ~f:zonk in
    let* constructors = Context.traverse_map constructors ~f:(Context.traverse ~f:zonk) in
    Context.pure @@ `SumType Term.{ ident; params; constructors; position }
  | term -> Context.pure term

and zonk_lit : type a. (a -> a Context.t) -> a Term.literal -> a Term.literal Context.t =
  fun f -> function
  | Record fields ->
    let* fields = Context.traverse_map fields ~f in
    Context.pure (Term.Record fields)
  | lit -> Context.pure lit

and zonk_value : Term.value -> Term.value Context.t = function
  | `Neutral (NMeta m) ->
    (match m.solution with
     | Some solution -> zonk_value solution
     | None -> Context.pure @@ `Neutral (Term.NMeta m))
  | `App (f, x) ->
    let* f = zonk_value f in
    let* x = zonk_value x in
    Context.pure @@ `App (f, x)
  | `Lam (plicity, x, body) ->
    let var = `Neutral (Term.NVar (0, Ident.Intern.underscore)) in
    let* body = Context.liftR (body var) >>= zonk_value in
    Context.pure @@ `Lam (plicity, x, Fun.const (Ok body))
  | `Pi (plicity, x, dom, cod) ->
    let* dom = zonk_value dom in
    let var = `Neutral (Term.NVar (0, Ident.Intern.underscore)) in
    let* cod = Context.liftR (cod var) >>= zonk_value in
    Context.pure @@ `Pi (plicity, x, dom, Fun.const (Ok cod))
  | `Ann (x, t) ->
    let* x = zonk_value x in
    let* t = zonk_value t in
    Context.pure @@ `Ann (x, t)
  | `Lit lit ->
    let* lit = zonk_lit zonk_value lit in
    Context.pure @@ `Lit lit
  | `Proj (tm, field) ->
    let* tm = zonk_value tm in
    Context.pure @@ `Proj (tm, field)
  | `Match (scrut, arms) ->
    let* scrut = zonk_value scrut in
    let* arms =
      Context.traverse
        ~f:(fun (p, e) -> Context.map (zonk_value e) ~f:(Tuple2.create p))
        arms
    in
    Context.pure @@ `Match (scrut, arms)
  | `RecordType { fields; tail } ->
    let* fields = Context.traverse_map fields ~f:zonk_value in
    let* tail =
      match tail with
      | Some tail ->
        zonk_value tail
        >>= (function
         (* Unsolved row variable - close the record *)
         | `Neutral (NMeta _) -> Context.pure None
         (* Solved to another record type - flatten/merge *)
         | `RecordType inner ->
           let fields =
             Map.merge_skewed fields inner.fields ~combine:(fun ~key:_ _ v -> v)
           in
           Context.pure
             (Some (`RecordType { fields; tail = inner.tail }) : Term.value option)
         | other -> Context.pure (Some other))
      | None -> Context.pure None
    in
    Context.pure @@ (`RecordType { fields; tail } : Term.value)
  | `SumType { ident; params; constructors; position } ->
    let* params = Context.traverse_map params ~f:zonk_value in
    let* constructors =
      Context.traverse_map constructors ~f:(Context.traverse ~f:zonk_value)
    in
    Context.pure @@ `SumType Term.{ ident; params; constructors; position }
  | t -> Context.pure t
;;

let zonk_toplevel : Term.t Term.declaration -> Term.t Term.declaration Context.t =
  let open Term in
  function
  | Function { ident; typ; body; position } ->
    let* typ = zonk typ in
    let* body = zonk body in
    Context.pure @@ Function { ident; typ; body; position }
  | Constant { ident; typ; body; position } ->
    let* typ = zonk typ in
    let* body = zonk body in
    Context.pure @@ Constant { ident; typ; body; position }
  | other -> Context.pure other
;;
