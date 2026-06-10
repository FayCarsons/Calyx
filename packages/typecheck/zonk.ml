open Core

let rec zonk : Term.t -> Term.t = function
  | `Meta m ->
    (match m.solution with
     | Some solution -> Checker.quote 0 (zonk_value solution)
     | None -> `Meta m)
  | `App (f, x) -> `App (zonk f, zonk x)
  | `Lam (plicity, x, body) -> `Lam (plicity, x, zonk body)
  | `Pi { plicity; ident; dom; cod } ->
    let dom = zonk dom in
    let cod = zonk cod in
    `Pi Term.{ plicity; ident; dom; cod }
  | `Let (ident, ty, value, body) ->
    let ty = Option.map ~f:zonk ty in
    let value = zonk value in
    let body = zonk body in
    `Let (ident, ty, value, body)
  | `Ann (x, t) -> `Ann (zonk x, zonk t)
  | `Lit lit -> `Lit (zonk_lit zonk lit)
  | `Proj (tm, field) -> `Proj (zonk tm, field)
  | `Match (scrut, arms) ->
    let scrut = zonk scrut in
    let arms = List.map arms ~f:(fun (p, e) -> p, zonk e) in
    `Match (scrut, arms)
  | `Pos (p, tm) -> `Pos (p, zonk tm)
  | `Self (x, body) -> `Self (x, zonk body)
  | `Infix { left; op; right } ->
    let left = zonk left in
    let op = zonk op in
    let right = zonk right in
    `Infix Term.{ left; op; right }
  | `RecordType { fields; tail } ->
    let fields = Map.map fields ~f:zonk in
    let tail = Option.map ~f:zonk tail in
    (`RecordType Term.{ fields; tail } : Term.t)
  | term -> term

and zonk_lit : type a. (a -> a) -> a Term.literal -> a Term.literal =
  fun f -> function
  | Record fields -> Term.Record (Map.map fields ~f)
  | lit -> lit

and zonk_value : Term.value -> Term.value = function
  | `Neutral (NMeta m) ->
    (match m.solution with
     | Some solution -> zonk_value solution
     | None -> `Neutral (Term.NMeta m))
  | `App (f, x) -> `App (zonk_value f, zonk_value x)
  | `Lam (plicity, x, body) ->
    let var = `Neutral (Term.NVar (0, Ident.Intern.underscore)) in
    let body = zonk_value (Context.lift_r (body var)) in
    `Lam (plicity, x, Fun.const (Ok body))
  | `Pi (plicity, x, dom, cod) ->
    let dom = zonk_value dom in
    let var = `Neutral (Term.NVar (0, Ident.Intern.underscore)) in
    let cod = zonk_value (Context.lift_r (cod var)) in
    `Pi (plicity, x, dom, Fun.const (Ok cod))
  | `Self (x, body) ->
    let var = `Neutral (Term.NVar (0, x)) in
    let body = zonk_value (Context.lift_r (body var)) in
    `Self (x, Fun.const (Ok body))
  | `Ann (x, t) -> `Ann (zonk_value x, zonk_value t)
  | `Lit lit -> `Lit (zonk_lit zonk_value lit)
  | `Proj (tm, field) -> `Proj (zonk_value tm, field)
  | `Match (scrut, arms) ->
    let scrut = zonk_value scrut in
    let arms = List.map arms ~f:(fun (p, e) -> p, zonk_value e) in
    `Match (scrut, arms)
  | `RecordType { fields; tail } ->
    let fields = Map.map fields ~f:zonk_value in
    let tail =
      match tail with
      | Some tail ->
        (match zonk_value tail with
         (* Unsolved row variable - close the record *)
         | `Neutral (NMeta _) -> None
         (* Solved to another record type - flatten/merge *)
         | `RecordType inner ->
           let fields =
             Map.merge_skewed fields inner.fields ~combine:(fun ~key:_ _ v -> v)
           in
           (Some (`RecordType { fields; tail = inner.tail }) : Term.value option)
         | other -> Some other)
      | None -> None
    in
    (`RecordType { fields; tail } : Term.value)
  | t -> t
;;

let zonk_toplevel : Term.t Term.declaration -> Term.t Term.declaration =
  let open Term in
  function
  | Function { ident; typ; body; position } ->
    let typ = zonk typ in
    let body = zonk body in
    Function { ident; typ; body; position }
  | Constant { ident; typ; body; position } ->
    let typ = zonk typ in
    let body = zonk body in
    Constant { ident; typ; body; position }
  | other -> other
;;

let%test_module "zero-cost representation" =
  (module struct
    (* Encodings are conversion-only: after inference, solving and zonking, no
       [`Self] node and no "$"-prefixed encoding binder may remain anywhere in
       the elaborated program. This is what guarantees codegen only ever sees
       nominal heads. *)
    let%test_unit "zonked programs contain no encoding artifacts" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make
           ~count:150
           ~name:"no-encoding-leakage"
           Testgen.arb_adt_with_depth
      @@ fun (spec, depth) ->
      let result, state =
        Context.run ~bindings:Testgen.stdlib (fun () ->
          let inferred = Checker.infer_toplevel (Testgen.program spec depth) in
          Solve.solve ();
          List.map ~f:zonk_toplevel inferred)
      in
      match result with
      | Ok zonked ->
        List.is_empty state.Context.errors && List.for_all zonked ~f:Testgen.decl_clean
      | Error _ -> false
    ;;
  end)
;;
