open Core
open Context.Syntax
open Term
module Meta = Meta

let force : value -> value Context.t =
  fun value ->
  (* NOTE: Fetching the context here places the rest of the function inside 
     the Context.t monad, which is necessary otherwise we have some 
     heisenbug where meta state can become stale
  *)
  let* ctx = Context.ask in
  let rec go v =
    match v with
    | `Neutral (NMeta m) ->
      (match m.Meta.solution with
       | Some solution -> go solution
       | None -> v)
    | `Neutral (NVar (_, ident)) as var ->
      (* Look up variable, but only follow Var/NVar chains.
               For any other value type, return the original var. *)
      let bound =
        match Map.find ctx.bindings ident with
        | Some (Context.Typed (v', _)) -> Some v'
        | Some (Context.Untyped v') -> Some v'
        | None -> None
      in
      (match bound with
       | Some (`Var ident' | `Neutral (NVar (_, ident'))) when Ident.equal ident ident' ->
         (* Self-reference, return original *)
         var
       | Some ((`Var _ | `Neutral (NVar _)) as v') ->
         (* Another var/nvar, follow the chain *)
         go v'
       | Some `Opaque ->
         (* Opaque type, return original *)
         var
       | _ ->
         (* Any other value or not found, return original var *)
         var)
    | v -> v
  in
  Context.pure @@ go value
;;

let rec occurs (m : Meta.t) (v : value) : bool Context.t =
  let open Context in
  let open Syntax in
  force v
  >>= function
  | `Neutral (NMeta m') -> pure @@ Meta.equal m m'
  | `Neutral (NApp (f, x)) -> pure ( || ) <*> occurs m x <*> occurs_neutral m f
  | `Neutral (NProj (n, _)) -> occurs_neutral m n
  | `Pi (_, _, dom, cod) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* cod = lift_r @@ cod var in
    pure ( || ) <*> occurs m dom <*> occurs m cod
  | `Lam (_, _, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* body = lift_r @@ body var in
    occurs m body
  | `RecordType { fields; tail } ->
    let* in_fields =
      Map.data fields |> traverse ~f:(occurs m) |> map ~f:(List.exists ~f:Fun.id)
    in
    let* in_tail = Option.value ~default:(pure false) (Option.map tail ~f:(occurs m)) in
    pure (in_fields || in_tail)
  | `Lit lit -> occurs_lit m lit
  | _ -> pure false

and occurs_neutral m =
  let open Context in
  let open Syntax in
  function
  | NMeta m' -> pure @@ Meta.equal m m'
  | NApp (f, x) -> pure ( || ) <*> occurs_neutral m f <*> occurs m x
  | NProj (n, _) -> occurs_neutral m n
  | _ -> pure false

and occurs_lit m = function
  | Record fields ->
    Map.data fields
    |> Context.traverse ~f:(occurs m)
    |> Context.map ~f:(List.exists ~f:Fun.id)
  | _ -> Context.pure false
;;

let solve_meta (m : Meta.t) (v : value) : unit Context.t =
  let open Context in
  let open Syntax in
  occurs m v
  >>= function
  | true -> Context.fail (`Occurs (Meta.show m))
  | false ->
    Meta.solve m v;
    Context.pure ()
;;

let rec unify : value -> value -> unit Context.t =
  fun a b ->
  let open Context in
  let open Syntax in
  let* a = force a in
  let* b = force b in
  match a, b with
  | `Var l, `Var r ->
    if Ident.equal l r
    then pure ()
    else tell_error (`UnificationFailure (Ident.Intern.lookup l, Ident.Intern.lookup r))
  | `Neutral (NMeta m1), `Neutral (NMeta m2) when Meta.equal m1 m2 -> pure ()
  | `Neutral (NMeta m), v | v, `Neutral (NMeta m) -> solve_meta m v
  | `Type, `Type -> pure ()
  | `Lam (_, _, body1), `Lam (_, _, body2) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* body1 = lift_r (body1 var) in
    let* body2 = lift_r (body2 var) in
    Context.tell_constraint @@ Constraint.equals body1 body2
  | `Lam (_, _, body), f | f, `Lam (_, _, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* right = vapp f var in
    let* body = lift_r (body var) in
    Context.tell_constraint @@ Constraint.equals body right
  | `Pi (_, _, dom, cod), `Pi (_, _, dom', cod') ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* _ = Context.tell_constraint Constraint.(equals dom dom') in
    let* cod = lift_r @@ cod var in
    let* cod' = lift_r @@ cod' var in
    Context.tell_constraint Constraint.(equals cod cod')
  | (`Neutral (NVar (_, name)), `Var name' | `Var name, `Neutral (NVar (_, name')))
    when Ident.equal name name' -> pure ()
  | `Neutral (NVar (l_level, l_name)), `Neutral (NVar (r_level, r_name)) ->
    if Int.equal l_level r_level && Ident.equal l_name r_name
    then pure ()
    else
      fail
        (`UnificationFailure
            ( Sexp.to_string_hum @@ Term.sexp_of_neutral (NVar (l_level, l_name))
            , Sexp.to_string_hum @@ Term.sexp_of_neutral (NVar (r_level, r_name)) ))
  | `Neutral l, `Neutral r -> unify_neutral l r
  | `Lit l, `Lit r -> unify_lit l r
  | `RecordType a, `RecordType b -> unify_record_types a b
  | `SumType a, `SumType b -> unify_sum_types a b
  (* A neutral variable referring to a sum type by name *)
  | `Neutral (NVar (_, name)), `SumType { ident; _ }
  | `SumType { ident; _ }, `Neutral (NVar (_, name))
    when Ident.equal name ident -> pure ()
  | `Err _, _ | _, `Err _ -> pure ()
  | a, b ->
    fail
      (`UnificationFailure
          ( Sexp.to_string_hum @@ Term.sexp_of_value a
          , Sexp.to_string_hum @@ Term.sexp_of_value b ))

and unify_neutral : neutral -> neutral -> unit Context.t =
  fun l r ->
  let open Context in
  let open Syntax in
  match l, r with
  | NVar (l_level, l_name), NVar (r_level, r_name)
    when Int.equal l_level r_level && Ident.equal l_name r_name -> pure ()
  | NApp (f, x), NApp (f', x') ->
    let* _ = unify_neutral f f' in
    Context.tell_constraint Constraint.(equals x x')
  | NProj (tm, field), NProj (tm', field') when Ident.equal field field' ->
    unify_neutral tm tm'
  (* Neutrals mismatch*)
  | l, r ->
    fail
      (`UnificationFailure
          ( Sexp.to_string_hum @@ Term.sexp_of_neutral l
          , Sexp.to_string_hum @@ Term.sexp_of_neutral r ))

and unify_lit l r =
  match l, r with
  | (Int a, Int b | UInt a, UInt b) when Int.equal a b -> Context.pure ()
  | Float a, Float b when Float.equal a b -> Context.pure ()
  | Bool a, Bool b when Bool.equal a b -> Context.pure ()
  | Record l, Record r -> unify_record_literals l r
  | _, _ -> Context.fail `Todo

and unify_record_literals : value Ident.Map.t -> value Ident.Map.t -> unit Context.t =
  fun a b ->
  if Set.equal (Map.key_set a) (Map.key_set b)
  then (
    let a' = Map.to_alist a
    and b' = Map.to_alist b in
    (* FIXME: Instead of relying on 'unify' to emit an error, we should make
       this whole check fallible and, on failure, emit a more descriptive
       "these" records don't match error *)
    List.zip_exn a' b'
    |> Context.traverse ~f:(fun ((_, a), (_, b)) -> unify a b)
    |> Context.map ~f:(Fun.const ()))
  else
    Context.fail
    @@ `Expected (Ident.Map.show Term.show_value a, Ident.Map.show Term.show_value b)

and unify_sum_types : value sum_type -> value sum_type -> unit Context.t =
  fun a b ->
  if Ident.equal a.ident b.ident
  then
    let* _ =
      Map.fold2
        a.constructors
        b.constructors
        ~init:(Context.pure ())
        ~f:(fun ~key:_ ~data acc ->
          let* () = acc in
          match data with
          | `Both (args_a, args_b) ->
            List.zip_exn args_a args_b
            |> Context.traverse ~f:(fun (a, b) ->
              Context.tell_constraint (Constraint.equals a b))
            >|= Fun.const ()
          | _ -> Context.pure ())
    in
    Context.pure ()
  else
    Context.fail
      (`UnificationFailure
          ( Printf.sprintf "SumType %s" (Ident.Intern.lookup a.ident)
          , Printf.sprintf "SumType %s" (Ident.Intern.lookup b.ident) ))

and unify_record_types : value row -> value row -> unit Context.t =
  fun a b ->
  let open Context.Syntax in
  let go field =
    match Map.find a.fields field, Map.find b.fields field with
    | Some a, Some b -> unify a b
    | None, Some x ->
      (match a.tail with
       | Some tail ->
         Context.tell_constraint
           (Constraint.has_field ~record:tail ~field_name:field ~field_type:x)
       | None ->
         Context.fail
           (`NoField
               ( field
               , List.map ~f:(Util.Tuple.second Term.show_value) @@ Map.to_alist a.fields
               )))
    | Some x, None ->
      (match b.tail with
       | Some tail ->
         Context.tell_constraint
           (Constraint.has_field ~record:tail ~field_name:field ~field_type:x)
       | None ->
         Context.fail
           (`NoField
               ( field
               , List.map ~f:(Util.Tuple.second Term.show_value) @@ Map.to_alist a.fields
               )))
    | None, None -> (* Unreachable *) Context.pure ()
  in
  let* _ =
    Context.traverse ~f:go
    @@ Set.to_list
    @@ Set.union (Map.key_set a.fields) (Map.key_set b.fields)
  in
  match a.tail, b.tail with
  | None, None -> Context.pure ()
  | Some a, Some b -> unify a b
  | _ ->
    Context.fail
      (`Expected
          ( "two record types which are either both open, or both closed and equal"
          , Printf.sprintf
              "%s and %s"
              (Term.show_row Term.pp_value a)
              (Term.show_row Term.pp_value b) ))

and vapp =
  fun f x ->
  match f with
  | `Lam (_, _, body) -> Context.lift_r @@ body x
  | `Neutral n -> Context.pure @@ `Neutral (NApp (n, x))
  | otherwise -> Context.fail (`Expected ("function", Term.show_value otherwise))
;;

type solver_error =
  | Stuck of
      { stuck : Constraint.t list
      ; errors : Calyx_error.t list
      }
  | Errors of Calyx_error.t list
[@@deriving show, sexp]

let pretty_solver_error = function
  | Stuck { stuck; errors = _ :: _ as es } ->
    Printf.sprintf
      "Could not solve constraints:\n%s\nWith errors:\n%s\n"
      (String.concat ~sep:", " @@ List.map ~f:Constraint.show stuck)
      (String.concat ~sep:", " @@ List.map ~f:Calyx_error.show es)
  | Stuck { stuck; _ } ->
    Printf.sprintf
      "Could not solve constraints:\n%s\n"
      (String.concat ~sep:", " @@ List.map ~f:Constraint.show stuck)
  | Errors es ->
    Printf.sprintf
      "Failed to solve due to errors:\n%s\n"
      (String.concat ~sep:",\n" @@ List.map ~f:Calyx_error.show es)
;;

let rec solve : unit -> unit Context.t =
  fun () ->
  let open Context.Syntax in
  let* constraints = Context.take_constraints in
  let progressed =
    Context.map ~f:(List.exists ~f:Fun.id) (Context.traverse ~f:solve_one constraints)
  in
  progressed
  >>= function
  | true ->
    let* has_constraints = Context.has_constraints in
    if has_constraints then solve () else Context.pure ()
  | false ->
    let* constraints = Context.take_constraints in
    Context.fail
      (`Stuck
          (List.map ~f:(Fun.compose Sexp.to_string_hum Constraint.sexp_of_t) constraints
           |> String.concat ~sep:"\n"))

and solve_one : Constraint.t -> bool Context.t = function
  | Equals (a, b) -> unify a b |> Context.map ~f:(Fun.const true)
  | Subtype { sub; super } ->
    (* Printf.printf "SOLVING SUBTYPE '%s' :> '%s'" (Term.show_value a) (Term.show_value b); *)
    subsumes ~sub ~super
  | HasField (record, field_name, field_type) ->
    solve_record ~record ~field_name ~field_type

and solve_record
  : record:value -> field_name:Ident.t -> field_type:value -> bool Context.t
  =
  fun ~record ~field_name ~field_type ->
  let open Context.Syntax in
  force record
  >>= function
  | `Lit (Record fields) ->
    let* _ =
      match Map.find fields field_name with
      | Some f -> Context.tell_constraint (Constraint.equals field_type f)
      | None -> Context.pure ()
    in
    Context.pure true
  | `RecordType { fields; tail } ->
    (match Map.find fields field_name with
     | Some existing_type ->
       Context.tell_constraint (Constraint.equals field_type existing_type)
       >|= Fun.const true
     | None ->
       (match tail with
        | Some tail_val ->
          Context.tell_constraint
            (Constraint.has_field ~record:tail_val ~field_name ~field_type)
          >|= Fun.const true
        | None ->
          let* _ =
            Context.tell_error
            @@ `NoField
                 ( field_name
                 , List.map ~f:(Util.Tuple.second Term.show_value) @@ Map.to_alist fields
                 )
          in
          Context.pure true))
  | `Neutral (NMeta m) ->
    (* Solve meta to a record type with this field and an open tail *)
    let* meta = Context.fresh_meta in
    let fresh_tail = `Neutral (NMeta meta) in
    let record_type : Term.value =
      `RecordType
        { fields = Ident.Map.singleton field_name field_type; tail = Some fresh_tail }
    in
    let* _ = solve_meta m record_type in
    Context.pure true
  | _ ->
    let* _ = Context.tell_error @@ `Expected ("record", Term.show_value record) in
    Context.pure true

and subsumes ~sub ~super =
  let open Continue_or_stop in
  let open Context.Syntax in
  match sub, super with
  | `RecordType sub, `RecordType super ->
    let* fields_match : bool =
      Map.fold_until
        super.fields
        ~init:(Context.pure true)
        ~finish:Fun.id
        ~f:(fun ~key ~data:super _ ->
          match Map.find sub.fields key with
          | Some sub ->
            Continue
              (Context.tell_constraint (Constraint.subtype ~sub ~super) >|= Fun.const true)
          | None ->
            (match sub.tail with
             | Some tail ->
               Continue
                 (Context.tell_constraint
                    (Constraint.has_field ~record:tail ~field_name:key ~field_type:super)
                  >|= Fun.const true)
             | None -> Stop (Context.pure false)))
    in
    (match sub.tail, super.tail with
     | _, None -> Context.pure fields_match
     | None, Some _ -> Context.pure false
     | Some sub, Some super ->
       Context.tell_constraint (Constraint.subtype ~sub ~super) >|= Fun.const true)
  (* NOTE: This is awkward and I am very suspicious of it *)
  | _ -> Context.tell_constraint (Constraint.equals sub super) >|= Fun.const true
;;

let%test "'a is subtype of 'a" =
  let open Ident in
  let a : Term.value =
    `RecordType
      { fields =
          Map.of_alist_exn
            [ Intern.intern "x", `Var (Intern.intern "Int")
            ; Intern.intern "y", `Var (Intern.intern "Int")
            ]
      ; tail =
          Some
            (`RecordType
                { fields =
                    Map.of_alist_exn [ Intern.intern "z", `Var (Intern.intern "Int") ]
                ; tail = None
                })
      }
  in
  Context.start @@ subsumes ~sub:a ~super:a
  |> fst
  |> function
  | Ok _ -> true
  | Error es ->
    Printf.printf
      "Failed: '%s'"
      (Sexp.to_string_hum @@ List.sexp_of_t Calyx_error.sexp_of_t es);
    false
;;
