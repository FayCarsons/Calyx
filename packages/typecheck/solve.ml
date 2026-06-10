open Core
open Term
module Meta = Meta

let force : value -> value =
  fun value ->
  let bindings = Context.bindings () in
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
        match Map.find bindings ident with
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
  go value
;;

let rec occurs (m : Meta.t) (v : value) : bool =
  match force v with
  | `Neutral (NMeta m') -> Meta.equal m m'
  | `Neutral (NApp (f, x)) -> occurs m x || occurs_neutral m f
  | `Neutral (NProj (n, _)) -> occurs_neutral m n
  | `Pi (_, _, dom, cod) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    occurs m dom || occurs m (Context.lift_r (cod var))
  | `Lam (_, _, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    occurs m (Context.lift_r (body var))
  | `Self (_, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    occurs m (Context.lift_r (body var))
  | `RecordType { fields; tail } ->
    List.exists (Map.data fields) ~f:(occurs m) || Option.exists tail ~f:(occurs m)
  | `Lit lit -> occurs_lit m lit
  | _ -> false

and occurs_neutral m = function
  | NMeta m' -> Meta.equal m m'
  | NApp (f, x) -> occurs_neutral m f || occurs m x
  | NProj (n, _) -> occurs_neutral m n
  | _ -> false

and occurs_lit m = function
  | Record fields -> List.exists (Map.data fields) ~f:(occurs m)
  | _ -> false
;;

let solve_meta (m : Meta.t) (v : value) : unit =
  if occurs m v then Context.fail (`Occurs (Meta.show m)) else Meta.solve m v
;;

(* Decompose a neutral into its head variable and application spine. *)
let rec spine : neutral -> (Ident.t * value list) option = function
  | NVar (_, head) -> Some (head, [])
  | NApp (f, x) -> Option.map (spine f) ~f:(fun (head, args) -> head, args @ [ x ])
  | _ -> None
;;

let rec unify : value -> value -> unit =
  fun a b ->
  let a = force a in
  let b = force b in
  match a, b with
  | `Var l, `Var r ->
    if Ident.equal l r
    then ()
    else
      Context.tell_error
        (`UnificationFailure (Ident.Intern.lookup l, Ident.Intern.lookup r))
  | `Neutral (NMeta m1), `Neutral (NMeta m2) when Meta.equal m1 m2 -> ()
  | `Neutral (NMeta m), v | v, `Neutral (NMeta m) -> solve_meta m v
  | `Type, `Type -> ()
  (* Delta unfolding (design/type_system.md §3.3): a nominal datatype or
     constructor head is unfolded to its Scott encoding only when compared
     against a non-neutral, non-meta value. Neutral-vs-neutral comparisons
     stay structural (same head -> args; different heads -> sealed mismatch),
     and metas always solve to the nominal form, never the encoding. *)
  | `Neutral n, other when delta_redex n other ->
    unify (Option.value_exn (unfold n)) other
  | other, `Neutral n when delta_redex n other ->
    unify other (Option.value_exn (unfold n))
  | `Lam (_, _, body1), `Lam (_, _, body2) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let body1 = Context.lift_r (body1 var) in
    let body2 = Context.lift_r (body2 var) in
    Context.tell_constraint @@ Constraint.equals body1 body2
  | `Lam (_, _, body), f | f, `Lam (_, _, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let right = vapp f var in
    let body = Context.lift_r (body var) in
    Context.tell_constraint @@ Constraint.equals body right
  | `Pi (_, _, dom, cod), `Pi (_, _, dom', cod') ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    Context.tell_constraint Constraint.(equals dom dom');
    let cod = Context.lift_r (cod var) in
    let cod' = Context.lift_r (cod' var) in
    Context.tell_constraint Constraint.(equals cod cod')
  | `Self (x, body), `Self (_, body') ->
    let var = `Neutral (NVar (0, x)) in
    let body = Context.lift_r (body var) in
    let body' = Context.lift_r (body' var) in
    Context.tell_constraint Constraint.(equals body body')
  | (`Neutral (NVar (_, name)), `Var name' | `Var name, `Neutral (NVar (_, name')))
    when Ident.equal name name' -> ()
  | `Neutral (NVar (l_level, l_name)), `Neutral (NVar (r_level, r_name)) ->
    if Int.equal l_level r_level && Ident.equal l_name r_name
    then ()
    else
      Context.fail
        (`UnificationFailure
            ( Sexp.to_string_hum @@ Term.sexp_of_neutral (NVar (l_level, l_name))
            , Sexp.to_string_hum @@ Term.sexp_of_neutral (NVar (r_level, r_name)) ))
  | `Neutral l, `Neutral r -> unify_neutral l r
  | `Lit l, `Lit r -> unify_lit l r
  | `RecordType a, `RecordType b -> unify_record_types a b
  | `Err _, _ | _, `Err _ -> ()
  | a, b ->
    Context.fail
      (`UnificationFailure
          ( Sexp.to_string_hum @@ Term.sexp_of_value a
          , Sexp.to_string_hum @@ Term.sexp_of_value b ))

and unify_neutral : neutral -> neutral -> unit =
  fun l r ->
  match l, r with
  | NVar (l_level, l_name), NVar (r_level, r_name)
    when Int.equal l_level r_level && Ident.equal l_name r_name -> ()
  | NApp (f, x), NApp (f', x') ->
    unify_neutral f f';
    Context.tell_constraint Constraint.(equals x x')
  | NProj (tm, field), NProj (tm', field') when Ident.equal field field' ->
    unify_neutral tm tm'
  (* Neutrals mismatch*)
  | l, r ->
    Context.fail
      (`UnificationFailure
          ( Sexp.to_string_hum @@ Term.sexp_of_neutral l
          , Sexp.to_string_hum @@ Term.sexp_of_neutral r ))

and unify_lit l r =
  match l, r with
  | (Int a, Int b | UInt a, UInt b) when Int.equal a b -> ()
  | Float a, Float b when Float.equal a b -> ()
  | Bool a, Bool b when Bool.equal a b -> ()
  | Record l, Record r -> unify_record_literals l r
  | _, _ -> Context.fail `Todo

and unify_record_literals : value Ident.Map.t -> value Ident.Map.t -> unit =
  fun a b ->
  if Set.equal (Map.key_set a) (Map.key_set b)
  then (
    let a' = Map.to_alist a
    and b' = Map.to_alist b in
    (* FIXME: Instead of relying on 'unify' to emit an error, we should make
       this whole check fallible and, on failure, emit a more descriptive
       "these records don't match" error *)
    List.zip_exn a' b' |> List.iter ~f:(fun ((_, a), (_, b)) -> unify a b))
  else
    Context.fail
    @@ `Expected (Ident.Map.show Term.show_value a, Ident.Map.show Term.show_value b)

and unify_record_types : value row -> value row -> unit =
  fun a b ->
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
    | None, None -> (* Unreachable *) ()
  in
  List.iter ~f:go
  @@ Set.to_list
  @@ Set.union (Map.key_set a.fields) (Map.key_set b.fields);
  match a.tail, b.tail with
  | None, None -> ()
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
  | `Neutral n -> `Neutral (NApp (n, x))
  | otherwise -> Context.fail (`Expected ("function", Term.show_value otherwise))

(* One-step delta unfolding: replace a defined head constant with its Scott
   encoding, applied to the spine arguments via NbE. The unfolded body's
   interior references are themselves nominal neutrals, so repeated
   unfolding terminates. *)
and unfold : neutral -> value option =
  fun n ->
  match spine n with
  | None -> None
  | Some (head, args) ->
    (match Context.lookup_defn head with
     | Some (Context.Data { encoding; _ } | Context.Ctor { encoding; _ }) ->
       Some (List.fold args ~init:encoding ~f:vapp)
     | None -> None)

and delta_redex : neutral -> value -> bool =
  fun n other ->
  (match other with
   | `Neutral _ | `Err _ -> false
   | _ -> true)
  && Option.is_some (unfold n)
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

let rec solve : unit -> unit =
  fun () ->
  match Context.take_constraints () with
  | [] -> ()
  | constraints ->
    (* NB: every constraint is attempted (no short-circuit) — [solve_one] may
       tell new constraints as it goes. *)
    let progressed = List.map ~f:solve_one constraints |> List.exists ~f:Fun.id in
    if progressed
    then (if Context.has_constraints () then solve ())
    else (
      let stuck = Context.take_constraints () in
      Context.fail
        (`Stuck
            (List.map ~f:(Fun.compose Sexp.to_string_hum Constraint.sexp_of_t) stuck
             |> String.concat ~sep:"\n")))

and solve_one : Constraint.t -> bool = function
  | Equals (a, b) ->
    unify a b;
    true
  | Subtype { sub; super } -> subsumes ~sub ~super
  | HasField (record, field_name, field_type) ->
    solve_record ~record ~field_name ~field_type

and solve_record : record:value -> field_name:Ident.t -> field_type:value -> bool =
  fun ~record ~field_name ~field_type ->
  match force record with
  | `Lit (Record fields) ->
    (match Map.find fields field_name with
     | Some f -> Context.tell_constraint (Constraint.equals field_type f)
     | None -> ());
    true
  | `RecordType { fields; tail } ->
    (match Map.find fields field_name with
     | Some existing_type ->
       Context.tell_constraint (Constraint.equals field_type existing_type);
       true
     | None ->
       (match tail with
        | Some tail_val ->
          Context.tell_constraint
            (Constraint.has_field ~record:tail_val ~field_name ~field_type);
          true
        | None ->
          Context.tell_error
          @@ `NoField
               ( field_name
               , List.map ~f:(Util.Tuple.second Term.show_value) @@ Map.to_alist fields );
          true))
  | `Neutral (NMeta m) ->
    (* Solve meta to a record type with this field and an open tail *)
    let fresh_tail = `Neutral (NMeta (Context.fresh_meta ())) in
    let record_type : Term.value =
      `RecordType
        { fields = Ident.Map.singleton field_name field_type; tail = Some fresh_tail }
    in
    solve_meta m record_type;
    true
  | _ ->
    Context.tell_error @@ `Expected ("record", Term.show_value record);
    true

and subsumes ~sub ~super =
  let open Continue_or_stop in
  match sub, super with
  | `RecordType sub, `RecordType super ->
    let fields_match : bool =
      Map.fold_until
        super.fields
        ~init:true
        ~finish:Fun.id
        ~f:(fun ~key ~data:super_ty acc ->
          match Map.find sub.fields key with
          | Some sub_ty ->
            Context.tell_constraint (Constraint.subtype ~sub:sub_ty ~super:super_ty);
            Continue acc
          | None ->
            (match sub.tail with
             | Some tail ->
               Context.tell_constraint
                 (Constraint.has_field ~record:tail ~field_name:key ~field_type:super_ty);
               Continue acc
             | None -> Stop false))
    in
    (match sub.tail, super.tail with
     | _, None -> fields_match
     | None, Some _ -> false
     | Some sub, Some super ->
       Context.tell_constraint (Constraint.subtype ~sub ~super);
       fields_match)
  (* NOTE: This is awkward and I am very suspicious of it *)
  | _ ->
    Context.tell_constraint (Constraint.equals sub super);
    true
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
  Context.run (fun () -> subsumes ~sub:a ~super:a)
  |> fst
  |> function
  | Ok _ -> true
  | Error es ->
    Printf.printf
      "Failed: '%s'"
      (Sexp.to_string_hum @@ List.sexp_of_t Calyx_error.sexp_of_t es);
    false
;;
