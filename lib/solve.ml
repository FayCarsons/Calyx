open Core

module Constraints = struct
  module M = struct
    (* Type solving constraints *)
    type t =
      | Equals of Term.value * Term.value
      | HasField of Term.value * Ident.t * Term.value
      | Subtype of
          { sub : Term.value
          ; super : Term.value
          }
    [@@deriving show, sexp]

    let equals a b = Equals (a, b)
    let ( %= ) = equals
    let subtype ~sub ~super = Subtype { sub; super }

    let has_field ~record ~field_name ~field_type =
      HasField (record, field_name, field_type)
    ;;

    let ( =. ) record (field_name, field_type) = has_field ~record ~field_name ~field_type
  end

  include Writer.Make (M)
  include M
end

module Solution = struct
  type _ Effect.t +=
    | Solve : Meta.t * Term.value -> unit Effect.t
    | Solution : Meta.t -> Term.value option Effect.t

  let solve : Meta.t -> Term.value -> unit = fun m t -> Effect.perform @@ Solve (m, t)
  let solution : Meta.t -> Term.value option = fun m -> Effect.perform @@ Solution m

  let handle : Term.value Meta.gen -> (unit -> 'a) -> 'a * Term.value Meta.gen =
    fun m f ->
    let open Effect.Deep in
    let result =
      try_with
        f
        ()
        { effc =
            (fun (type c) (eff : c Effect.t) ->
              match eff with
              | Solve (meta, solution) ->
                Some
                  (fun (k : (c, _) continuation) ->
                    Meta.Solutions.add m.solutions meta (Some solution);
                    continue k ())
              | Solution meta ->
                Some
                  (fun (k : (c, _) continuation) ->
                    let entry = Meta.Solutions.find m.solutions meta in
                    continue k entry)
              | eff ->
                Some (fun (k : (c, _) continuation) -> continue k (Effect.perform eff)))
        }
    in
    result, m
  ;;
end

open Term

let rec force : value -> value = function
  | `Neutral (NMeta m) as v ->
    (match Solution.solution m with
     | Some solution -> force solution
     | None -> v)
  | `Neutral (NVar (_, ident)) as v ->
    (* Try to look up the variable in the environment *)
    (match Env.lookup_value ident with
     | Some ((`Var ident' | `Neutral (NVar (_, ident'))) as v') ->
       if Ident.equal ident ident' then v (* Self-referential, keep as is *) else force v'
     | Some `Opaque -> v (* Don't resolve opaque types, keep as NVar *)
     | _ -> v)
  | v -> v
;;

let rec occurs (m : Meta.t) (v : value) : bool =
  match force v with
  | `Neutral (NMeta m') -> Meta.equal m m'
  | `Neutral (NApp (f, x)) -> occurs_neutral m f || occurs m x
  | `Neutral (NProj (n, _)) -> occurs_neutral m n
  | `Pi (_, dom, cod) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    occurs m dom || occurs m (cod var)
  | `Lam (_, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    occurs m (body var)
  | `RecordType { fields; tail } ->
    Map.exists ~f:(occurs m) fields
    || Option.value ~default:false (Option.map tail ~f:(occurs m))
  | `Lit lit -> occurs_lit m lit
  | _ -> false

and occurs_neutral m = function
  | NMeta m' -> Meta.equal m m'
  | NApp (f, x) -> occurs_neutral m f || occurs m x
  | NProj (n, _) -> occurs_neutral m n
  | _ -> false

and occurs_lit m = function
  | Record fields -> Map.exists fields ~f:(occurs m)
  | _ -> false
;;

let solve_meta (m : Meta.t) (v : value) : (unit, CalyxError.t) result =
  if occurs m v
  then Error (`Occurs m)
  else (
    Solution.solve m v;
    Ok ())
;;

let ( let* ) = Result.Let_syntax.( >>= )

let rec unify : value -> value -> (unit, CalyxError.t) result =
  fun a b ->
  let a = force a
  and b = force b in
  match a, b with
  | `Var l, `Var r ->
    if Ident.equal l r
    then Ok ()
    else Error (`UnificationFailure (Ident.Intern.lookup l, Ident.Intern.lookup r))
  | `Neutral (NMeta m1), `Neutral (NMeta m2) when Meta.equal m1 m2 -> Ok ()
  | `Neutral (NMeta m), v | v, `Neutral (NMeta m) -> solve_meta m v
  | `Type, `Type -> Ok ()
  | `Lam (_, body1), `Lam (_, body2) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    Constraints.(tell (body1 var %= body2 var));
    Ok ()
  | `Lam (_, body), f | f, `Lam (_, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* right = vapp f var in
    Constraints.(tell (body var %= right));
    Ok ()
  | `Pi (_, dom, cod), `Pi (_, dom', cod') ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    Constraints.(tell (dom %= dom'));
    Constraints.(tell (cod var %= cod' var));
    Ok ()
  | (`Neutral (NVar (_, name)), `Var name' | `Var name, `Neutral (NVar (_, name')))
    when Ident.equal name name' -> Ok ()
  | `Neutral (NVar (l_level, l_name)), `Neutral (NVar (r_level, r_name)) ->
    if Int.equal l_level r_level && Ident.equal l_name r_name
    then Ok ()
    else
      Error
        (`UnificationFailure
            ( Term.show_neutral (NVar (l_level, l_name))
            , Term.show_neutral (NVar (r_level, r_name)) ))
  | `Neutral l, `Neutral r -> unify_neutral l r
  | `Lit l, `Lit r -> unify_lit l r
  | `RecordType a, `RecordType b -> unify_record_types a b
  | `Err _, _ | _, `Err _ -> Ok ()
  | a, b -> Error (`UnificationFailure (Term.show_value a, Term.show_value b))

and unify_neutral : neutral -> neutral -> (unit, CalyxError.t) result =
  fun l r ->
  match l, r with
  | NVar (l_level, l_name), NVar (r_level, r_name)
    when Int.equal l_level r_level && Ident.equal l_name r_name -> Ok ()
  | NApp (f, x), NApp (f', x') ->
    let* _ = unify_neutral f f' in
    Constraints.(tell (x %= x'));
    Ok ()
  | NProj (tm, field), NProj (tm', field') when Ident.equal field field' ->
    unify_neutral tm tm'
  (* Neutrals mismatch*)
  | l, r -> Error (`UnificationFailure (Term.show_neutral l, Term.show_neutral r))

and unify_lit l r =
  match l, r with
  | (Int a, Int b | UInt a, UInt b) when Int.equal a b -> Ok ()
  | Float a, Float b when Float.equal a b -> Ok ()
  | Bool a, Bool b when Bool.equal a b -> Ok ()
  | Record l, Record r -> unify_record_literals l r
  | _, _ -> Error `Todo

and unify_record_literals
  : value Ident.Map.t -> value Ident.Map.t -> (unit, CalyxError.t) result
  =
  fun a b ->
  if Set.equal (Map.key_set a) (Map.key_set b)
  then (
    let a' = Map.to_alist a
    and b' = Map.to_alist b in
    if List.for_all2_exn a' b' ~f:(fun (_, a) (_, b) -> Result.is_ok (unify a b))
    then Ok ()
    else
      Error
        (`Expected (Ident.Map.show Term.show_value a, Ident.Map.show Term.show_value b)))
  else
    Error (`Expected (Ident.Map.show Term.show_value a, Ident.Map.show Term.show_value b))

and unify_record_types : value row -> value row -> (unit, CalyxError.t) result =
  fun a b ->
  let all_fields = Set.union (Map.key_set a.fields) (Map.key_set b.fields) in
  let* _ =
    Set.fold_result all_fields ~init:() ~f:(fun _ field ->
      match Map.find a.fields field, Map.find b.fields field with
      | Some a, Some b -> unify a b
      | None, Some x ->
        (match a.tail with
         | Some tail ->
           Constraints.(tell @@ has_field ~record:tail ~field_name:field ~field_type:x);
           Ok ()
         | None ->
           Error
             (`NoField
                 ( field
                 , List.map ~f:(Util.Tuple.second Term.show_value)
                   @@ Map.to_alist a.fields )))
      | Some x, None ->
        (match b.tail with
         | Some tail ->
           Constraints.(tell @@ has_field ~record:tail ~field_name:field ~field_type:x);
           Ok ()
         | None ->
           Error
             (`NoField
                 ( field
                 , List.map ~f:(Util.Tuple.second Term.show_value)
                   @@ Map.to_alist a.fields )))
      | None, None -> (* Unreachable *) Ok ())
  in
  match a.tail, b.tail with
  | None, None -> Ok ()
  | Some a, Some b -> unify a b
  | _ ->
    Error
      (`Expected
          ( "two record types which are either both open, or both closed and equal"
          , Printf.sprintf
              "%s and %s"
              (Term.show_row Term.pp_value a)
              (Term.show_row Term.pp_value b) ))

and vapp =
  fun f x ->
  match f with
  | `Lam (_, body) -> Result.return @@ body x
  | `Neutral n -> Result.return @@ `Neutral (NApp (n, x))
  | otherwise -> Error (`Expected ("function", Term.show_value otherwise))
;;

type solver_error =
  | Stuck of
      { stuck : Constraints.t list
      ; errors : CalyxError.t list
      }
  | Errors of CalyxError.t list
[@@deriving show, sexp]

let pretty_solver_error = function
  | Stuck { stuck; errors = _ :: _ as es } ->
    Printf.sprintf
      "Could not solve constraints:\n%s\nWith errors:\n%s\n"
      (String.concat ~sep:", " @@ List.map ~f:Constraints.show stuck)
      (String.concat ~sep:", " @@ List.map ~f:CalyxError.show es)
  | Stuck { stuck; _ } ->
    Printf.sprintf
      "Could not solve constraints:\n%s\n"
      (String.concat ~sep:", " @@ List.map ~f:Constraints.show stuck)
  | Errors es ->
    Printf.sprintf
      "Failed to solve due to errors:\n%s\n"
      (String.concat ~sep:", " @@ List.map ~f:CalyxError.show es)
;;

let rec solve (initial : Constraints.t list) : (unit, solver_error) result =
  let progressed = ref false in
  let (_, next), errors =
    CalyxError.handle (fun () ->
      Constraints.handle (fun () ->
        List.iter
          ~f:(fun c ->
            (* print_endline (Printf.sprintf "SOLVING CONSTRAINT: %s" (Constraints.show c)); *)
            progressed := solve_one c)
          initial))
  in
  match next, errors, !progressed with
  | [], [], _ -> Ok ()
  | [], (_ :: _ as es), _ -> Error (Errors es)
  | _, _, true -> solve next
  | stuck, errors, false -> Error (Stuck { stuck; errors })

and solve_one = function
  | Equals (a, b) ->
    (* Printf.printf "SOLVING EQUALS '%s' '%s'\n" (Term.show_value a) (Term.show_value b); *)
    (match unify a b with
     | Ok () ->
       (* print_endline "SUCCEEDED"; *)
       true
     | Error e ->
       (* print_endline "FAILED"; *)
       CalyxError.tell e;
       true)
  | Subtype { sub; super } ->
    (* Printf.printf "SOLVING SUBTYPE '%s' :> '%s'" (Term.show_value a) (Term.show_value b); *)
    subsumes ~sub ~super
  | HasField (record, field_name, field_type) ->
    solve_record ~record ~field_name ~field_type

and solve_record : record:value -> field_name:Ident.t -> field_type:value -> bool =
  fun ~record ~field_name ~field_type ->
  match force record with
  | `Lit (Record fields) ->
    Option.iter
      ~f:(Fun.compose Constraints.tell (Constraints.equals field_type))
      (Map.find fields field_name);
    true
  | `Neutral (NMeta _) ->
    Constraints.(tell @@ (record =. (field_name, field_type)));
    false
  | _ ->
    CalyxError.tell @@ `Expected ("record", Term.show_value record);
    true

and subsumes ~sub ~super =
  let open Continue_or_stop in
  match sub, super with
  | `RecordType sub, `RecordType super ->
    Map.fold_until super.fields ~init:true ~finish:Fun.id ~f:(fun ~key ~data _ ->
      match Map.find sub.fields key with
      | Some sub ->
        Constraints.(tell @@ subtype ~sub ~super:data);
        Continue true
      | None ->
        (match sub.tail with
         | Some tail ->
           Constraints.(tell @@ has_field ~record:tail ~field_name:key ~field_type:data);
           Continue true
         | None -> Stop false))
    &&
      (match sub.tail, super.tail with
      | _, None -> true
      | None, Some _ -> false
      | Some sub, Some super ->
        Constraints.(tell @@ subtype ~sub ~super);
        true)
  (* NOTE: This is awkward and I am very suspicious of it *)
  | _ ->
    Constraints.(tell @@ equals sub super);
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
  fst @@ Constraints.handle (fun () -> subsumes ~sub:a ~super:a)
;;
