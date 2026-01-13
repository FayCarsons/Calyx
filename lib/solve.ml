open Util

module Constraints = struct
  module M = struct
    (* Type solving constraints *)
    type t =
      | Unify of Term.value * Term.value
      | HasField of Ident.t * Term.value * Term.value
      | HasStage of Term.value * stage

    and stage =
      | Fragment
      | Vertex
      | Compute

    let pretty = function
      | Unify (a, b) -> Printf.sprintf "Unify %s %s" (Pretty.value a) (Pretty.value b)
      | HasField (ident, ty, tm) ->
        Printf.sprintf
          "HasField %s %s %s"
          (Ident.Intern.lookup ident)
          (Pretty.value ty)
          (Pretty.value tm)
      | HasStage (tm, stage) ->
        Printf.sprintf "HasStage %s %s" (Pretty.value tm)
        @@
          (match stage with
          | Fragment -> "Fragment"
          | Vertex -> "Vertex"
          | Compute -> "Compute")
    ;;
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

let force_row (r : row) : row =
  { fields = List.map (fun (l, v) -> l, force v) r.fields
  ; tail = Option.map force r.tail
  }
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
  | `Row r -> occurs_row m r
  | `Rec r -> occurs m r
  | `Lit lit -> occurs_lit m lit
  | _ -> false

and occurs_neutral m = function
  | NMeta m' -> Meta.equal m m'
  | NApp (f, x) -> occurs_neutral m f || occurs m x
  | NProj (n, _) -> occurs_neutral m n
  | _ -> false

and occurs_row m r =
  List.exists (fun v -> occurs m @@ snd v) r.fields || is_some_and (occurs m) r.tail

and occurs_lit m = function
  | Record fields -> List.exists (fun v -> occurs m @@ snd v) fields
  | _ -> false
;;

let solve_meta (m : Meta.t) (v : value) : (unit, Error.t) result =
  match occurs m v with
  | true -> Error (`Occurs m)
  | false ->
    Solution.solve m v;
    Ok ()
;;

let ( let* ) = Result.bind

let rec unify : value -> value -> (unit, Error.t) result =
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
    Constraints.tell @@ Unify (body1 var, body2 var);
    Ok ()
  | `Lam (_, body), f | f, `Lam (_, body) ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    let* right = vapp f var in
    Constraints.tell @@ Unify (body var, right);
    Ok ()
  | `Pi (_, dom, cod), `Pi (_, dom', cod') ->
    let var = `Neutral (NVar (0, Ident.Intern.underscore)) in
    Constraints.(tell $ Unify (dom, dom'));
    Constraints.(tell $ Unify (cod var, cod' var));
    Ok ()
  | (`Neutral (NVar (_, name)), `Var name' | `Var name, `Neutral (NVar (_, name')))
    when Ident.equal name name' -> Ok ()
  | `Neutral (NVar (l_level, l_name)), `Neutral (NVar (r_level, r_name)) ->
    if Int.equal l_level r_level && Ident.equal l_name r_name
    then Ok ()
    else
      Error
        (`UnificationFailure
            ( Pretty.neutral (NVar (l_level, l_name))
            , Pretty.neutral (NVar (r_level, r_name)) ))
  | `Neutral l, `Neutral r -> unify_neutral l r
  | `Lit l, `Lit r -> unify_lit l r
  | `Err _, _ | _, `Err _ -> Ok ()
  | a, b -> Error (`UnificationFailure (Pretty.value a, Pretty.value b))

and unify_neutral : neutral -> neutral -> (unit, Error.t) result =
  fun l r ->
  match l, r with
  | NVar (l_level, l_name), NVar (r_level, r_name)
    when Int.equal l_level r_level && Ident.equal l_name r_name -> Ok ()
  | NApp (f, x), NApp (f', x') ->
    let* _ = unify_neutral f f' in
    Constraints.tell @@ Unify (x, x');
    Ok ()
  | NProj (tm, field), NProj (tm', field') when Ident.equal field field' ->
    unify_neutral tm tm'
  (* Neutrals mismatch*)
  | l, r -> Error (`UnificationFailure (Pretty.neutral l, Pretty.neutral r))

and unify_lit l r =
  match l, r with
  | (Int a, Int b | UInt a, UInt b) when Int.equal a b -> Ok ()
  | Float a, Float b when Float.equal a b -> Ok ()
  | Bool a, Bool b when Bool.equal a b -> Ok ()
  | Record l, Record r ->
    let sorted_l : (Ident.t * value) list =
      List.sort_uniq (fun a b -> Ident.compare (fst a) (fst b)) l
    and sorted_r : (Ident.t * value) list =
      List.sort_uniq (fun a b -> Ident.compare (fst a) (fst b)) r
    in
    let fields_l = List.map fst sorted_l
    and fields_r = List.map fst sorted_r in
    if List.equal Ident.equal fields_l fields_r
    then (
      List.iter2
        (fun (_, v1) (_, v2) -> Constraints.tell @@ Unify (v1, v2))
        sorted_l
        sorted_r;
      Ok ())
    else Error (`UnificationFailure ("record fields mismatch", "record fields mismatch"))
  | _, _ -> Error `Todo

and unify_rows : row -> row -> (unit, Error.t) result =
  fun l r ->
  let l = force_row l
  and r = force_row r in
  let module M = Map.Make (Ident) in
  let left_fields = M.of_list l.fields
  and right_fields = M.of_list r.fields in
  M.union
    (fun _label v1 v2 ->
       Constraints.(tell $ Unify (v1, v2));
       None)
    left_fields
    right_fields
  |> ignore;
  let only_left =
    M.filter (fun k _ -> not (M.mem k right_fields)) left_fields |> M.bindings
  in
  let only_right =
    M.filter (fun k _ -> not @@ M.mem k left_fields) right_fields |> M.bindings
  in
  let tail_meta = `Neutral (NMeta (Meta.fresh ())) in
  let tail = Some tail_meta in
  Option.iter
    (fun (l, r) ->
       Constraints.(tell $ Unify (l, `Row { fields = only_right; tail }));
       Constraints.(tell $ Unify (r, `Row { fields = only_left; tail })))
    (Tuple.into <$> l.tail <*> r.tail);
  Ok ()

and vapp =
  fun f x ->
  match f with
  | `Lam (_, body) -> Result.ok @@ body x
  | `Neutral n -> Result.ok @@ `Neutral (NApp (n, x))
  | otherwise -> Error (`Expected ("function", Pretty.value otherwise))
;;

type solver_error =
  | Stuck of
      { stuck : Constraints.t list
      ; errors : Error.t list
      }
  | Errors of Error.t list

let pretty_solver_error = function
  | Stuck { stuck; errors } ->
    Printf.sprintf
      "Stuck!\nCould not solve constraints:\n%s\nWith errors:\n%s\n"
      (String.concat ", " @@ List.map Constraints.pretty stuck)
      (String.concat ", " @@ List.map Pretty.error errors)
  | Errors es ->
    Printf.sprintf
      "Failed to solve due to errors:\n%s\n"
      (String.concat ", " @@ List.map Pretty.error es)
;;

let rec solve (initial : Constraints.t list) : (unit, solver_error) result =
  let progressed = ref false in
  let (_, next), errors =
    Error.handle (fun () ->
      Constraints.handle (fun () ->
        List.iter
          (fun c ->
             print_endline
               (Printf.sprintf "SOLVING CONSTRAINT: %s" (Constraints.pretty c));
             progressed := solve_one c)
          initial))
  in
  match next, errors, !progressed with
  | [], [], _ -> Ok ()
  | [], (_ :: _ as es), _ -> Error (Errors es)
  | _, _, true -> solve next
  | stuck, errors, false -> Error (Stuck { stuck; errors })

and solve_one = function
  | Unify (a, b) ->
    Printf.printf "SOLVING UNIFY '%s' '%s'\n" (Pretty.value a) (Pretty.value b);
    (match unify a b with
     | Ok () ->
       print_endline "SUCCEEDED";
       true
     | Error e ->
       print_endline "FAILED";
       Error.tell e;
       true)
  | HasField (label, record, field) -> solve_record label record field
  | HasStage _ ->
    Error.tell `Todo;
    true

and solve_record : Ident.t -> value -> value -> bool =
  fun label record field ->
  match force record with
  | `Rec row_val ->
    (match force row_val with
     | `Row r ->
       Option.iter
         (fun ty -> Constraints.(tell $ Unify (field, ty)))
         (List.assoc_opt label r.fields);
       true
     | `Neutral (NMeta _) ->
       Constraints.(tell @@ HasField (label, record, field));
       false
     | _ ->
       Error.tell @@ `Expected ("row", Pretty.value row_val);
       true)
  | `Neutral (NMeta _) ->
    Constraints.tell @@ HasField (label, record, field);
    false
  | _ ->
    Error.tell @@ `Expected ("record", Pretty.value record);
    true
;;
