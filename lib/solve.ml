open Core
open Util

module Constraints = struct
  module M = struct
    (* Type solving constraints *)
    type t =
      | Equals of Term.value * Term.value
      | Subtype of Term.value * Term.value
      | HasField of Term.value * Ident.t * Term.value
    [@@deriving show, sexp]

    let equals a b = Equals (a, b)
    let ( %= ) = equals
    let subtype a b = Subtype (a, b)
    let ( => ) = subtype

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

let force_row (r : row) : row =
  { fields = Map.map r.fields ~f:force; tail = Option.map r.tail ~f:force }
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
  Map.exists r.fields ~f:(fun v -> occurs m v) || is_some_and (occurs m) r.tail

and occurs_lit m = function
  | Record fields -> Map.exists fields ~f:(occurs m)
  | _ -> false
;;

let solve_meta (m : Meta.t) (v : value) : (unit, CalyxError.t) result =
  match occurs m v with
  | true -> Error (`Occurs m)
  | false ->
    Solution.solve m v;
    Ok ()
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
  | Record l, Record r ->
    let sorted_l : (Ident.t * value) list =
      List.sort ~compare:(fun a b -> Ident.compare (fst a) (fst b)) (Map.to_alist l)
    and sorted_r : (Ident.t * value) list =
      List.sort ~compare:(fun a b -> Ident.compare (fst a) (fst b)) (Map.to_alist r)
    in
    let fields_l = List.map ~f:fst sorted_l
    and fields_r = List.map ~f:fst sorted_r in
    if List.equal Ident.equal fields_l fields_r
    then (
      List.iter2_exn
        ~f:(fun (_, v1) (_, v2) -> Constraints.(tell (v1 %= v2)))
        sorted_l
        sorted_r;
      Ok ())
    else Error (`UnificationFailure ("record fields mismatch", "record fields mismatch"))
  | _, _ -> Error `Todo

and unify_rows : row -> row -> (unit, CalyxError.t) result =
  fun l r ->
  let l = force_row l
  and r = force_row r in
  let module M = Ident.Map in
  let left_fields = l.fields
  and right_fields = r.fields in
  Map.iter2
    ~f:(fun ~key:_ ~data ->
      match data with
      | `Both (v1, v2) -> Constraints.(tell (v1 %= v2))
      | _ -> ())
    left_fields
    right_fields
  |> ignore;
  let only_left =
    Map.filter_keys ~f:(fun k -> not (Map.mem right_fields k)) left_fields
  in
  let only_right =
    Map.filter_keys ~f:(fun k -> not @@ Map.mem left_fields k) right_fields
  in
  let tail = Option.some @@ `Neutral (NMeta (Meta.fresh ())) in
  Option.iter
    ~f:(fun (l, r) ->
      Constraints.(tell (l %= `Row { fields = only_left; tail }));
      Constraints.(tell (r %= `Row { fields = only_right; tail })))
    (Tuple.into <$> l.tail <*> r.tail);
  Ok ()

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
  | Subtype (_a, _b) ->
    (* Printf.printf "SOLVING SUBTYPE '%s' :> '%s'" (Term.show_value a) (Term.show_value b); *)
    failwith "TODO"
  | HasField (record, field_name, field_type) ->
    solve_record ~record ~field_name ~field_type

and solve_record : record:value -> field_name:Ident.t -> field_type:value -> bool =
  fun ~record ~field_name ~field_type ->
  match force record with
  | `Rec row_val ->
    (match force row_val with
     | `Row r ->
       Option.iter
         ~f:(Fun.compose Constraints.tell (Constraints.equals field_type))
         (Map.find r.fields field_name);
       true
     | `Neutral (NMeta _) ->
       Constraints.(tell @@ (record =. (field_name, field_type)));
       false
     | _ ->
       CalyxError.tell @@ `Expected ("row", Term.show_value row_val);
       true)
  | `Neutral (NMeta _) ->
    Constraints.(tell @@ (record =. (field_name, field_type)));
    false
  | _ ->
    CalyxError.tell @@ `Expected ("record", Term.show_value record);
    true
;;
