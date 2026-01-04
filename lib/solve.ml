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

  let handle : Term.value Meta.gen -> (unit -> 'a) -> 'a = fun m f -> 
    let open Effect.Deep in
    try f () 
      with 
      | effect (Solve (meta, solution)), k ->
          Meta.Solutions.add m.solutions meta (Some solution);
          continue k ()
      | effect (Solution meta), k ->
          let entry = Meta.Solutions.find m.solutions meta in
          continue k entry 
      | effect e, k ->
          (* print_endline "RERAISE IN SOLUTION"; *)
          (* Re-perform unhandled effects *)
          continue k (Effect.perform e)
end

open Term
let rec force : value -> value = function
  | `Neutral (NMeta m) as v ->
    (match Solution.solution m with
     | Some solution -> force solution
     | None -> v)
  | v -> v
;;

let force_row (r : row) : row =
  { fields = List.map (fun (l, v) -> l, force v) r.fields; tail = force r.tail }
;;

let rec occurs (m : Meta.t) (v : value) : bool = 
  match force v with 
  | `Neutral (NMeta m') -> Meta.equal m m'
  | `Neutral (NApp (f, x)) -> occurs_neutral m f || occurs m x 
  | `Neutral (NProj (n, _)) -> occurs_neutral m n 
  | `Pi (_, dom, cod) -> 
    let var = `Neutral (NVar (0, "_")) in 
    occurs m dom || occurs m (cod var)
  | `Lam (_, body) -> 
    let var = `Neutral (NVar (0, "_")) in 
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
  List.exists (fun v -> occurs m @@ snd v) r.fields || occurs m r.tail

and occurs_lit m = function 
  Record fields -> List.exists (fun v -> occurs m @@ snd v) fields 
  | _ -> false

let solve_meta (m : Meta.t) (v : value) : (unit, Error.t) result = 
  match occurs m v with 
  | true -> 
      Error (`Occurs m)
  | false -> 
    Solution.solve m v; 
    Ok ()

let (let*) = Result.bind

let rec unify  : value -> value -> (unit, Error.t) result = fun a b ->
  let a = force a and b = force b in 
  match a, b with 
  | `Neutral (NMeta m1), `Neutral (NMeta m2) when Meta.equal m1 m2 -> 
      Ok ()
  | `Neutral (NMeta m), v 
  | v, `Neutral (NMeta m) -> 
    let* _ = solve_meta m v in Ok ()
  | `Type, `Type -> Ok ()
  | `Lam (_, body1), `Lam (_, body2) -> 
      let var = `Neutral (NVar (0, "_")) in 
      Constraints.tell @@ Unify ((body1 var), (body2 var));
      Ok()
  | `Lam (_, body), f | f, `Lam (_, body) -> 
      let var = `Neutral (NVar (0, "_")) in 
      let* right = vapp f var in
      Constraints.tell @@ Unify (body var, right);
      Ok ()
  | `Pi (_, dom, cod), `Pi (_, dom', cod') -> 
      let var = `Neutral (NVar (0, "_")) in 
      Constraints.(tell $ Unify (dom, dom'));
      Constraints.(tell $ Unify (cod var, cod' var));
      Ok ()
  | `Neutral l, `Neutral r -> unify_neutral l r 
  | `Lit l, `Lit r -> unify_lit l r 
  | `Row l, `Row r -> unify_rows l r 
  | `Rec l, `Rec r -> 
      Constraints.(tell $ Unify (l, r)); 
      Ok ()
  | `RowNil, `RowNil -> Ok ()
  | `Row { fields = []; tail }, `RowNil -> 
      Constraints.(tell $ Unify (tail, `RowNil));
      Ok ()
  | `RowNil, `Row { fields = _ :: _; _ }
  | `Row { fields = _ :: _; _}, `RowNil -> 
      Error (`Expected (Pretty.value a, Pretty.value b))
  | `Err _, _ | _, `Err _ -> Ok ()
  | _,_ -> Error `Todo

and unify_neutral : neutral -> neutral -> (unit, Error.t) result = fun l r -> 
  match l,r with 
  | NVar (l, _) , NVar (r, _) when Int.equal l r -> Ok ()
  | NApp (f, x), NApp (f', x') -> 
      let* _ = unify_neutral f f' in 
      Constraints.tell @@Unify (x, x');
      Ok ()
  | NProj (tm, field), NProj (tm', field') when String.equal field field' -> 
      unify_neutral tm tm' 
  (* Neutrals mismatch*)
  | _, _ -> Error `Todo

and unify_lit l r = 
  match l, r with 
  | Int a, Int b 
  | UInt a, UInt b when Int.equal a b -> Ok ()
  | Float a, Float b when Float.equal a b -> Ok ()
  | Bool a, Bool b when Bool.equal a b -> Ok ()
  | Record l, Record r ->  begin
      let sorted_l : (Ident.t * value) list = List.sort_uniq (fun a b -> Ident.compare (fst a) (fst b)) l
      and sorted_r : (Ident.t * value) list = List.sort_uniq (fun a b -> Ident.compare (fst a) (fst b)) r in 
      let fields_l = List.map fst sorted_l 
      and fields_r = List.map fst sorted_r in
      if (List.equal Ident.equal fields_l fields_r) then begin
        List.iter2 (fun (_, v1) (_, v2) -> 
          Constraints.tell @@ Unify (v1, v2);
        ) sorted_l sorted_r;
        Ok ()
      end else 
        Error `Todo
    end
  | _, _ -> 
      Error `Todo

and unify_rows  : row -> row -> (unit, Error.t) result =  fun l r ->
  let l = force_row l and r = force_row r in 
  let module M = Map.Make(Ident) in 
  let left_fields = M.of_list l.fields and right_fields = M.of_list r.fields in 

  M.union (fun _label v1 v2 -> 
    Constraints.(tell $ Unify (v1, v2));
    None 
  ) left_fields right_fields |> ignore;

  let only_left  = M.filter (fun k _ -> not (M.mem k right_fields)) left_fields |> M.bindings in
  let only_right = M.filter (fun k _ -> not @@ M.mem k left_fields) right_fields |> M.bindings in 

  let fresh_tail = `Neutral (NMeta (Meta.fresh ())) in
  Constraints.(tell $ Unify (l.tail, `Row { fields = only_right; tail = fresh_tail }));
  Constraints.(tell $ Unify (r.tail, `Row { fields = only_left; tail = fresh_tail }));
  Ok ()

and vapp = fun f x -> 
  match f with 
  | `Lam (_, body) -> Result.ok @@ body x 
  | `Neutral n -> Result.ok @@ `Neutral (NApp (n , x))
  | otherwise -> Error (`Expected ("function", (Pretty.value otherwise)))

type solver 
  = Solved 
  | Stuck of { stuck : Constraints.t list; errors : Error.t list }

let rec solve (initial : Constraints.t list) : solver =
  let progressed = ref false in
  let ((_, next), errors) = 
    Error.handle @@ fun () -> 
      Constraints.handle @@ fun () -> 
        List.iter (fun c -> progressed := solve_one c) initial
  in 
  match next, errors, !progressed with 
  | [], [], _ -> Solved 
  | _, _, true -> solve next
  | stuck, errors, false -> Stuck { stuck; errors }

and solve_one = function 
  | Unify (a, b) -> 
    begin 
      match unify a b with 
      | Ok () -> true
      | Error e -> Error.tell e; true
    end
  | HasField (label, record, field) -> solve_record label record field  
  | HasStage _ -> Error.tell `Todo; true
and solve_record : Ident.t -> value -> value -> bool = fun label record field -> 
    match force record with 
    | `Rec row_val -> 
        begin 
          match force row_val with 
          | `Row r -> 
              begin 
                match List.assoc_opt label r.fields with 
                | Some ty -> 
                    Constraints.(tell $ Unify (field, ty));
                    true 
                | None -> 
                  let tail = `Neutral (NMeta (Meta.fresh ())) in 
                  Constraints.(tell $ Unify (r.tail, `Row { fields = [label, field]; tail }));
                  true
              end
          | `Neutral (NMeta _) -> 
              Constraints.(tell @@ HasField (label, record, field));
              false
          | _ -> 
              Error.tell @@ `Expected ("row", (Pretty.value row_val));
              true 
        end

    | `Neutral (NMeta _) -> 
        Constraints.tell @@ HasField (label, record, field);
        false
    | _ -> Error.tell @@ `Expected ("record", (Pretty.value record)); true

