open Core

(** Strict positivity: the declared datatype name must not occur inside the
    domain of any function type within a constructor field. This admits
    [Cons a (List a)] and [List (List a)], and rejects [MkBad (Bad -> Int)].

    Nested positivity (occurrences as arguments to other, not-yet-checked type
    constructors) is not refined here; it is deliberately coarse for now. *)

let rec occurs (name : Ident.t) : Term.t -> bool = function
  | `Var x -> Ident.equal x name
  | `App (f, x) -> occurs name f || occurs name x
  | `Pi { dom; cod; _ } -> occurs name dom || occurs name cod
  | `Ann (x, t) -> occurs name x || occurs name t
  | `Pos (_, t) -> occurs name t
  | `RecordType { fields; tail } ->
    List.exists (Map.data fields) ~f:(occurs name)
    || Option.exists tail ~f:(occurs name)
  | `Self (_, body) -> occurs name body
  | `Infix { left; op; right } ->
    occurs name left || occurs name op || occurs name right
  | _ -> false
;;

let rec negative (name : Ident.t) : Term.t -> bool = function
  | `Pi { dom; cod; _ } -> occurs name dom || negative name cod
  | `App (f, x) -> negative name f || negative name x
  | `Ann (x, t) -> negative name x || negative name t
  | `Pos (_, t) -> negative name t
  | `RecordType { fields; tail } ->
    List.exists (Map.data fields) ~f:(negative name)
    || Option.exists tail ~f:(negative name)
  | `Self (_, body) -> negative name body
  | _ -> false
;;

let check ({ ident; constructors; _ } : Term.t Term.sum_type)
  : (unit, Calyx_error.t) result
  =
  Map.to_alist ~key_order:`Increasing constructors
  |> List.find_map ~f:(fun (ctor, fields) ->
    if List.exists fields ~f:(negative ident)
    then Some (`Positivity (ident, ctor))
    else None)
  |> function
  | Some err -> Error err
  | None -> Ok ()
;;

let%test_module "positivity" =
  (module struct
    let%test_unit "generated specifications are strictly positive" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make ~count:200 ~name:"positive-accepted" Testgen.arb_adt_spec
      @@ fun spec -> Result.is_ok (check (Testgen.adt_of_spec spec))
    ;;

    let%test_unit "an injected negative occurrence is rejected" =
      QCheck.Test.check_exn
      @@ QCheck.Test.make ~count:200 ~name:"negative-rejected" Testgen.arb_adt_with_index
      @@ fun (spec, i) ->
      let adt = Testgen.adt_of_spec spec in
      let negative_field : Term.t =
        `Pi
          { plicity = Term.Explicit
          ; ident = Ident.Intern.underscore
          ; dom = Testgen.applied_data spec
          ; cod = `Var Testgen.int_name
          }
      in
      let constructors =
        Map.update adt.constructors (Testgen.ctor_name i) ~f:(fun fields ->
          negative_field :: Option.value fields ~default:[])
      in
      (match check { adt with constructors } with
       | Error (`Positivity _) -> true
       | _ -> false)
    ;;
  end)
;;
