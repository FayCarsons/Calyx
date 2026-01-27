open Core

let topsort : Ident.t list Ident.Map.t -> Ident.t list =
  fun deps ->
  let in_degree = Hashtbl.create (module Ident) in
  let dependents = Hashtbl.create (module Ident) in
  Map.iteri deps ~f:(fun ~key ~data ->
    Hashtbl.set in_degree ~key ~data:(List.length data);
    List.iter data ~f:(fun dep ->
      Hashtbl.update dependents dep ~f:(function
        | None -> [ key ]
        | Some xs -> key :: xs)));
  let queue = Queue.create () in
  Map.iter_keys deps ~f:(fun key ->
    match Hashtbl.find in_degree key with
    | Some deg when Int.equal deg 0 -> Queue.enqueue queue key
    | _ -> ());
  let result = ref [] in
  while not (Queue.is_empty queue) do
    let node = Queue.dequeue_exn queue in
    result := node :: !result;
    Hashtbl.find dependents node
    |> Option.value ~default:[]
    |> List.iter ~f:(fun dep ->
      Hashtbl.change in_degree dep ~f:(function
        | None -> None
        | Some n ->
          let n' = n - 1 in
          if n' = 0 then Queue.enqueue queue dep;
          Some n'))
  done;
  List.rev !result
;;

let%test "empty graph returns empty list" =
  let deps = Ident.Map.empty in
  List.is_empty (topsort deps)
;;

let%test "single node with no deps" =
  let a = Ident.Intern.intern "a" in
  let deps = Ident.Map.of_alist_exn [ a, [] ] in
  List.equal Ident.equal (topsort deps) [ a ]
;;

let%test "linear chain: c -> b -> a" =
  let a = Ident.Intern.intern "topsort_a" in
  let b = Ident.Intern.intern "topsort_b" in
  let c = Ident.Intern.intern "topsort_c" in
  let deps = Ident.Map.of_alist_exn [ a, []; b, [ a ]; c, [ b ] ] in
  let result = topsort deps in
  List.equal Ident.equal [ a; b; c ] result
;;

let%test "diamond: d depends on b,c; b,c depend on a" =
  let a = Ident.Intern.intern "diamond_a" in
  let b = Ident.Intern.intern "diamond_b" in
  let c = Ident.Intern.intern "diamond_c" in
  let d = Ident.Intern.intern "diamond_d" in
  let deps = Ident.Map.of_alist_exn [ a, []; b, [ a ]; c, [ a ]; d, [ b; c ] ] in
  let result = topsort deps in
  let pos x =
    List.findi result ~f:(fun _ y -> Ident.equal x y) |> Option.value_exn |> fst
  in
  pos a < pos b && pos a < pos c && pos b < pos d && pos c < pos d
;;

let%test "independent nodes all appear" =
  let a = Ident.Intern.intern "indep_a" in
  let b = Ident.Intern.intern "indep_b" in
  let c = Ident.Intern.intern "indep_c" in
  let deps = Ident.Map.of_alist_exn [ a, []; b, []; c, [] ] in
  let result = topsort deps in
  List.length result = 3
  && List.exists result ~f:(Ident.equal a)
  && List.exists result ~f:(Ident.equal b)
  && List.exists result ~f:(Ident.equal c)
;;

let%test "multiple deps: c depends on both a and b" =
  let a = Ident.Intern.intern "multi_a" in
  let b = Ident.Intern.intern "multi_b" in
  let c = Ident.Intern.intern "multi_c" in
  let deps = Ident.Map.of_alist_exn [ a, []; b, []; c, [ a; b ] ] in
  let result = topsort deps in
  let pos x =
    List.findi result ~f:(fun _ y -> Ident.equal x y) |> Option.value_exn |> fst
  in
  pos a < pos c && pos b < pos c
;;

let%test_unit "property: deps always precede dependents" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:100
       ~name:"all dependencies appear before their dependents"
       QCheck.(list_of_size (Gen.int_range 2 32) (string_of_size (Gen.int_range 1 64)))
       (fun names ->
          let names = List.dedup_and_sort names ~compare:String.compare in
          let idents = List.map names ~f:Ident.Intern.intern in
          let deps =
            List.mapi idents ~f:(fun i id ->
              let possible_deps = List.take idents i in
              id, possible_deps)
            |> Ident.Map.of_alist_exn
          in
          let result = topsort deps in
          let pos x =
            List.findi result ~f:(fun _ y -> Ident.equal x y) |> Option.value_exn |> fst
          in
          Map.for_alli deps ~f:(fun ~key ~data ->
            List.for_all data ~f:(fun dep -> pos dep < pos key)))
;;
