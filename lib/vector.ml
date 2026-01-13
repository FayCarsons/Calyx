type 'a t

external create_raw : int -> 'a t = "caml_vector_create"
external release_roots : 'a t -> unit = "caml_vector_release_roots"
external push : 'a t -> 'a -> unit = "caml_vector_push"
external pop : 'a t -> 'a option = "caml_vector_pop"
external get : 'a t -> int -> 'a option = "caml_vector_get"
external set : 'a t -> int -> 'a -> bool = "caml_vector_set"
external length : 'a t -> int = "caml_vector_length"
external capacity : 'a t -> int = "caml_vector_capacity"

let create ?(capacity = 64) () =
  let v = create_raw capacity in
  Gc.finalise release_roots v;
  v
;;

let is_empty v = length v = 0

let%test "push increases length" =
  let v = create () in
  push v 1;
  push v 2;
  push v 3;
  Int.equal (length v) 3
;;

let%test "pop returns last pushed" =
  let v = create () in
  push v "a";
  push v "b";
  push v "c";
  pop v = Some "c" && pop v = Some "b" && pop v = Some "a"
;;

let%test "pop on empty returns None" =
  let v = create () in
  pop v = None
;;

let%test "get retrieves correct element" =
  let v = create () in
  push v 10;
  push v 20;
  push v 30;
  get v 0 = Some 10 && get v 1 = Some 20 && get v 2 = Some 30
;;

let%test "get out of bounds returns None" =
  let v = create () in
  push v 1;
  get v 0 = Some 1 && get v 1 = None && get v 100 = None
;;

let%test "set modifies element" =
  let v = create () in
  push v "x";
  push v "y";
  set v 0 "z" && get v 0 = Some "z" && get v 1 = Some "y"
;;

let%test "set out of bounds returns false" =
  let v = create () in
  push v "a";
  set v 0 "b" && (not (set v 1 "c")) && not (set v 100 "d")
;;

let%test "is_empty on new vector" =
  let v = create () in
  is_empty v
;;

let%test "is_empty after push" =
  let v = create () in
  push v 42;
  not (is_empty v)
;;

let%test_unit "push/pop roundtrip (QCheck)" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:100
       ~name:"push then pop returns elements in reverse"
       QCheck.(list_of_size (Gen.int_range 1 100) int)
       (fun xs ->
          let v = create () in
          List.iter (push v) xs;
          let popped = List.filter_map (fun _ -> pop v) xs in
          popped = List.rev xs)
;;

let%test_unit "get after push (QCheck)" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:100
       ~name:"'get' returns pushed elements at correct indices"
       QCheck.(list_of_size (Gen.int_range 1 100) string)
       (fun xs ->
          let v = create () in
          List.iter (push v) xs;
          List.for_all (fun (i, x) -> get v i = Some x) (List.mapi (fun i x -> i, x) xs))
;;

let%test_unit "capacity grows (QCheck)" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:50
       ~name:"vector grows beyond initial capacity"
       QCheck.(int_range 100 500)
       (fun n ->
          let v = create ~capacity:8 () in
          for i = 0 to n - 1 do
            push v i
          done;
          length v = n && capacity v >= n)
;;

let%test_unit "survives GC pressure" =
  for _ = 1 to 10 do
    let v = create () in
    for i = 1 to 1000 do
      push v (String.make 100 'x');
      (* allocate heap objects *)
      if i mod 100 = 0 then Gc.full_major () (* force GC *)
    done;
    assert (length v = 1000)
  done
;;
