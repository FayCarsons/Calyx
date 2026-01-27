type 'a t

external create_raw : int -> 'a t = "caml_vector_create"
external release_roots : 'a t -> unit = "caml_vector_release_roots"
external push : 'a t -> 'a -> unit = "caml_vector_push"
external pop : 'a t -> 'a option = "caml_vector_pop"
external get : 'a t -> int -> 'a option = "caml_vector_get"
external set : 'a t -> int -> 'a -> bool = "caml_vector_set"
external length : 'a t -> int = "caml_vector_length"
external capacity : 'a t -> int = "caml_vector_capacity"
external unsafe_get : 'a t -> int -> 'a = "caml_vector_unsafe_get"
external unsafe_set : 'a t -> int -> 'a -> unit = "caml_vector_unsafe_set"
external to_array : 'a t -> 'a array = "caml_vector_to_array"
external of_array : 'a array -> 'a t = "caml_vector_of_array"
external clear : 'a t -> unit = "caml_vector_clear"
external shrink_to_fit : 'a t -> unit = "caml_vector_shrink_to_fit"
external copy : 'a t -> 'a t = "caml_vector_copy"
external append : 'a t -> 'a t -> unit = "caml_vector_append"
external concat : 'a t -> 'a t -> 'a t = "caml_vector_concat"
external rev : 'a t -> 'a t = "caml_vector_rev"
external rev_in_place : 'a t -> unit = "caml_vector_rev_in_place"
external blit : 'a t -> int -> 'a t -> int -> int -> unit = "caml_vector_blit"

let create ?(capacity = 64) () =
  let v = create_raw capacity in
  Gc.finalise release_roots v;
  v
;;

let[@inline] is_empty v = length v = 0
let[@inline] first v = if is_empty v then None else Some (unsafe_get v 0)

let[@inline] last v =
  let len = length v in
  if len = 0 then None else Some (unsafe_get v (len - 1))
;;

let iter f v =
  let len = length v in
  for i = 0 to len - 1 do
    f (unsafe_get v i)
  done
;;

let iteri f v =
  let len = length v in
  for i = 0 to len - 1 do
    f i (unsafe_get v i)
  done
;;

let fold_left f acc v =
  let len = length v in
  let r = ref acc in
  for i = 0 to len - 1 do
    r := f !r (unsafe_get v i)
  done;
  !r
;;

let fold_right f v acc =
  let len = length v in
  let r = ref acc in
  for i = len - 1 downto 0 do
    r := f (unsafe_get v i) !r
  done;
  !r
;;

let to_list v = fold_right List.cons v []

let of_list xs =
  let v = create () in
  List.iter (push v) xs;
  v
;;

let map f v =
  let len = length v in
  let r = create ~capacity:(max len 1) () in
  for i = 0 to len - 1 do
    push r (f (unsafe_get v i))
  done;
  r
;;

let mapi f v =
  let len = length v in
  let r = create ~capacity:(max len 1) () in
  for i = 0 to len - 1 do
    push r (f i (unsafe_get v i))
  done;
  r
;;

let filter p v =
  let r = create () in
  let len = length v in
  for i = 0 to len - 1 do
    let x = unsafe_get v i in
    if p x then push r x
  done;
  r
;;

let filter_map f v =
  let r = create () in
  let len = length v in
  for i = 0 to len - 1 do
    match f (unsafe_get v i) with
    | Some y -> push r y
    | None -> ()
  done;
  r
;;

let find p v =
  let len = length v in
  let rec go i =
    if i >= len
    then None
    else (
      let x = unsafe_get v i in
      if p x then Some x else go (i + 1))
  in
  go 0
;;

let findi p v =
  let len = length v in
  let rec go i =
    if i >= len
    then None
    else (
      let x = unsafe_get v i in
      if p x then Some (i, x) else go (i + 1))
  in
  go 0
;;

let find_map f v =
  let len = length v in
  let rec go i =
    if i >= len
    then None
    else (
      match f (unsafe_get v i) with
      | Some _ as r -> r
      | None -> go (i + 1))
  in
  go 0
;;

let exists p v =
  let len = length v in
  let rec go i = i < len && (p (unsafe_get v i) || go (i + 1)) in
  go 0
;;

let for_all p v =
  let len = length v in
  let rec go i = i >= len || (p (unsafe_get v i) && go (i + 1)) in
  go 0
;;

let mem ?(equal = ( = )) x v = exists (equal x) v
let count p v = fold_left (fun n x -> if p x then n + 1 else n) 0 v

let compare cmp a b =
  let len_a = length a
  and len_b = length b in
  let rec go i =
    if i >= len_a
    then if i >= len_b then 0 else -1
    else if i >= len_b
    then 1
    else (
      let c = cmp (unsafe_get a i) (unsafe_get b i) in
      if c <> 0 then c else go (i + 1))
  in
  go 0
;;

let equal eq a b =
  let len_a = length a
  and len_b = length b in
  len_a = len_b
  &&
  let rec go i = i >= len_a || (eq (unsafe_get a i) (unsafe_get b i) && go (i + 1)) in
  go 0
;;

let hash hash_elt v =
  let len = length v in
  let h = ref (Hashtbl.hash len) in
  for i = 0 to len - 1 do
    h := Hashtbl.seeded_hash !h (hash_elt (unsafe_get v i))
  done;
  !h
;;

let t_of_sexp : (Core.Sexp.t -> 'a) -> Core.Sexp.t -> 'a t =
  let open Core in
  fun f sexp -> Array.t_of_sexp f sexp |> of_array
;;

let sexp_of_t : ('a -> Core.Sexp.t) -> 'a t -> Core.Sexp.t =
  let open Core in
  fun f self -> to_array self |> Array.sexp_of_t f
;;

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
