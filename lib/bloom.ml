type 'a filter

external create : int -> int -> 'a filter = "caml_bloom_create"
external count : 'a filter -> int = "caml_bloom_count"
external size : 'a filter -> int = "caml_bloom_size"
external _put : 'a filter -> 'a -> unit = "caml_bloom_put"
external _test : 'a filter -> 'a -> bool = "caml_bloom_test"

module String = struct
  type t = string filter

  let create ~size ~numHashes = create size numHashes
  let count = count
  let size = size
  let add : t -> string -> unit = _put
  let member : t -> string -> bool = _test

  let%test_unit "String not present" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:1000
         ~name:"Strings not inserted are not members"
         QCheck.(list_of_size (Gen.int_range 16 512) string)
         (fun strings ->
            let filter = create ~size:4096 ~numHashes:3 in
            List.for_all (Fun.compose not (member filter)) strings)
  ;;

  let%test_unit "String no false negatives" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:1000
         ~name:"inserted strings are found"
         QCheck.(list_of_size (Gen.int_range 16 512) string)
         (fun strings ->
            let filter = create ~size:8192 ~numHashes:3 in
            List.iter (add filter) strings;
            List.for_all (member filter) strings)
  ;;
end

module Int = struct
  type t = int filter

  let create ~size ~numHashes = create size numHashes
  let count = count
  let size = size
  let add : t -> int -> unit = _put
  let member : t -> int -> bool = _test

  let%test_unit "Int no false negatives" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:1000
         ~name:"inserted ints are found"
         QCheck.(list_of_size (Gen.int_range 16 512) int)
         (fun ints ->
            let filter = create ~size:8192 ~numHashes:3 in
            List.iter (add filter) ints;
            List.for_all (member filter) ints)
  ;;

  let%test_unit "Int not present" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:1000
         ~name:"Ints not inserted are not members"
         QCheck.(list_of_size (Gen.int_range 16 512) int)
         (fun ints ->
            let filter = create ~size:4096 ~numHashes:3 in
            List.for_all (Fun.compose not (member filter)) ints)
  ;;
end
