type 'a filter

external create : int -> int -> 'a filter = "caml_bloom_create"
external count : 'a filter -> int = "caml_bloom_count"
external size : 'a filter -> int = "caml_bloom_size"
external _put : 'a filter -> 'a -> unit = "caml_bloom_put"
external _test : 'a filter -> 'a -> bool = "caml_bloom_test"

module type BloomFilter = sig
  type t
  type elt

  (** [create size num_hashes] creates a bloom filter.
    [size] is the number of bits (rounded up to power of 2).
    [num_hashes] is the number of hash functions to simulate. *)
  val create : size:int -> numHashes:int -> t

  (** [count t] returns the number of elements inserted. *)
  val count : t -> int

  (** [size t] returns the actual bit array size (power of 2). *)
  val size : t -> int

  (** [put t v] inserts value [v] into the filter.
    Dispatches to optimized int path for [int t], string path for [string t]. *)
  val add : t -> elt -> unit

  (** [test t v] tests if value [v] may be in the filter.
    Returns [false] if definitely not present, [true] if possibly present. *)
  val member : t -> elt -> bool
end

module String : BloomFilter = struct
  type t = string filter
  type elt = string

  let create ~size ~numHashes = create size numHashes
  let count = count
  let size = size
  let add : t -> elt -> unit = _put
  let member : t -> elt -> bool = _test

  let%test_unit "String not present" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:100
         ~name:"Strings not inserted are not members"
         QCheck.(list_of_size (Gen.int_range 16 256) string)
         (fun strings ->
            let filter = create ~size:4096 ~numHashes:3 in
            List.for_all (Fun.compose not (member filter)) strings)
  ;;

  let%test_unit "String no false negatives" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:100
         ~name:"inserted strings are found"
         QCheck.(list_of_size (Gen.int_range 1 200) string)
         (fun strings ->
            let filter = create ~size:8192 ~numHashes:3 in
            List.iter (add filter) strings;
            List.for_all (member filter) strings)
  ;;
end

module Int : BloomFilter = struct
  type t = int filter
  type elt = int

  let create ~size ~numHashes = create size numHashes
  let count = count
  let size = size
  let add : t -> int -> unit = _put
  let member : t -> int -> bool = _test

  let%test_unit "Int no false negatives" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:100
         ~name:"inserted ints are found"
         QCheck.(list_of_size (Gen.int_range 1 200) int)
         (fun ints ->
            let filter = create ~size:8192 ~numHashes:3 in
            List.iter (add filter) ints;
            List.for_all (member filter) ints)
  ;;

  let%test_unit "Int not present" =
    QCheck.Test.check_exn
    @@ QCheck.Test.make
         ~count:100
         ~name:"Ints not inserted are not members"
         QCheck.(list_of_size (Gen.int_range 16 256) int)
         (fun ints ->
            let filter = create ~size:4096 ~numHashes:3 in
            List.for_all (Fun.compose not (member filter)) ints)
  ;;
end
