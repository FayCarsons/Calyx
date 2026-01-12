(** Fast probabilistic set membership using a bloom filter.

    Uses wyhash and Kirsch-Mitzenmacher optimization internally.
    Size is rounded up to the next power of two for fast indexing. *)

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

module String : BloomFilter
module Int : BloomFilter
