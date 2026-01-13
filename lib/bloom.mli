(** Fast probabilistic set membership using a bloom filter.

    Uses wyhash and Kirsch-Mitzenmacher optimization internally.
    Size is rounded up to the next power of two for fast indexing. *)

module String : sig
  type t

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
  val add : t -> string -> unit

  (** [test t v] tests if value [v] may be in the filter.
    Returns [false] if definitely not present, [true] if possibly present. *)
  val member : t -> string -> bool
end

module Int : sig
  type t

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
  val add : t -> int -> unit

  (** [test t v] tests if value [v] may be in the filter.
    Returns [false] if definitely not present, [true] if possibly present. *)
  val member : t -> int -> bool
end
