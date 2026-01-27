(** Resizable vector with GC-safe storage of arbitrary OCaml values.

    Backed by a contiguous C array with O(1) amortized push and O(1) random access.
    All OCaml values are registered as GC roots. *)

type 'a t

(** {1 Construction} *)

(** [create ?capacity ()] creates an empty vector. Default capacity is 64. *)
val create : ?capacity:int -> unit -> 'a t

(** [of_list xs] creates a vector from a list. *)
val of_list : 'a list -> 'a t

(** [of_array arr] creates a vector from an array. O(n). *)
val of_array : 'a array -> 'a t

(** [copy v] returns a shallow copy. O(n). *)
val copy : 'a t -> 'a t

(** {1 Access} *)

(** [length v] returns the number of elements. O(1). *)
val length : 'a t -> int

(** [capacity v] returns current capacity. O(1). *)
val capacity : 'a t -> int

(** [is_empty v] returns [true] if [v] has no elements. O(1). *)
val is_empty : 'a t -> bool

(** [get v i] returns element at index [i], or [None] if out of bounds. *)
val get : 'a t -> int -> 'a option

(** [unsafe_get v i] returns element at index [i]. No bounds check. *)
val unsafe_get : 'a t -> int -> 'a

(** [first v] returns the first element, or [None] if empty. *)
val first : 'a t -> 'a option

(** [last v] returns the last element, or [None] if empty. *)
val last : 'a t -> 'a option

(** {1 Modification} *)

(** [push v x] appends [x] to the end. O(1) amortized. *)
val push : 'a t -> 'a -> unit

(** [pop v] removes and returns the last element, or [None] if empty. *)
val pop : 'a t -> 'a option

(** [set v i x] sets element at [i] to [x]. Returns [false] if out of bounds. *)
val set : 'a t -> int -> 'a -> bool

(** [unsafe_set v i x] sets element at [i] to [x]. No bounds check. *)
val unsafe_set : 'a t -> int -> 'a -> unit

(** [clear v] removes all elements. O(n) for GC root cleanup. *)
val clear : 'a t -> unit

(** [append dst src] appends all elements of [src] to [dst]. Mutates [dst]. *)
val append : 'a t -> 'a t -> unit

(** [blit src srcoff dst dstoff len] copies [len] elements.
    @raise Invalid_argument if indices are out of bounds. *)
val blit : 'a t -> int -> 'a t -> int -> int -> unit

(** [rev_in_place v] reverses [v] in place. O(n). *)
val rev_in_place : 'a t -> unit

(** [shrink_to_fit v] reduces capacity to match length. *)
val shrink_to_fit : 'a t -> unit

(** {1 Transformation} *)

(** [map f v] applies [f] to each element, returning a new vector. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [mapi f v] like [map] but [f] also receives the index. *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** [filter p v] returns a new vector with elements satisfying [p]. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** [filter_map f v] applies [f] and collects [Some] results. *)
val filter_map : ('a -> 'b option) -> 'a t -> 'b t

(** [concat a b] returns a new vector with elements of [a] followed by [b]. *)
val concat : 'a t -> 'a t -> 'a t

(** [rev v] returns a new vector with elements in reverse order. *)
val rev : 'a t -> 'a t

(** {1 Iteration} *)

(** [iter f v] applies [f] to each element in order. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [iteri f v] applies [f] to each index and element. *)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** [fold_left f init v] folds left-to-right. *)
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

(** [fold_right f v init] folds right-to-left. *)
val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

(** {1 Search} *)

(** [find p v] returns the first element satisfying [p]. *)
val find : ('a -> bool) -> 'a t -> 'a option

(** [findi p v] returns the first index and element satisfying [p]. *)
val findi : ('a -> bool) -> 'a t -> (int * 'a) option

(** [find_map f v] returns the first [Some] result of [f]. *)
val find_map : ('a -> 'b option) -> 'a t -> 'b option

(** [exists p v] returns [true] if any element satisfies [p]. *)
val exists : ('a -> bool) -> 'a t -> bool

(** [for_all p v] returns [true] if all elements satisfy [p]. *)
val for_all : ('a -> bool) -> 'a t -> bool

(** [mem x v] returns [true] if [x] is in [v]. Uses structural equality by default. *)
val mem : ?equal:('a -> 'a -> bool) -> 'a -> 'a t -> bool

(** [count p v] returns the number of elements satisfying [p]. *)
val count : ('a -> bool) -> 'a t -> int

(** {1 Conversion} *)

(** [to_list v] converts to a list. O(n). *)
val to_list : 'a t -> 'a list

(** [to_array v] converts to an array. O(n). *)
val to_array : 'a t -> 'a array
