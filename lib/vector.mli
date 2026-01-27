(** Resizable vector with GC-safe storage of arbitrary OCaml values.

    Backed by a contiguous C array with O(1) amortized push and O(1) random access.
    All OCaml values are registered as GC roots. *)

type 'a t

(** {1 Construction} *)

val create : ?capacity:int -> unit -> 'a t
(** [create ?capacity ()] creates an empty vector. Default capacity is 64. *)

val of_list : 'a list -> 'a t
(** [of_list xs] creates a vector from a list. *)

val of_array : 'a array -> 'a t
(** [of_array arr] creates a vector from an array. O(n). *)

val copy : 'a t -> 'a t
(** [copy v] returns a shallow copy. O(n). *)

(** {1 Access} *)

val length : 'a t -> int
(** [length v] returns the number of elements. O(1). *)

val capacity : 'a t -> int
(** [capacity v] returns current capacity. O(1). *)

val is_empty : 'a t -> bool
(** [is_empty v] returns [true] if [v] has no elements. O(1). *)

val get : 'a t -> int -> 'a option
(** [get v i] returns element at index [i], or [None] if out of bounds. *)

val unsafe_get : 'a t -> int -> 'a
(** [unsafe_get v i] returns element at index [i]. No bounds check. *)

val first : 'a t -> 'a option
(** [first v] returns the first element, or [None] if empty. *)

val last : 'a t -> 'a option
(** [last v] returns the last element, or [None] if empty. *)

(** {1 Modification} *)

val push : 'a t -> 'a -> unit
(** [push v x] appends [x] to the end. O(1) amortized. *)

val pop : 'a t -> 'a option
(** [pop v] removes and returns the last element, or [None] if empty. *)

val set : 'a t -> int -> 'a -> bool
(** [set v i x] sets element at [i] to [x]. Returns [false] if out of bounds. *)

val unsafe_set : 'a t -> int -> 'a -> unit
(** [unsafe_set v i x] sets element at [i] to [x]. No bounds check. *)

val clear : 'a t -> unit
(** [clear v] removes all elements. O(n) for GC root cleanup. *)

val append : 'a t -> 'a t -> unit
(** [append dst src] appends all elements of [src] to [dst]. Mutates [dst]. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] elements.
    @raise Invalid_argument if indices are out of bounds. *)

val rev_in_place : 'a t -> unit
(** [rev_in_place v] reverses [v] in place. O(n). *)

val shrink_to_fit : 'a t -> unit
(** [shrink_to_fit v] reduces capacity to match length. *)

(** {1 Transformation} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f v] applies [f] to each element, returning a new vector. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f v] like [map] but [f] also receives the index. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p v] returns a new vector with elements satisfying [p]. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f v] applies [f] and collects [Some] results. *)

val concat : 'a t -> 'a t -> 'a t
(** [concat a b] returns a new vector with elements of [a] followed by [b]. *)

val rev : 'a t -> 'a t
(** [rev v] returns a new vector with elements in reverse order. *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f v] applies [f] to each element in order. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f v] applies [f] to each index and element. *)

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** [fold_left f init v] folds left-to-right. *)

val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold_right f v init] folds right-to-left. *)

(** {1 Search} *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p v] returns the first element satisfying [p]. *)

val findi : ('a -> bool) -> 'a t -> (int * 'a) option
(** [findi p v] returns the first index and element satisfying [p]. *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f v] returns the first [Some] result of [f]. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p v] returns [true] if any element satisfies [p]. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p v] returns [true] if all elements satisfy [p]. *)

val mem : ?equal:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** [mem x v] returns [true] if [x] is in [v]. Uses structural equality by default. *)

val count : ('a -> bool) -> 'a t -> int
(** [count p v] returns the number of elements satisfying [p]. *)

(** {1 Conversion} *)

val to_list : 'a t -> 'a list
(** [to_list v] converts to a list. O(n). *)

val to_array : 'a t -> 'a array
(** [to_array v] converts to an array. O(n). *)
