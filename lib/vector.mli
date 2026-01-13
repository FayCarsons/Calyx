(** Resizable vector with GC-safe storage of arbitrary OCaml values. *)

type 'a t

val create : ?capacity:int -> unit -> 'a t
(** [create ?capacity ()] creates an empty vector.
    Default capacity is 64 elements. *)

val push : 'a t -> 'a -> unit
(** [push v x] appends [x] to the end of [v]. *)

val pop : 'a t -> 'a option
(** [pop v] removes and returns the last element, or [None] if empty. *)

val get : 'a t -> int -> 'a option
(** [get v i] returns the element at index [i], or [None] if out of bounds. *)

val set : 'a t -> int -> 'a -> bool
(** [set v i x] sets the element at index [i] to [x].
    Returns [true] on success, [false] if out of bounds. *)

val length : 'a t -> int
(** [length v] returns the number of elements in [v]. *)

val capacity : 'a t -> int
(** [capacity v] returns the current capacity of [v]. *)

val is_empty : 'a t -> bool
(** [is_empty v] returns [true] if [v] has no elements. *)
