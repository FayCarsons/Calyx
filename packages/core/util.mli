(** Small general-purpose combinators shared across the compiler. *)

(** Applicative [<*>] over [option]. *)
val ( <*> ) : ('a -> 'b) option -> 'a option -> 'b option

(** [is_some_and p o] is [true] when [o] is [Some x] and [p x] holds. *)
val is_some_and : ('a -> bool) -> 'a option -> bool

(** [windows n xs] yields every length-[n] sliding window of [xs]. *)
val windows : int -> 'a list -> 'a list list

(** [pairs xs] yields each adjacent pair of [xs]. *)
val pairs : 'a list -> ('a * 'a) list

(** [sequence rs] collapses a list of results into a result of list,
    short-circuiting on the first [Error]. *)
val sequence : ('a, 'e) result list -> ('a list, 'e) result

module Tuple : sig
  val into : 'a -> 'b -> 'a * 'b
  val into_rev : 'a -> 'b -> 'b * 'a
  val first : ('a -> 'c) -> 'a * 'b -> 'c * 'b
  val second : ('b -> 'c) -> 'a * 'b -> 'a * 'c
  val both : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val bimap : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
end
