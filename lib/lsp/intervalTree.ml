open Core

module type Key = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val low : t -> int
  val high : t -> int
end

module type M = sig
  type key
  type 'a t

  val empty : 'a t
  val of_list : (key * 'a) list -> 'a t
  val insert : key -> 'a -> 'a t -> 'a t
  val overlaps : key -> 'a t -> (key * 'a) list
end

module Make (Key : Key) : M with type key = Key.t = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Node of
        { left : 'a t
        ; interval : key
        ; height : int
        ; max_high : int
        ; data : 'a
        ; right : 'a t
        }

  let empty = Empty

  let[@inline] height = function
    | Empty -> 0
    | Node { height; _ } -> height
  ;;

  let[@inline] max_high = function
    | Empty -> Int.min_value
    | Node { max_high; _ } -> max_high
  ;;

  let[@inline] node : key -> 'a -> left:'a t -> right:'a t -> 'a t =
    fun interval data ~left ~right ->
    let height = succ (Int.max (height left) (height right))
    and max_high =
      Int.max (Key.high interval) (Int.max (max_high left) (max_high right))
    in
    Node { left; interval; height; max_high; data; right }
  ;;

  let[@inline] balance = function
    | Empty -> 0
    | Node { left; right; _ } -> height left - height right
  ;;

  let rotate_right = function
    | Node ({ left = Node nl; _ } as n) ->
      node
        nl.interval
        nl.data
        ~left:nl.left
        ~right:(node n.interval n.data ~left:nl.right ~right:n.right)
    | t -> t
  ;;

  let rotate_left = function
    | Node ({ right = Node nr; _ } as n) ->
      node
        nr.interval
        nr.data
        ~left:(node n.interval n.data ~left:n.left ~right:nr.left)
        ~right:nr.right
    | t -> t
  ;;

  let rebalance = function
    | Empty -> Empty
    | Node n ->
      let balance_factor = balance (Node n) in
      if balance_factor > 1
      then
        (* Left heavy *)
        if balance n.left < 0
        then
          rotate_right (node n.interval n.data ~left:(rotate_left n.left) ~right:n.right)
        else rotate_right (Node n)
      else if balance_factor < -1
      then
        (* Right heavy *)
        if balance n.right > 0
        then
          rotate_left (node n.interval n.data ~left:n.left ~right:(rotate_right n.right))
        else rotate_left (Node n)
      else Node n
  ;;

  let rec insert : key -> 'a -> 'a t -> 'a t =
    fun interval data -> function
    | Empty ->
      Node
        { interval
        ; data
        ; max_high = Key.high interval
        ; height = 1
        ; left = Empty
        ; right = Empty
        }
    | Node n ->
      rebalance
      @@
      if Key.low interval <= Key.low n.interval
      then node n.interval n.data ~left:(insert interval data n.left) ~right:n.right
      else node n.interval n.data ~left:n.left ~right:(insert interval data n.right)
  ;;

  let of_list : (Key.t * 'a) list -> 'a t =
    fun xs -> List.fold_right ~f:(Tuple2.uncurry insert) ~init:empty xs
  ;;

  let fold_overlaps interval ~f ~init =
    let rec go acc = function
      | Empty -> acc
      | Node n ->
        if n.max_high < Key.low interval
        then acc
        else (
          let acc = go acc n.left in
          let acc =
            if
              Key.low n.interval <= Key.high interval
              && Key.high n.interval >= Key.low interval
            then f acc n.interval n.data
            else acc
          in
          if Key.low n.interval > Key.high interval then acc else go acc n.right)
    in
    go init
  ;;

  let overlaps : key -> 'a t -> (key * 'a) list =
    fun interval ->
    fold_overlaps interval ~f:(fun acc interval data -> (interval, data) :: acc) ~init:[]
  ;;
end

open Calyx

(** Position key for interval tree.
    Note: Pos.column is actually Menhir's pos_cnum (absolute character offset),
    which is ideal for interval tree operations as it's a single monotonic integer. *)
module PosKey : Key with type t = Pos.t = struct
  type t = Pos.t

  let compare a b =
    let cmp_lo = Int.compare a.Pos.lo.column b.Pos.lo.column in
    if cmp_lo <> 0 then cmp_lo else Int.compare a.Pos.hi.column b.Pos.hi.column
  ;;

  let equal a b = a.Pos.lo.column = b.Pos.lo.column && a.Pos.hi.column = b.Pos.hi.column
  let low t = t.Pos.lo.column
  let high t = t.Pos.hi.column
end

(** source position tree *)
module Positions = Make (PosKey)
