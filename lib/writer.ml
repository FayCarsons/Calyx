module type M = sig
    type t
    val tell : t -> unit
    val handle : (unit -> 'a) -> ('a * t list)
end

module Make (W : sig
    type t
  end) : M with type t = W.t =
struct
  type t = W.t
  type _ Effect.t += Tell : W.t -> unit Effect.t 

  let tell x = Effect.perform (Tell x)
  let handle (f : unit -> 'a) : ('a * W.t list) = 
    let open Effect.Deep in 
    let acc = ref [] in 
    let result =  
      try f () 
        with 
        | effect (Tell x), k -> 
          acc := x :: !acc;
          continue k ()
        | effect e, k ->
          (* print_endline "RERAISE IN WRITER"; *)
          (* Re-perform unhandled effects *)
          continue k (Effect.perform e)
    in
    (result, !acc)
end
