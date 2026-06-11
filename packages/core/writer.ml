module type M = sig
  type t

  val tell : t -> unit
  val handle : (unit -> 'a) -> 'a * t list
end

module Make (W : sig
    type t
  end) : M with type t = W.t = struct
  type t = W.t
  type _ Effect.t += Tell : W.t -> unit Effect.t

  let tell x = Effect.perform (Tell x)

  let handle : type a. (unit -> a) -> a * W.t list = fun f ->
    let open Effect.Deep in
    let run : W.t list -> a * W.t list = 
      match f () with 
      | x -> fun log -> (x, List.rev log)
      | effect (Tell s), k -> 
          fun (log : W.t list) -> continue k () (s :: log)
    in 
    run []
  ;;
end
