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

  let handle (f : unit -> 'a) : 'a * W.t list =
    let open Effect.Deep in
    let acc = ref [] in
    let result =
      try_with
        f
        ()
        { effc =
            (fun (type c) (eff : c Effect.t) ->
              match eff with
              | Tell x ->
                Some
                  (fun (k : (c, _) continuation) ->
                    acc := x :: !acc;
                    continue k ())
              | eff ->
                Some (fun (k : (c, _) continuation) -> continue k (Effect.perform eff)))
        }
    in
    result, !acc
  ;;
end
