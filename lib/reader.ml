module type M = sig
  type t

  val ask : unit -> t
  val local : f:(t -> t) -> (unit -> 'a) -> 'a
  val handle : ?init:t -> (unit -> 'a) -> 'a
end

module Make (M : sig
    type t

    val default : unit -> t
  end) : M with type t = M.t = struct
  type t = M.t

  type _ Effect.t +=
    | Ask : M.t Effect.t
    | Local : (M.t -> M.t) * (unit -> 'a) -> 'a Effect.t

  let ask () = Effect.perform Ask
  let local ~f go = Effect.perform (Local (f, go))

  let handle : ?init:M.t -> (unit -> 'a) -> 'a =
    fun ?(init = M.default ()) comp ->
    let open Effect.Deep in
    let rec go : type b. M.t -> (unit -> b) -> b =
      fun env thunk ->
      try_with
        thunk
        ()
        { effc =
            (fun (type c) (eff : c Effect.t) ->
              match eff with
              | Ask -> Some (fun (k : (c, b) continuation) -> continue k env)
              | Local (f, inner_thunk) ->
                Some
                  (fun (k : (c, b) continuation) ->
                    let result = go (f env) inner_thunk in
                    continue k result)
              | _ -> None (* Re-raise unhandled effects *))
        }
    in
    go init comp
  ;;
end
