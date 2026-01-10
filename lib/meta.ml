module M = struct
  type t = (int[@opaque])

  let equal = Int.equal
  let compare = Int.compare
  let hash = Int.hash
end

module Solutions = Hashtbl.Make (M)
include M

type 'a gen =
  { mutable counter : t
  ; mutable solutions : 'a option Solutions.t
  }

let default () = { counter = 0; solutions = Solutions.create 256 }

type _ Effect.t += Fresh : unit -> t Effect.t | Reset : unit Effect.t

let fresh () = Effect.perform (Fresh ())

let handle (gen : 'a gen) (f : unit -> 'b) : 'b =
  let open Effect.Deep in
  try_with
    f
    ()
    { effc =
        (fun (type c) (eff : c Effect.t) ->
          match eff with
          | Fresh () ->
            Some
              (fun (k : (c, _) continuation) ->
                let next = gen.counter in
                (* Add unsolved to next's entry in solutions map *)
                Solutions.add gen.solutions next None;
                (* Increment counter *)
                gen.counter <- succ gen.counter;
                continue k next)
          | Reset ->
            Some
              (fun (k : (c, _) continuation) ->
                gen.counter <- 0;
                continue k ())
          | eff -> Some (fun (k : (c, _) continuation) -> continue k (Effect.perform eff)))
    }
;;
