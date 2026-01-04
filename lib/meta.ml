module M = struct
  type t = (int[@opaque])
  let equal = Int.equal
  let compare = Int.compare
  let hash = Int.hash
end

module Solutions = Hashtbl.Make (M)
include M

type 'a gen = { mutable counter : t; mutable solutions : 'a option Solutions.t }
let default ()  = { counter = 0; solutions = Solutions.create 256 }

type _ Effect.t += 
  | Fresh : unit -> t Effect.t 
  | Reset : unit Effect.t

let fresh () = Effect.perform (Fresh ())

let handle (gen : 'a gen) (f : unit -> 'b) : 'b =
  try f () with
  | effect (Fresh ()), k ->
    let next = gen.counter in
    (* Add unsolved to next's entry in solutions map *)
    Solutions.add gen.solutions next None;
    (* Increment counter *)
    gen.counter <- succ gen.counter;
    Effect.Deep.continue k next
  | effect Reset, k -> 
      gen.counter <- 0;
      Effect.Deep.continue k ()
  | effect e, k ->
      (* print_endline "RERAISE IN META"; *)
      (* Re-perform unhandled effects *)
      Effect.Deep.continue k (Effect.perform e)
;;
