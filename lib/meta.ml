open Core

module M = struct
  type t = (int[@opaque]) [@@deriving show, sexp]

  let equal = Int.equal
  let compare = Int.compare
  let hash = Int.hash

  let t_of_sexp : Sexplib.Sexp.t -> t =
    fun sexp ->
    let s = Sexplib.Conv.string_of_sexp sexp in
    if Char.equal s.[0] '?'
    then int_of_string @@ String.sub s ~pos:1 ~len:(String.length s - 1)
    else raise (Failure "Invalid Meta.t in sexpr")
  ;;

  let sexp_of_t : t -> Sexplib.Sexp.t =
    fun self ->
    string_of_int self |> String.append (String.make 1 '?') |> Sexplib.Conv.sexp_of_string
  ;;
end

module Solutions = Hashtbl.Make (M)
include M

type 'a gen =
  { mutable counter : t
  ; mutable solutions : 'a option Solutions.t
  }

let default () =
  let solutions = Solutions.create () in
  { counter = 0; solutions }
;;

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
                Hashtbl.update gen.solutions next ~f:(Fun.const None) |> ignore;
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
