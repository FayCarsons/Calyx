open Core

let ( $ ) = ( @@ )
let ( let* ) = Result.Let_syntax.( >>= )
let ( and* ) x f = Core.Result.bind x ~f
let ( >>= ) x f = Core.Result.bind x ~f
let ( >>| ) x f = Core.Result.map x ~f
let ( <$> ) f x = Core.Option.map x ~f

let ( <*> ) f x =
  match f with
  | Some f ->
    (match x with
     | Some x -> Some (f x)
     | _ -> None)
  | _ -> None
;;

let is_some_and (f : 'a -> bool) : 'a option -> bool = function
  | Some x -> f x
  | None -> false
;;

let rec windows len = function
  | _ :: _ as xs -> Core.List.take xs len :: windows len (Core.List.tl_exn xs)
  | [] -> []
;;

let rec pairs = function
  | x :: (y :: _ as rest) -> (x, y) :: pairs rest
  | _ -> []
;;

let rec sequence = function
  | Ok x :: xs -> Core.Result.map (sequence xs) ~f:(fun rest -> x :: rest)
  | [] -> Ok []
  | Error e :: _ -> Error e
;;

module Tuple = struct
  let into a b = a, b
  let intoRev a b = b, a
  let first f (a, b) = f a, b
  let second f (a, b) = a, f b
  let both f (a, b) = f a, f b
  let bimap fa fb (a, b) = fa a, fb b
end
