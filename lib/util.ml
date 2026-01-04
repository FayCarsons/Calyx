let ( $ ) = ( @@ )
let ( let* ) = Result.bind
let ( >>= ) = Result.bind
let ( >>| ) = Result.map
let ( <$> ) = Option.map

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

module Tuple = struct
  let into a b = a, b
  let intoRev a b = b, a
  let first f (a, b) = f a, b
  let second f (a, b) = a, f b
  let both f (a, b) = f a, f b
  let bimap fa fb (a, b) = fa a, fb b
end
