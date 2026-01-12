module M = struct
  type t = (string[@opaque])

  let compare : t -> t -> int = String.compare
  let equal : t -> t -> bool = String.equal
  let hash : t -> int = String.hash
  let mk : string -> t = Fun.id
end

module Map = Map.Make (M)
module Table = Hashtbl.Make (M)
include M
