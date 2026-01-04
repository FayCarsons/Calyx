module M = struct
  type t = (string[@opaque])

  let compare = String.compare
  let equal = String.equal
  let hash = String.hash
  let mk (s : string) : t = s
end

module Map = Hashtbl.Make (M)
include M
