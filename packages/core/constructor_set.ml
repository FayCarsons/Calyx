open Core
module Set = Ident.Set

module M = struct
  let backing = Set.create ~size:4096 ~growth_allowed:true ()
  let member : Ident.t -> bool = fun ident -> Hash_set.mem backing ident
  let add : Ident.t -> unit = fun ident -> Hash_set.add backing ident
end
