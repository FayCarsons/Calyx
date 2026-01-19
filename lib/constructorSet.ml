open Core
module S = Hash_set.Make (Ident)

type t =
  { bloom : Bloom.Ident.t
  ; backing : S.t
  }

type impl =
  { add : Ident.t -> unit
  ; member : Ident.t -> bool
  }

module type Impl = sig
  val member : Ident.t -> bool
  val add : Ident.t -> unit
end

module M = struct
  let bloom = Bloom.Ident.create ~size:4096 ~numHashes:3
  let backing = S.create ~size:4096 ~growth_allowed:true ()

  let member : Ident.t -> bool =
    fun ident -> Bloom.Ident.member bloom ident && Hash_set.mem backing ident
  ;;

  let add : Ident.t -> unit =
    fun ident ->
    Bloom.Ident.add bloom ident;
    Hash_set.add backing ident
  ;;
end
