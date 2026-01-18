open Core

let topsort : Ident.t list Ident.Map.t -> Ident.t list =
  fun deps ->
  let find : Ident.t list -> int Lazy.t Ident.Map.t -> int =
    fun dependencies depths ->
    List.map ~f:(fun dep -> Lazy.force (Map.find_exn depths dep)) dependencies
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
    |> succ
  in
  let rec depths () =
    Map.mapi ~f:(fun ~key:_ ~data -> lazy (find data (depths ()))) deps
  in
  let compare : Ident.t -> Ident.t -> int =
    fun a b ->
    Int.compare
      (Lazy.force (Map.find_exn (depths ()) a))
      (Lazy.force (Map.find_exn (depths ()) b))
  in
  Map.keys deps |> List.sort ~compare
;;
