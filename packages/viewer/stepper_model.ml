(** Stepper data model: tree paths, per-node UI state, and lookup. *)

open Core

module Path = struct
  type t = int list [@@deriving sexp, compare, equal]

  let empty = []
  let is_empty = List.is_empty
  let append path idx = path @ [ idx ]
  let parent path = List.drop_last path |> Option.value ~default:[]
  let depth path = List.length path
end

module PathMap = Map.Make (struct
    type t = Path.t [@@deriving sexp, compare]
  end)

module Node = struct
  type t =
    { expanded : bool
    ; sexp_visible : bool (* S-expression display, toggled with 'f' *)
    ; context_visible : bool (* Context display, toggled with 'c' *)
    }

  let default = { expanded = true; sexp_visible = false; context_visible = false }
end

module State = struct
  type t =
    { root : Trace.node option
    ; cursor : Path.t
    ; current_path : Path.t
    ; is_enter : bool
    ; step_mode : Trace.step_mode
    ; node_states : Node.t PathMap.t
    ; scroll_offset : int
    ; viewport_height : int
    ; source : string
    }

  let default =
    { root = None
    ; cursor = Path.empty
    ; current_path = Path.empty
    ; is_enter = true
    ; step_mode = Trace.Into
    ; node_states = PathMap.empty
    ; scroll_offset = 0
    ; viewport_height = 20
    ; source = ""
    }
  ;;

  let get_node_state : t -> Path.t -> Node.t =
    fun state path ->
    Map.find state.node_states path |> Option.value ~default:Node.default
  ;;

  let set_node_state : t -> Path.t -> Node.t -> t =
    fun state path node_state ->
    { state with node_states = Map.set state.node_states ~key:path ~data:node_state }
  ;;
end

let get_node_at_path : Trace.node option -> Path.t -> Trace.node option =
  fun root path ->
  let rec go node = function
    | [] -> Some node
    | idx :: rest ->
      let children = node.Trace.children in
      if idx >= 0 && idx < Dynarray.length children
      then go (Dynarray.get children idx) rest
      else None
  in
  Option.bind root ~f:(fun r -> go r path)
;;
