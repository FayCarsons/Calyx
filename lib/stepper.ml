open Core
module A = Notty.A
module I = Notty.I

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
      (match Vector.get node.Trace.children idx with
       | Some child -> go child rest
       | None -> None)
  in
  Option.bind root ~f:(fun r -> go r path)
;;

let stage_name : type i o. (i, o) Trace.stage -> string = function
  | Trace.Infer -> "Infer"
  | Trace.Check _ -> "Check"
  | Trace.Eval -> "Eval"
  | Trace.Quote -> "Quote"
  | Trace.Unify -> "Unify"
;;

let stage_color : type i o. (i, o) Trace.stage -> A.t = function
  | Trace.Infer -> A.(fg lightblue)
  | Trace.Check _ -> A.(fg lightgreen)
  | Trace.Eval -> A.(fg lightyellow)
  | Trace.Quote -> A.(fg lightmagenta)
  | Trace.Unify -> A.(fg lightcyan)
;;

let format_focus : type i o. (i, o) Trace.stage -> i -> string =
  fun stage focus ->
  let sexp = Trace.sexp_of_focus stage focus in
  Sexp.to_string_hum sexp
;;

let format_location (loc : Pos.t) =
  Printf.sprintf "[%d:%d -> %d:%d]" loc.lo.line loc.lo.column loc.hi.line loc.hi.column
;;

let slice_source source (loc : Pos.t) =
  let start = loc.lo.column in
  let len = loc.hi.column - loc.lo.column in
  if start >= 0 && len > 0 && start + len <= String.length source
  then (
    let snippet = String.sub source ~pos:start ~len in
    (* Take up to first newline for compact preview *)
    match String.lsplit2 snippet ~on:'\n' with
    | Some (first_line, _) ->
      let trimmed = String.rstrip first_line in
      if String.length trimmed < String.length snippet then trimmed ^ " ..." else trimmed
    | None -> snippet)
  else "<source unavailable>"
;;

type line =
  { node : Trace.node
  ; path : Path.t
  ; depth : int
  ; is_last : bool
  ; ancestors_last : bool list
  }

let flatten_visible state =
  let rec go ~path ~depth ~ancestors_last ~is_last node acc =
    let line = { node; path; depth; is_last; ancestors_last } in
    let acc = line :: acc in
    let node_state = State.get_node_state state path in
    if node_state.expanded
    then (
      let n_children = Vector.length node.Trace.children in
      let new_ancestors =
        if depth > 0 then is_last :: ancestors_last else ancestors_last
      in
      (* TODO: Add Vector.fold_lefti to vector.ml to avoid this *)
      let children = Vector.to_list node.Trace.children in
      List.foldi children ~init:acc ~f:(fun i acc child ->
        let child_path = Path.append path i in
        let child_is_last = i = n_children - 1 in
        go
          ~path:child_path
          ~depth:(depth + 1)
          ~ancestors_last:new_ancestors
          ~is_last:child_is_last
          child
          acc))
    else acc
  in
  match state.root with
  | None -> []
  | Some virtual_root ->
    (* Skip the virtual root, render its children (top-level traces) directly *)
    let n_children = Vector.length virtual_root.Trace.children in
    let children = Vector.to_list virtual_root.Trace.children in
    List.foldi children ~init:[] ~f:(fun i acc child ->
      let child_path = [ i ] in
      let child_is_last = i = n_children - 1 in
      go ~path:child_path ~depth:0 ~ancestors_last:[] ~is_last:child_is_last child acc)
    |> List.rev
;;

let tree_prefix ~depth ~is_last ~ancestors_last =
  if depth = 0
  then ""
  else (
    let ancestor_parts =
      List.rev ancestors_last
      |> List.map ~f:(fun last -> if last then "    " else "|   ")
      |> String.concat
    in
    let connector = if is_last then "`-- " else "|-- " in
    ancestor_parts ^ connector)
;;

let render_line state line =
  let { node; path; depth; is_last; ancestors_last } = line in
  let node_state = State.get_node_state state path in
  let is_cursor = Path.equal path state.cursor in
  let is_current = Path.equal path state.current_path in
  let prefix = tree_prefix ~depth ~is_last ~ancestors_last in
  let marker = if is_current then ">> " else "   " in
  let n_children = Vector.length node.Trace.children in
  let tree_indicator =
    if n_children = 0
    then " "
    else if node_state.expanded
    then "-"
    else Printf.sprintf "+%d" n_children
  in
  let sexp_indicator = if node_state.sexp_visible then "f" else "" in
  let ctx_indicator = if node_state.context_visible then "c" else "" in
  let extra_indicators = sexp_indicator ^ ctx_indicator in
  let extra_indicators =
    if String.is_empty extra_indicators then "" else extra_indicators
  in
  let stage, location, status =
    match node.data with
    | Trace.Pending j ->
      let stage_s = stage_name j.stage in
      let loc_s = format_location j.location in
      stage_s, loc_s, if state.is_enter then "ENTER" else "..."
    | Trace.Complete (j, outcome) ->
      let stage_s = stage_name j.stage in
      let loc_s = format_location j.location in
      let status_s =
        match outcome with
        | Trace.Succeeded _ -> "OK"
        | Trace.Failed _ -> "FAIL"
      in
      stage_s, loc_s, status_s
  in
  let stage_attr =
    match node.data with
    | Trace.Pending j -> stage_color j.stage
    | Trace.Complete (j, _) -> stage_color j.stage
  in
  let status_attr =
    match status with
    | "OK" -> A.(fg lightgreen)
    | "FAIL" -> A.(fg lightred)
    | "ENTER" -> A.(fg lightyellow)
    | _ -> A.(fg (gray 12))
  in
  let base_attr = if is_cursor then A.(st reverse) else A.empty in
  let marker_attr = if is_current then A.(fg lightyellow ++ st bold) else A.empty in
  let indicator_str =
    if String.is_empty extra_indicators
    then Printf.sprintf "[%s]" tree_indicator
    else Printf.sprintf "[%s:%s]" tree_indicator extra_indicators
  in
  I.hcat
    [ I.string marker_attr marker
    ; I.string base_attr prefix
    ; I.string A.(base_attr ++ fg (gray 12)) indicator_str
    ; I.string base_attr " "
    ; I.string A.(base_attr ++ stage_attr ++ st bold) (Printf.sprintf "[%s]" stage)
    ; I.string A.(base_attr ++ fg (gray 10)) (Printf.sprintf " @ %s" location)
    ; I.string base_attr " "
    ; I.string A.(base_attr ++ status_attr) (Printf.sprintf "[%s]" status)
    ]
;;

let render_focus_for_node state line =
  let node_state = State.get_node_state state line.path in
  let indent =
    String.make
      (3
       + String.length
           (tree_prefix
              ~depth:line.depth
              ~is_last:line.is_last
              ~ancestors_last:line.ancestors_last))
      ' '
  in
  let loc =
    match line.node.data with
    | Trace.Pending j -> j.location
    | Trace.Complete (j, _) -> j.location
  in
  (* Always show source snippet *)
  let snippet_line =
    let preview = slice_source state.source loc in
    I.hcat
      [ I.string A.(fg (gray 10)) (indent ^ "src: "); I.string A.(fg lightwhite) preview ]
  in
  (* Optionally show S-expression when toggled with 'f' *)
  let sexp_lines =
    if node_state.sexp_visible
    then (
      let focus_s =
        match line.node.data with
        | Trace.Pending j -> format_focus j.stage j.focus
        | Trace.Complete (j, _) -> format_focus j.stage j.focus
      in
      let lines = String.split_lines focus_s in
      match lines with
      | [] -> []
      | first :: rest ->
        let first_line =
          I.hcat
            [ I.string A.(fg (gray 10)) (indent ^ "ast: ")
            ; I.string A.(fg (gray 14)) first
            ]
        in
        let rest_lines =
          List.map rest ~f:(fun l -> I.string A.(fg (gray 14)) (indent ^ "     " ^ l))
        in
        first_line :: rest_lines)
    else []
  in
  [ snippet_line ] @ sexp_lines
;;

let render_context_for_node state line =
  let node_state = State.get_node_state state line.path in
  if not node_state.context_visible
  then []
  else (
    let ctx =
      match line.node.data with
      | Trace.Pending j -> Lazy.force j.context
      | Trace.Complete (j, _) -> Lazy.force j.context
    in
    let indent =
      String.make
        (3
         + String.length
             (tree_prefix
                ~depth:line.depth
                ~is_last:line.is_last
                ~ancestors_last:line.ancestors_last))
        ' '
    in
    let name_attr = A.(fg lightgreen) in
    let type_attr = A.(fg lightyellow) in
    let value_attr = A.(fg (gray 14)) in
    if List.is_empty ctx
    then
      [ I.hcat
          [ I.string A.(fg (gray 10)) (indent ^ "ctx: ")
          ; I.string A.(fg (gray 12)) "(empty)"
          ]
      ]
    else (
      let max_name_len =
        List.fold ctx ~init:0 ~f:(fun acc (name, _, _) -> max acc (String.length name))
      in
      List.mapi ctx ~f:(fun i (name, value, typ_opt) ->
        let padded_name = String.pad_right name ~len:max_name_len in
        let typ_s =
          match typ_opt with
          | Some t -> Sexp.to_string_hum (Term.sexp_of_value t)
          | None -> "?"
        in
        let value_s = Sexp.to_string_hum (Term.sexp_of_value value) in
        let label = if i = 0 then "ctx: " else "     " in
        I.hcat
          [ I.string A.(fg (gray 10)) (indent ^ label)
          ; I.string name_attr padded_name
          ; I.string type_attr (" : " ^ typ_s)
          ; I.string value_attr (" = " ^ value_s)
          ])))
;;

let node_render_height state line =
  let node_state = State.get_node_state state line.path in
  (* Snippet is always 1 line *)
  let snippet_height = 1 in
  (* S-expression can be multiple lines when visible *)
  let sexp_height =
    if node_state.sexp_visible
    then (
      let focus_s =
        match line.node.data with
        | Trace.Pending j -> format_focus j.stage j.focus
        | Trace.Complete (j, _) -> format_focus j.stage j.focus
      in
      List.length (String.split_lines focus_s))
    else 0
  in
  let ctx_height =
    if node_state.context_visible
    then (
      let ctx =
        match line.node.data with
        | Trace.Pending j -> Lazy.force j.context
        | Trace.Complete (j, _) -> Lazy.force j.context
      in
      max 1 (List.length ctx))
    else 0
  in
  1 + snippet_height + sexp_height + ctx_height
;;

let render_help state =
  let mode_str =
    match state.State.step_mode with
    | Trace.Into -> "INTO"
    | Trace.Over -> "OVER"
    | Trace.Out -> "OUT"
    | Trace.Run -> "RUN"
  in
  I.hcat
    [ I.string A.(fg black ++ bg lightblue) (Printf.sprintf " %s " mode_str)
    ; I.string A.empty " "
    ; I.string A.(fg lightcyan ++ st bold) "s"
    ; I.string A.(fg white) ":into "
    ; I.string A.(fg lightcyan ++ st bold) "n"
    ; I.string A.(fg white) ":over "
    ; I.string A.(fg lightcyan ++ st bold) "o"
    ; I.string A.(fg white) ":out "
    ; I.string A.(fg lightcyan ++ st bold) "r"
    ; I.string A.(fg white) ":run "
    ; I.string A.(fg lightcyan ++ st bold) "Enter"
    ; I.string A.(fg white) ":step"
    ; I.string A.(fg (gray 8)) " | "
    ; I.string A.(fg lightcyan ++ st bold) "hjkl"
    ; I.string A.(fg white) ":nav "
    ; I.string A.(fg lightcyan ++ st bold) "Space"
    ; I.string A.(fg white) ":tree "
    ; I.string A.(fg lightcyan ++ st bold) "f"
    ; I.string A.(fg white) ":focus "
    ; I.string A.(fg lightcyan ++ st bold) "c"
    ; I.string A.(fg white) ":ctx"
    ; I.string A.(fg (gray 8)) " | "
    ; I.string A.(fg lightcyan ++ st bold) "q"
    ; I.string A.(fg white) ":quit"
    ]
;;

let render state =
  let all_lines = flatten_visible state in
  (* Compute total line height for scroll indicator *)
  let total_height =
    List.fold all_lines ~init:0 ~f:(fun acc line -> acc + node_render_height state line)
  in
  (* Collect visible nodes based on line height, not node count *)
  let rec collect_visible scroll_remaining height_remaining acc = function
    | [] -> List.rev acc
    | line :: rest ->
      let h = node_render_height state line in
      if scroll_remaining > 0
      then (
        (* Still scrolling past lines *)
        let skip = min scroll_remaining h in
        collect_visible (scroll_remaining - skip) height_remaining acc rest)
      else if height_remaining <= 0
      then List.rev acc
      else collect_visible 0 (height_remaining - h) (line :: acc) rest
  in
  let visible_lines =
    collect_visible state.scroll_offset state.viewport_height [] all_lines
  in
  let line_images =
    List.concat_map visible_lines ~f:(fun line ->
      let main = render_line state line in
      let focus = render_focus_for_node state line in
      let ctx = render_context_for_node state line in
      [ main ] @ focus @ ctx)
  in
  let scroll_info =
    if total_height > state.viewport_height
    then
      Printf.sprintf
        " [lines %d-%d of %d]"
        (state.scroll_offset + 1)
        (min (state.scroll_offset + state.viewport_height) total_height)
        total_height
    else ""
  in
  I.vcat
    [ I.string A.(fg white ++ st bold) ("Fingerpaint Type Checker Stepper" ^ scroll_info)
    ; I.string A.(fg (gray 8)) (String.make 78 '-')
    ; I.vcat line_images
    ; I.string A.(fg (gray 8)) (String.make 78 '-')
    ; render_help state
    ]
;;

module Action = struct
  type t =
    | Nav of nav_action
    | Step of step_action

  and nav_action =
    | Up
    | Down
    | Left
    | Right
    | ToggleExpand
    | ToggleFocus
    | ToggleContext
    | JumpTop
    | JumpBottom
    | PageUp
    | PageDown

  and step_action =
    | StepInto
    | StepOver
    | StepOut
    | Run
    | StepDefault
    | Quit

  let of_key = function
    | `Key (`ASCII 'k', []) | `Key (`Arrow `Up, []) -> Some (Nav Up)
    | `Key (`ASCII 'j', []) | `Key (`Arrow `Down, []) -> Some (Nav Down)
    | `Key (`ASCII 'h', []) | `Key (`Arrow `Left, []) -> Some (Nav Left)
    | `Key (`ASCII 'l', []) | `Key (`Arrow `Right, []) -> Some (Nav Right)
    | `Key (`ASCII ' ', []) -> Some (Nav ToggleExpand)
    | `Key (`ASCII 'f', []) -> Some (Nav ToggleFocus)
    | `Key (`ASCII 'c', []) -> Some (Nav ToggleContext)
    | `Key (`ASCII 'g', []) -> Some (Nav JumpTop)
    | `Key (`ASCII 'G', []) -> Some (Nav JumpBottom)
    | `Key (`Page `Up, []) | `Key (`ASCII 'u', [ `Ctrl ]) -> Some (Nav PageUp)
    | `Key (`Page `Down, []) | `Key (`ASCII 'd', [ `Ctrl ]) -> Some (Nav PageDown)
    | `Key (`ASCII 's', []) -> Some (Step StepInto)
    | `Key (`ASCII 'n', []) -> Some (Step StepOver)
    | `Key (`ASCII 'o', []) -> Some (Step StepOut)
    | `Key (`ASCII 'r', []) -> Some (Step Run)
    | `Key (`Enter, []) -> Some (Step StepDefault)
    | `Key (`ASCII 'q', []) | `Key (`Escape, []) -> Some (Step Quit)
    | _ -> None
  ;;

  let find_cursor_node_index state =
    let lines = flatten_visible state in
    List.find_mapi lines ~f:(fun i line ->
      if Path.equal line.path state.cursor then Some i else None)
  ;;

  let find_cursor_line_position state =
    let lines = flatten_visible state in
    let rec go cumulative = function
      | [] -> None
      | line :: rest ->
        let height = node_render_height state line in
        if Path.equal line.path state.cursor
        then Some (cumulative, height)
        else go (cumulative + height) rest
    in
    go 0 lines
  ;;

  let ensure_cursor_visible state =
    match find_cursor_line_position state with
    | None -> state
    | Some (cursor_start, cursor_height) ->
      let cursor_end = cursor_start + cursor_height in
      let new_offset =
        if cursor_start < state.scroll_offset
        then cursor_start
        else if cursor_end > state.scroll_offset + state.viewport_height
        then cursor_end - state.viewport_height
        else state.scroll_offset
      in
      { state with scroll_offset = new_offset }
  ;;

  let move_cursor_up state =
    let lines = flatten_visible state in
    match find_cursor_node_index state with
    | None -> state
    | Some 0 -> state
    | Some idx ->
      let prev_line = List.nth_exn lines (idx - 1) in
      { state with cursor = prev_line.path } |> ensure_cursor_visible
  ;;

  let move_cursor_down state =
    let lines = flatten_visible state in
    match find_cursor_node_index state with
    | None -> state
    | Some idx when idx >= List.length lines - 1 -> state
    | Some idx ->
      let next_line = List.nth_exn lines (idx + 1) in
      { state with cursor = next_line.path } |> ensure_cursor_visible
  ;;

  let move_cursor_left : State.t -> State.t =
    fun state ->
    (* Don't go above top-level nodes (path length 1) *)
    if List.length state.State.cursor <= 1
    then state
    else { state with cursor = Path.parent state.cursor } |> ensure_cursor_visible
  ;;

  let move_cursor_right : State.t -> State.t =
    fun state ->
    match get_node_at_path state.root state.cursor with
    | None -> state
    | Some node ->
      let node_state = State.get_node_state state state.cursor in
      if node_state.expanded && Vector.length node.Trace.children > 0
      then { state with cursor = Path.append state.cursor 0 } |> ensure_cursor_visible
      else state
  ;;

  let toggle_expand : State.t -> State.t =
    fun state ->
    let node_state = State.get_node_state state state.cursor in
    State.set_node_state
      state
      state.cursor
      { node_state with expanded = not node_state.expanded }
  ;;

  let toggle_sexp : State.t -> State.t =
    fun state ->
    let node_state = State.get_node_state state state.cursor in
    State.set_node_state
      state
      state.cursor
      { node_state with sexp_visible = not node_state.sexp_visible }
  ;;

  let toggle_context : State.t -> State.t =
    fun state ->
    let node_state = State.get_node_state state state.cursor in
    State.set_node_state
      state
      state.cursor
      { node_state with context_visible = not node_state.context_visible }
  ;;

  let jump_to_top : State.t -> State.t =
    fun state -> { state with cursor = [ 0 ]; scroll_offset = 0 }
  ;;

  let jump_to_bottom : State.t -> State.t =
    fun state ->
    let lines = flatten_visible state in
    match List.last lines with
    | None -> state
    | Some line ->
      let total_height =
        List.fold lines ~init:0 ~f:(fun acc l -> acc + node_render_height state l)
      in
      let new_offset = max 0 (total_height - state.viewport_height) in
      { state with cursor = line.path; scroll_offset = new_offset }
  ;;

  let interpret state = function
    | Up -> move_cursor_up state
    | Down -> move_cursor_down state
    | Left -> move_cursor_left state
    | Right -> move_cursor_right state
    | ToggleExpand -> toggle_expand state
    | ToggleFocus -> toggle_sexp state
    | ToggleContext -> toggle_context state
    | JumpTop -> jump_to_top state
    | JumpBottom -> jump_to_bottom state
    | PageUp ->
      let new_offset = max 0 (state.scroll_offset - state.viewport_height) in
      { state with scroll_offset = new_offset }
    | PageDown ->
      let lines = flatten_visible state in
      let total_height =
        List.fold lines ~init:0 ~f:(fun acc l -> acc + node_render_height state l)
      in
      let max_offset = max 0 (total_height - state.viewport_height) in
      let new_offset = min max_offset (state.scroll_offset + state.viewport_height) in
      { state with scroll_offset = new_offset }
  ;;
end

let term : Notty_unix.Term.t option ref = ref None
let tui_state : State.t ref = ref State.default
let node_stack : Path.t Stack.t = Stack.create ()

let make_virtual_root () : Trace.node =
  (* Virtual root to hold top-level traces as children. Data is never accessed. *)
  { Trace.data = Obj.magic (); children = Vector.create () }
;;

let on_finish () =
  Option.iter !term ~f:Notty_unix.Term.release;
  term := None
;;

let render_and_get_action () : Trace.step_action =
  match !term with
  | None -> Trace.Continue
  | Some t ->
    let rec loop () =
      let _, h = Notty_unix.Term.size t in
      tui_state := { !tui_state with viewport_height = h - 4 };
      tui_state := Action.ensure_cursor_visible !tui_state;
      let img = render !tui_state in
      Notty_unix.Term.image t img;
      match Notty_unix.Term.event t with
      | `Resize _ -> loop ()
      | event ->
        (match Action.of_key event with
         | None -> loop ()
         | Some (Nav nav) ->
           tui_state := Action.interpret !tui_state nav;
           loop ()
         | Some (Step StepInto) ->
           tui_state := { !tui_state with step_mode = Trace.Into };
           Trace.StepInto
         | Some (Step StepOver) ->
           tui_state := { !tui_state with step_mode = Trace.Over };
           Trace.StepOver
         | Some (Step StepOut) ->
           tui_state := { !tui_state with step_mode = Trace.Out };
           Trace.StepOut
         | Some (Step Run) ->
           tui_state := { !tui_state with step_mode = Trace.Run };
           Trace.Continue
         | Some (Step StepDefault) ->
           (match !tui_state.step_mode with
            | Trace.Into -> Trace.StepInto
            | Trace.Over -> Trace.StepOver
            | Trace.Out -> Trace.StepOut
            | Trace.Run -> Trace.Continue)
         | Some (Step Quit) -> Trace.Abort)
    in
    loop ()
;;

let on_enter : Trace.enter_handler =
  { on_enter =
      (fun ~depth:_ ~mode:_ j ->
        let current_path, parent_node =
          match Stack.top node_stack with
          | None ->
            (* Top-level trace: add as child of virtual root *)
            let virtual_root = Option.value_exn !tui_state.root in
            let idx = Vector.length virtual_root.Trace.children in
            [ idx ], virtual_root
          | Some parent_path ->
            let parent =
              Option.value_exn (get_node_at_path !tui_state.root parent_path)
            in
            let child_idx = Vector.length parent.Trace.children in
            Path.append parent_path child_idx, parent
        in
        let node = { Trace.data = Trace.Pending j; children = Vector.create () } in
        Vector.push parent_node.Trace.children node;
        Stack.push node_stack current_path;
        tui_state
        := { !tui_state with current_path; cursor = current_path; is_enter = true };
        render_and_get_action ())
  }
;;

let on_leave : Trace.leave_handler =
  { on_leave =
      (fun ~depth:_ ~mode:_ j outcome ->
        let current_path =
          match Stack.pop node_stack with
          | None -> Path.empty
          | Some p -> p
        in
        (match get_node_at_path !tui_state.root current_path with
         | Some node -> node.Trace.data <- Trace.Complete (j, outcome)
         | None -> ());
        tui_state := { !tui_state with current_path; is_enter = false };
        render_and_get_action ())
  }
;;

let run ~source ~f =
  let on_start () =
    term := Some (Notty_unix.Term.create ());
    tui_state := { State.default with root = Some (make_virtual_root ()); source };
    Stack.clear node_stack
  in
  Trace.handle_interactive ~on_enter ~on_leave ~on_start ~on_finish ~f
;;
