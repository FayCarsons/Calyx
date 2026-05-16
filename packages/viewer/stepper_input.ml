(** Stepper input: key bindings and navigation actions. *)

open Core
open Stepper_model
open Stepper_render

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
      if node_state.expanded && Dynarray.length node.Trace.children > 0
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
