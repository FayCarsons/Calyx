(** Stepper rendering: trace tree, focus, context and help, as Notty images. *)

open Core
module A = Notty.A
module I = Notty.I
open Stepper_model

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
      let n_children = Dynarray.length node.Trace.children in
      let new_ancestors =
        if depth > 0 then is_last :: ancestors_last else ancestors_last
      in
      let children = Dynarray.to_list node.Trace.children in
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
    let n_children = Dynarray.length virtual_root.Trace.children in
    let children = Dynarray.to_list virtual_root.Trace.children in
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
  let n_children = Dynarray.length node.Trace.children in
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
        | Trace.Failed err -> Printf.sprintf "FAIL: '%s'" (Calyx_error.show err)
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
      List.concat_mapi ctx ~f:(fun i (name, value, typ_opt) ->
        let padded_name = String.pad_right name ~len:max_name_len in
        let typ_s =
          match typ_opt with
          | Some t -> Sexp.to_string_hum (Term.sexp_of_value t)
          | None -> "?"
        in
        let value_s = Sexp.to_string_hum (Term.sexp_of_value value) in
        let label = if i = 0 then "ctx: " else "     " in
        (* Indentation for type continuation lines (after "name : ") *)
        let type_cont_indent =
          String.make (String.length indent + 5 + max_name_len + 3) ' '
        in
        (* Indentation for value continuation lines (after "= ") *)
        let value_cont_indent =
          String.make (String.length indent + 5 + max_name_len + 1) ' '
        in
        let typ_lines = String.split_lines typ_s in
        let value_lines = String.split_lines value_s in
        let header =
          I.hcat
            [ I.string A.(fg (gray 10)) (indent ^ label)
            ; I.string name_attr padded_name
            ; I.string type_attr " : "
            ]
        in
        let typ_first, typ_rest =
          match typ_lines with
          | [] -> "", []
          | first :: rest -> first, rest
        in
        let val_first, val_rest =
          match value_lines with
          | [] -> "()", []
          | first :: rest -> first, rest
        in
        let first_line = I.hcat [ header; I.string type_attr typ_first ] in
        let typ_rest_lines =
          List.map typ_rest ~f:(fun l -> I.string type_attr (type_cont_indent ^ l))
        in
        let eq_line =
          I.hcat
            [ I.string A.empty value_cont_indent; I.string value_attr ("= " ^ val_first) ]
        in
        let val_rest_lines =
          List.map val_rest ~f:(fun l ->
            I.string value_attr (value_cont_indent ^ "  " ^ l))
        in
        [ first_line ] @ typ_rest_lines @ [ eq_line ] @ val_rest_lines)))
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
    [ I.string A.(fg white ++ st bold) ("Calyx Type Checker Stepper" ^ scroll_info)
    ; I.string A.(fg (gray 8)) (String.make 78 '-')
    ; I.vcat line_images
    ; I.string A.(fg (gray 8)) (String.make 78 '-')
    ; render_help state
    ]
;;
