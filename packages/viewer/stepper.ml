(** Interactive trace stepper: terminal setup, event loop, trace handlers. *)

open Core
open Stepper_model
open Stepper_render
open Stepper_input

let term : Notty_unix.Term.t option ref = ref None
let tui_state : State.t ref = ref State.default
let node_stack : Path.t Stack.t = Stack.create ()

let make_virtual_root () : Trace.node =
  (* Virtual root to hold top-level traces as children. Data is never accessed. *)
  { Trace.data = Obj.magic (); children = Dynarray.create () }
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
            let idx = Dynarray.length virtual_root.Trace.children in
            [ idx ], virtual_root
          | Some parent_path ->
            let parent =
              Option.value_exn (get_node_at_path !tui_state.root parent_path)
            in
            let child_idx = Dynarray.length parent.Trace.children in
            Path.append parent_path child_idx, parent
        in
        let node = { Trace.data = Trace.Pending j; children = Dynarray.create () } in
        Dynarray.add_last parent_node.Trace.children node;
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

let inspect_loop () =
  match !term with
  | None -> ()
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
         | Some (Step Quit) -> ()
         | Some (Step _) -> loop ())
    in
    loop ()
;;

let run ~source ~f =
  let on_start () =
    term := Some (Notty_unix.Term.create ());
    tui_state := { State.default with root = Some (make_virtual_root ()); source };
    Stack.clear node_stack
  in
  let result =
    Trace.handle_interactive ~on_enter ~on_leave ~on_start ~on_finish:(Fun.const ()) ~f
  in
  (* Stay in inspection mode after tracing completes *)
  tui_state := { !tui_state with current_path = Path.empty };
  inspect_loop ();
  on_finish ();
  result
;;
