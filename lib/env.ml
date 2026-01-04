type entry =
  | Untyped of Term.value
  | Typed of Term.value * Term.value

let value : entry -> Term.value = function
  | Untyped v -> v
  | Typed (v, _) -> v
;;

let typ : entry -> Term.value option = function
  | Typed (_, t) -> Some t
  | _ -> None
;;

type t =
  { mutable bindings : (Ident.t * entry) list
  ; mutable pos : Pos.t
  ; mutable level : int
  }

let default () =
  { bindings = []; pos = { filename = "<TEST>"; line = 0L; col = 0L }; level = 0 }
;;

type _ Effect.t +=
  | Lookup : Ident.t -> entry Effect.t
  | Pushy : (Ident.t * entry) -> unit Effect.t
  | Pop : unit -> unit Effect.t
  | SetPos : Pos.t -> unit Effect.t
  | GetPos : Pos.t Effect.t
  | Level : int Effect.t

let ( >> ) f g = Fun.compose g f
let lookup : Ident.t -> entry = fun ident -> Effect.perform (Lookup ident)
let lookup_value : Ident.t -> Term.value = lookup >> value
let lookup_type : Ident.t -> Term.value option = lookup >> typ
let get_pos : unit -> Pos.t = fun () -> Effect.perform GetPos
let set_pos pos = Effect.perform (SetPos pos)
let level () = Effect.perform Level

let local : Ident.t -> entry -> (unit -> 'a) -> 'a =
  fun ident entry f ->
  Effect.perform (Pushy (ident, entry));
  Fun.protect f ~finally:(fun () -> Effect.perform (Pop ()))
;;

let local_typed ident ~value ~ty f = local ident (Typed (value, ty)) f
let local_untyped ident ~value f = local ident (Untyped value) f

let add_typed env ident ~value ~typ =
  env.bindings <- (ident, Typed (value, typ)) :: env.bindings
;;

let from_bindings bindings =
  let def = default () in
  { def with bindings }
;;

let fresh_var : Ident.t -> Term.value -> (Term.value -> 'a) -> 'a =
  fun name ty f ->
  let lvl = level () in
  let var = `Neutral (Term.NVar (lvl, name)) in
  local name (Typed (var, ty)) (fun () -> f var)
;;

let handle ?(env = default ()) (f : unit -> 'a) : 'a =
  let open Effect.Deep in
  print_endline "Env.handle";
  try_with
    f
    ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Level ->
            Some
              (fun (k : (a, _) continuation) ->
                print_endline "LEVEL";
                continue k env.level)
          | Lookup ident ->
            Some
              (fun (k : (a, _) continuation) ->
                print_endline "LOOKUP";
                match List.assoc_opt ident env.bindings with
                | Some entry -> continue k entry
                | None -> failwith @@ Printf.sprintf "Variable %s not in scope" ident)
          | Pushy (ident, binding) ->
            Some
              (fun (k : (a, _) continuation) ->
                print_endline "PUSH";
                env.bindings <- (ident, binding) :: env.bindings;
                continue k ())
          | Pop _ ->
            Some
              (fun (k : (a, _) continuation) ->
                print_endline "POP";
                env.bindings <- List.tl env.bindings;
                continue k ())
          | SetPos pos ->
            Some
              (fun (k : (a, _) continuation) ->
                print_endline "SET_POS";
                env.pos <- pos;
                continue k ())
          | GetPos ->
            Some
              (fun (k : (a, _) continuation) ->
                print_endline "GET_POS";
                continue k env.pos)
          | _ -> None (* Re-raise unhandled effects *))
    }
;;
