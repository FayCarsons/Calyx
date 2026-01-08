open Util

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
  ; mutable types : Ident.t list
  ; mutable pos : Pos.t
  ; mutable level : int
  }

let default () =
  { bindings = []
  ; types = []
  ; pos = { filename = "<TEST>"; line = 0L; col = 0L }
  ; level = 0
  }
;;

type _ Effect.t +=
  | Lookup : Ident.t -> entry option Effect.t
  | Push : (Ident.t * entry) -> unit Effect.t
  | Pop : unit -> unit Effect.t
  | (* Try to find a type with the same structure *)
      LookupTypeName :
      (Ident.t * Term.value) list
      -> Ident.t option Effect.t
  | SetPos : Pos.t -> unit Effect.t
  | GetPos : Pos.t Effect.t
  | Level : int Effect.t

let ( >> ) f g = Fun.compose g f
let lookup : Ident.t -> entry option = fun ident -> Effect.perform (Lookup ident)
let lookup_value : Ident.t -> Term.value option = fun ident -> value <$> lookup ident

let lookup_type : Ident.t -> Term.value option =
  fun ident -> Option.bind (lookup ident) typ
;;

let get_pos : unit -> Pos.t = fun () -> Effect.perform GetPos
let set_pos pos = Effect.perform (SetPos pos)
let level () = Effect.perform Level

let local : Ident.t -> entry -> (unit -> 'a) -> 'a =
  fun ident entry f ->
  Effect.perform (Push (ident, entry));
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
  (* *Do not touch*
     For whatever ungodly reason we *have* to use 'try_with' here as opposed 
     to the syntax sugar for effects, otherwise some effects (I think only if 
     captured in lambdas) can escape the handler even if they are within its 
     scope 
  *)
  try_with
    f
    ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Level ->
            Some
              (fun (k : (a, _) continuation) ->
                (* print_endline "LEVEL"; *)
                continue k env.level)
          | Lookup ident ->
            Some
              (fun k ->
                (* print_endline "LOOKUP"; *)
                continue k @@ List.assoc_opt ident env.bindings)
          | LookupTypeName fields ->
            Some
              (fun k ->
                (* print_endline "LOOKUP TYPENAME"; *)
                continue k
                @@ List.find_opt
                     (fun ident ->
                        match List.assoc_opt ident env.bindings with
                        | Some (Typed (`Row { fields = fields'; _ }, `Type)) ->
                          fields = fields'
                        | _ -> false)
                     env.types)
          | Push (ident, binding) ->
            Printf.printf "Push binding for '%s'\n" ident;
            Some
              (fun k ->
                env.bindings <- (ident, binding) :: env.bindings;
                (match binding with
                 | Typed (_, `Type) -> env.types <- ident :: env.types
                 | _ -> ());
                continue k ())
          | Pop _ ->
            Some
              (fun k ->
                (* print_endline "POP"; *)
                env.bindings <- List.tl env.bindings;
                continue k ())
          | SetPos pos ->
            Some
              (fun k ->
                (* print_endline "SET_POS"; *)
                env.pos <- pos;
                continue k ())
          | GetPos ->
            Some
              (fun k ->
                (* print_endline "GET_POS"; *)
                continue k env.pos)
          | _ -> None (* Re-raise unhandled effects *))
    }
;;
