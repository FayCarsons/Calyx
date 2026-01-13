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

module IdentMap = Ident.Map

type t =
  { bindings : entry IdentMap.t
  ; pos : Pos.t
  ; level : int
  }

let default () =
  { bindings = IdentMap.empty
  ; pos = { filename = "<TEST>"; line = 0L; col = 0L }
  ; level = 0
  }
;;

let env : t ref = ref @@ default ()
let init e = env := e
let ask () = !env

let local ~f thunk =
  let old = !env in
  env := f old;
  Fun.protect thunk ~finally:(fun () -> env := old)
;;

let lookup ident =
  let env = ask () in
  IdentMap.find_opt ident env.bindings
;;

let lookup_value : Ident.t -> Term.value option = fun ident -> value <$> lookup ident

let lookup_type : Ident.t -> Term.value option =
  fun ident -> Option.bind (lookup ident) typ
;;

let pos () =
  let env = ask () in
  env.pos
;;

let with_pos (pos : Pos.t) (env : t) = { env with pos }

let level () =
  let env = ask () in
  env.level
;;

let level_succ ({ level; _ } as env) = { env with level = succ level }

let with_binding
      (ident : Ident.t)
      ~(value : Term.value)
      ?(typ : Term.value option)
      ({ bindings; _ } as env)
  =
  let binding =
    match typ with
    | Some t -> Typed (value, t)
    | None -> Untyped value
  in
  { env with bindings = IdentMap.add ident binding bindings }
;;

let from_bindings bindings =
  let default = default () in
  { default with bindings }
;;

let fresh_var name typ f =
  let lvl = level () in
  let value = `Neutral (Term.NVar (lvl, name)) in
  local ~f:(Fun.compose level_succ (with_binding name ~value ~typ)) (fun () -> f value)
;;
