open Core

type entry =
  | Untyped of Term.value
  | Typed of Term.value * Term.value
[@@deriving sexp]

let entry_value : entry -> Term.value = function
  | Untyped v -> v
  | Typed (v, _) -> v
;;

let entry_typ : entry -> Term.value option = function
  | Typed (_, t) -> Some t
  | _ -> None
;;

type ctx =
  { bindings : entry Ident.Map.t
  ; pos : Pos.t
  ; level : int
  ; meta_gen : Term.Meta.Id.t
  ; errors : CalyxError.t list
  ; constraints : Constraint.t list
  }
[@@deriving sexp]

let empty : ctx =
  { bindings = Ident.Map.empty
  ; pos = Pos.empty
  ; level = 0
  ; meta_gen = 0
  ; errors = []
  ; constraints = []
  }
;;

type 'a t = { run : 'r. ctx -> ('a -> ctx -> 'r) -> (ctx -> 'r) -> 'r }

let pure : 'a -> 'a t = fun x -> { run = (fun ctx ok _ -> ok x ctx) }

let bind : 'a t -> f:('a -> 'b t) -> 'b t =
  fun m ~f ->
  { run = (fun ctx ok err -> m.run ctx (fun a ctx' -> (f a).run ctx' ok err) err) }
;;

let map : f:('a -> 'b) -> 'a t -> 'b t =
  fun ~f m -> { run = (fun ctx ok err -> m.run ctx (fun a ctx' -> ok (f a) ctx') err) }
;;

let map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
  fun ~f ma mb ->
  { run =
      (fun ctx ok err ->
        ma.run ctx (fun a ctx' -> mb.run ctx' (fun b ctx'' -> ok (f a b) ctx'') err) err)
  }
;;

let product : 'a t -> 'b t -> ('a * 'b) t = fun ma mb -> map2 ~f:(fun a b -> a, b) ma mb
let ask : ctx t = { run = (fun ctx ok _err -> ok ctx ctx) }
let asks (f : ctx -> 'a) : 'a t = { run = (fun ctx ok _err -> ok (f ctx) ctx) }

let local : f:(ctx -> ctx) -> 'a t -> 'a t =
  fun ~f m -> { run = (fun ctx ok err -> m.run (f ctx) ok err) }
;;

let tell_error : CalyxError.t -> unit t =
  fun e -> { run = (fun ctx ok _err -> ok () { ctx with errors = e :: ctx.errors }) }
;;

let fail : CalyxError.t -> 'a t =
  fun e -> { run = (fun ctx _ok err -> err { ctx with errors = e :: ctx.errors }) }
;;

let fallible : default:'a -> 'a t -> 'a t =
  fun ~default m -> { run = (fun ctx ok _ -> m.run ctx ok (ok default)) }
;;

let liftR : ('a, CalyxError.t) result -> 'a t =
  fun r ->
  { run =
      (fun ctx ok err ->
        match r with
        | Ok a -> ok a ctx
        | Error e -> err { ctx with errors = e :: ctx.errors })
  }
;;

let liftO : default:'a -> 'a option -> 'a t =
  fun ~default m -> { run = (fun ctx ok _ -> ok (Option.value ~default m) ctx) }
;;

let liftO_or_fail : error:CalyxError.t -> 'a option -> 'a t =
  fun ~error o ->
  { run =
      (fun ctx ok err ->
        match o with
        | Some x -> ok x ctx
        | None -> err { ctx with errors = error :: ctx.errors })
  }
;;

let errors : CalyxError.t list t = asks (fun ctx -> ctx.errors)

let tell_constraint : Constraint.t -> unit t =
  fun c ->
  { run = (fun ctx ok _err -> ok () { ctx with constraints = c :: ctx.constraints }) }
;;

let get_constraints : Constraint.t list t = asks (fun ctx -> ctx.constraints)

let clear_constraints : unit t =
  { run = (fun ctx ok _err -> ok () { ctx with constraints = [] }) }
;;

let fresh_meta : Term.Meta.t t =
  { run =
      (fun ctx ok _err ->
        let id = ctx.meta_gen in
        let meta = Term.Meta.make ~id ~level:ctx.level () in
        ok meta { ctx with meta_gen = succ id })
  }
;;

let level : int t = asks (fun ctx -> ctx.level)
let pos : Pos.t t = asks (fun ctx -> ctx.pos)

let lookup : Ident.t -> entry option t =
  fun ident -> asks (fun ctx -> Map.find ctx.bindings ident)
;;

let lookup_value : Ident.t -> Term.value option t =
  Fun.compose (map ~f:(Option.map ~f:entry_value)) lookup
;;

let lookup_type : Ident.t -> Term.value option t =
  Fun.compose (map ~f:(Option.bind ~f:entry_typ)) lookup
;;

let is_bound : Ident.t -> bool t = Fun.compose (map ~f:Option.is_some) lookup

let with_binding : Ident.t -> value:Term.value -> ?typ:Term.value -> ctx -> ctx =
  fun ident ~value ?typ ctx ->
  let entry =
    match typ with
    | Some t -> Typed (value, t)
    | None -> Untyped value
  in
  { ctx with bindings = Map.set ctx.bindings ~key:ident ~data:entry }
;;

let with_pos : Pos.t -> ctx -> ctx = fun pos ctx -> { ctx with pos }
let incr_level : ctx -> ctx = fun ctx -> { ctx with level = succ ctx.level }

let with_var : Ident.t -> typ:Term.value -> f:(Term.value -> 'a t) -> 'a t =
  fun ident ~typ ~f ->
  { run =
      (fun ctx ok err ->
        let lvl = ctx.level in
        let var = `Neutral (Term.NVar (lvl, ident)) in
        let ctx' =
          { ctx with
            level = lvl + 1
          ; bindings = Map.set ctx.bindings ~key:ident ~data:(Typed (var, typ))
          }
        in
        (f var).run ctx' ok err)
  }
;;

let close : f:('a -> 'b t) -> ('a -> ('b, CalyxError.t) result) t =
  fun ~f ->
  { run =
      (fun ctx ok _err ->
        let frozen_bindings = ctx.bindings in
        let frozen_level = ctx.level in
        let closure x =
          let scoped_ctx =
            { ctx with bindings = frozen_bindings; level = frozen_level }
          in
          (f x).run
            scoped_ctx
            (fun b _ctx' -> Ok b)
            (fun ctx' ->
               match ctx'.errors with
               | e :: _ -> Error e
               | [] -> Error `Todo)
        in
        ok closure ctx)
  }
;;

let run : ctx -> 'a t -> ('a, CalyxError.t list) result * ctx =
  fun ctx m ->
  m.run
    ctx
    (fun a ctx' -> Ok a, ctx')
    (fun ctx' ->
       match ctx'.errors with
       | _ :: _ as es -> Error es, ctx'
       | [] -> assert false)
;;

let start : 'a t -> ('a, CalyxError.t list) result * ctx = fun m -> run empty m
let from_bindings : entry Ident.Map.t -> ctx = fun bindings -> { empty with bindings }

module Syntax = struct
  let ( let* ) m f = bind m ~f
  let ( and* ) m f = bind m ~f
  let ( let+ ) m f = map ~f m
  let ( and+ ) = product
  let ( >>= ) m f = bind m ~f
  let ( >|= ) m f = map ~f m

  let ( <*> ) fm m =
    let* f = fm in
    map ~f m
  ;;
end

let rec sequence (ms : 'a t list) : 'a list t =
  match ms with
  | [] -> pure []
  | m :: rest ->
    let open Syntax in
    let* x = m in
    let* xs = sequence rest in
    pure (x :: xs)
;;

let traverse : f:('a -> 'b t) -> 'a list -> 'b list t =
  fun ~f xs -> sequence @@ List.map ~f xs
;;

let traverse_map : f:('a -> 'b t) -> ('key, 'a, 'cmp) Map.t -> ('key, 'b, 'cmp) Map.t t =
  fun ~f m ->
  Map.fold
    m
    ~init:(pure (Map.empty (Map.comparator_s m)))
    ~f:(fun ~key ~data acc ->
      let open Syntax in
      let* acc = acc in
      let* data' = f data in
      pure (Map.set acc ~key ~data:data'))
;;

let rec fold_left : f:('acc -> 'a -> 'acc t) -> init:'acc -> 'a list -> 'acc t =
  fun ~f ~init xs ->
  match xs with
  | [] -> pure init
  | x :: rest ->
    let open Syntax in
    let* acc = f init x in
    fold_left ~f ~init:acc rest
;;

let trace
  :  Trace.judgement_kind
  -> to_sexp:('a -> Sexp.t)
  -> ?focus:Sexp.t
  -> Lexing.position
  -> 'a t
  -> 'a t
  =
  fun kind ~to_sexp ?focus here m ->
  { run =
      (fun ctx ok err ->
        let context =
          Map.to_alist ctx.bindings
          |> List.map ~f:(function
            | ident, Untyped tm -> Ident.Intern.lookup ident, Term.sexp_of_value tm, None
            | ident, Typed (tm, typ) ->
              ( Ident.Intern.lookup ident
              , Term.sexp_of_value tm
              , Some (Term.sexp_of_value typ) ))
        in
        let source_location =
          Trace.{ file = here.Lexing.pos_fname; line = here.Lexing.pos_lnum }
        in
        let judgement =
          Trace.{ kind; focus; context; location = ctx.pos; source_location }
        in
        let action = Trace.enter judgement in
        match action with
        | Abort -> raise Trace.Trace_aborted
        | _ ->
          m.run
            ctx
            (fun a ctx' ->
               let outcome = Trace.Succeeded (to_sexp a) in
               let _ = Trace.leave judgement outcome in
               ok a ctx')
            (fun ctx' ->
               let outcome =
                 Trace.Failed
                   (match ctx'.errors with
                    | e :: _ -> e
                    | [] -> `Todo)
               in
               let _ = Trace.leave judgement outcome in
               err ctx'))
  }
;;
