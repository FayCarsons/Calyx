open Core

type 'a base =
  [ `Var of Ident.t
  | `App of 'a * 'a
  | `Infix of 'a infix
  | `Ann of 'a * 'a
  | `Type
  | `Lit of 'a literal
  | `Proj of 'a * Ident.t
  | `Match of 'a * ('a pattern * 'a) list
  | `Err of CalyxError.t
  ]
[@@deriving show, sexp]

and 'a literal =
  | Int of int
  | UInt of int
  | Float of float
  | Bool of bool
  | Record of 'a Ident.Map.t
[@@deriving show, sexp]

and 'a row =
  { fields : 'a Ident.Map.t
  ; tail : 'a option [@sexp.option]
  }
[@@deriving show, sexp]

and 'a pattern =
  | PVar of Ident.t
  | PWild
  | PCtor of Ident.t * 'a pattern list
  | PLit of 'a literal
  | PRec of (Ident.t * 'a pattern) list
[@@deriving show, sexp]

and 'a infix =
  { left : 'a
  ; op : 'a
  ; right : 'a
  }
[@@deriving show, sexp]

let over_literal (tf : 'a -> 'b) : 'a literal -> 'b literal = function
  | Record fields -> Record (Map.map ~f:tf fields)
  | Int n -> Int n
  | UInt n -> UInt n
  | Float x -> Float x
  | Bool b -> Bool b
;;

let rec over_pattern (tf : 'a -> 'b) : 'a pattern -> 'b pattern = function
  | PCtor (ident, ps) -> PCtor (ident, List.map ~f:(over_pattern tf) ps)
  | PLit lit -> PLit (over_literal tf lit)
  | PRec fields -> PRec (List.map ~f:(fun (l, f) -> l, over_pattern tf f) fields)
  | PVar x -> PVar x
  | PWild -> PWild
;;

let map_infix f { left; op; right } =
  let left = f left
  and op = f op
  and right = f right in
  { left; op; right }
;;

let map_base (tf : 'a -> 'b) : 'a base -> 'b base = function
  | `App (f, x) -> `App (tf f, tf x)
  | `Infix inf -> `Infix (map_infix tf inf)
  | `Ann (x, t) -> `Ann (tf x, tf t)
  | `Lit lit -> `Lit (over_literal tf lit)
  | `Proj (tm, f) -> `Proj (tf tm, f)
  | `Match (x, arms) ->
    `Match (tf x, List.map ~f:(fun (cond, res) -> over_pattern tf cond, tf res) arms)
  | `Var x -> `Var x
  | `Type -> `Type
  | `Err e -> `Err e
;;

let is_annotation : type a. a base -> bool = function
  | `Ann _ -> true
  | _ -> false
;;

(* ['a term_binders] allows us to abstract over binders, i.e. HOAS for [value] *)
type 'a term_binders =
  [ `Lam of plicity * Ident.t * 'a
  | `Pi of 'a pi
  | `Let of Ident.t * 'a option * 'a * 'a
  ]
[@@deriving show, sexp, map]

and 'a pi =
  { plicity : plicity
  ; ident : Ident.t
  ; dom : 'a
  ; cod : 'a
  }
[@@deriving show, sexp, map]

and plicity =
  | Implicit
  | Instance
  | Explicit
[@@deriving show, sexp, eq]

type cst =
  [ cst base
  | cst term_binders
  | `Pos of Pos.t * cst
  | `If of cst * cst * cst
  | `RecordType of row_syntax
  ]
[@@deriving show, sexp]

and row_syntax =
  { fields : cst Ident.Map.t
  ; tail : record_tail
  }
[@@deriving show, sexp]

and record_tail =
  (* Implicit tail `{ x : Float, y : Float }`*)
  | ImplicitTail
  (* Explicit tail `{ x : Float, y : Float | rest }` *)
  | ExplicitTail of Ident.t
  (* Explicitly closed `{ x : Float, y : Float !}` *)
  | TailClosed
[@@deriving show, sexp]

type 'a sum_type =
  { ident : Ident.t
  ; params : 'a Ident.Map.t
  ; constructors : 'a list Ident.Map.t
  ; position : Pos.pos * Pos.pos
  }
[@@deriving show, sexp]

let tail_opt : record_tail -> Ident.t option = function
  | ExplicitTail ident -> Some ident
  | _ -> None
;;

type ast =
  [ ast base
  | ast term_binders
  | `Pos of Pos.t * ast
  | `Meta of Meta.t
  | `RecordType of ast row
  | `SumType of ast sum_type
  ]
[@@deriving show, sexp]

let rec desugar : cst -> ast = function
  | `Pos (pos, t) -> `Pos (pos, desugar t)
  | `Infix inf -> `Infix (map_infix desugar inf)
  | `RecordType { fields; tail = TailClosed } ->
    let fields = Map.map ~f:desugar fields in
    `RecordType { fields; tail = None }
  | `RecordType { fields; tail } ->
    let fields = Map.map ~f:desugar fields in
    let tail_ident = tail_opt tail |> Option.value ~default:(Ident.Intern.intern "a") in
    `RecordType { fields; tail = Some (`Var tail_ident) }
  | `If (cond, t, f) ->
    let cond = desugar cond
    and t = desugar t
    and f = desugar f in
    `Match
      ( cond
      , [ PVar (Ident.Intern.intern "True"), t; PVar (Ident.Intern.intern "False"), f ] )
  | #base as b -> (map_base desugar b :> ast)
  | #term_binders as binder -> (map_term_binders desugar binder :> ast)
;;

(* Free variable computation - polymorphic helpers over term types *)

module FreeVars = struct
  module S = Ident.TreeSet

  let of_literal (go : 'a -> S.t) : 'a literal -> S.t = function
    | Record fields -> Map.data fields |> List.map ~f:go |> S.union_list
    | Int _ | UInt _ | Float _ | Bool _ -> S.empty
  ;;

  let rec of_pattern : 'a pattern -> S.t = function
    | PVar x -> S.singleton x
    | PWild -> S.empty
    | PCtor (_, pats) -> List.map pats ~f:of_pattern |> S.union_list
    | PLit _ -> S.empty
    | PRec fields -> List.map fields ~f:(Fun.compose of_pattern snd) |> S.union_list
  ;;

  let of_base (go : 'a -> S.t) : 'a base -> S.t = function
    | `Var ident -> S.singleton ident
    | `App (f, x) -> Set.union (go f) (go x)
    | `Infix { left; op; right } -> S.union_list [ go left; go op; go right ]
    | `Ann (x, t) -> Set.union (go x) (go t)
    | `Lit lit -> of_literal go lit
    | `Proj (tm, _) -> go tm
    | `Match (scrut, arms) ->
      let arm_free (pat, body) = Set.diff (go body) (of_pattern pat) in
      Set.union (go scrut) (List.map arms ~f:arm_free |> S.union_list)
    | `Type | `Err _ -> S.empty
  ;;

  let of_binders (go : 'a -> S.t) : 'a term_binders -> S.t = function
    | `Lam (_, x, body) -> Set.remove (go body) x
    | `Pi { ident; dom; cod; _ } -> Set.union (go dom) (Set.remove (go cod) ident)
    | `Let (x, typ, value, body) ->
      S.union_list
        [ Option.value_map typ ~default:S.empty ~f:go; go value; Set.remove (go body) x ]
  ;;

  let rec of_cst : cst -> S.t = function
    | `Pos (_, t) -> of_cst t
    | `If (cond, t, f) -> S.union_list [ of_cst cond; of_cst t; of_cst f ]
    | `RecordType { fields; tail } ->
      let field_vars = Map.data fields |> List.map ~f:of_cst |> S.union_list in
      let tail_var =
        match tail with
        | ExplicitTail ident -> S.singleton ident
        | ImplicitTail | TailClosed -> S.empty
      in
      Set.union field_vars tail_var
    | #base as b -> of_base of_cst b
    | #term_binders as binder -> of_binders of_cst binder
  ;;

  let rec of_ast : ast -> S.t = function
    | `Pos (_, t) -> of_ast t
    | `Meta _ -> S.empty
    | `RecordType { fields; tail } ->
      let field_vars = Map.data fields |> List.map ~f:of_ast |> S.union_list in
      let tail_var = Option.value_map tail ~default:S.empty ~f:of_ast in
      Set.union field_vars tail_var
    | `SumType { params; constructors; _ } ->
      let param_vars = Map.data params |> List.map ~f:of_ast |> S.union_list in
      let ctor_vars =
        Map.data constructors |> List.bind ~f:(List.map ~f:of_ast) |> S.union_list
      in
      Set.union param_vars ctor_vars
    | #base as b -> of_base of_ast b
    | #term_binders as binder -> of_binders of_ast binder
  ;;
end

(* Convenience wrapper returning list *)
let free : cst -> Ident.t list = fun term -> Set.to_list (FreeVars.of_cst term)

(* HOAS encoding *)
type value =
  [ value base
  | `Lam of plicity * Ident.t * (value -> value)
  | `Pi of plicity * Ident.t * value * (value -> value)
  | `RecordType of value row
  | `SumType of value sum_type
  | `Neutral of neutral
  | `Opaque
  ]
[@@deriving show, sexp]

and neutral =
  | NVar of int * Ident.t
  | NApp of neutral * value
  | NProj of neutral * Ident.t
  | NMeta of Meta.t
[@@deriving show, sexp]

type 'a declaration =
  | Function of
      { ident : Ident.t
      ; typ : 'a
      ; body : 'a
      ; position : Pos.pos * Pos.pos
      }
  | Constant of
      { ident : Ident.t
      ; typ : 'a
      ; body : 'a
      ; position : Pos.pos * Pos.pos
      }
  | RecordDecl of
      { ident : Ident.t
      ; params : 'a Ident.Map.t
      ; fields : 'a Ident.Map.t
      ; position : Pos.pos * Pos.pos
      }
  | SumDecl of 'a sum_type
[@@deriving show, sexp]

let desugar_toplevel = function
  | Function { ident; typ; body; position } ->
    let typ = desugar typ
    and body = desugar body in
    Function { ident; typ; body; position }
  | Constant { ident; typ; body; position } ->
    let typ = desugar typ
    and body = desugar body in
    Constant { ident; typ; body; position }
  | RecordDecl { ident; params; fields; position } ->
    let params = Map.map params ~f:desugar
    and fields = Map.map fields ~f:desugar in
    RecordDecl { ident; params; fields; position }
  | SumDecl { ident; params; constructors; position } ->
    let params = Map.map params ~f:desugar
    and constructors = Map.map constructors ~f:(List.map ~f:desugar) in
    SumDecl { ident; params; constructors; position }
;;
