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
  [ `Lam of Ident.t * 'a
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
[@@deriving show, sexp, map]

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

(* HOAS encoding *)
type value =
  [ value base
  | `Lam of Ident.t * (value -> value)
  | `Pi of plicity * Ident.t * value * (value -> value)
  | `RecordType of value row
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
      }
  | Constant of
      { ident : Ident.t
      ; typ : 'a
      ; body : 'a
      }
  | RecordDecl of
      { ident : Ident.t
      ; params : 'a Ident.Map.t
      ; fields : 'a Ident.Map.t
      }
  | SumDecl of
      { ident : Ident.t
      ; params : 'a Ident.Map.t
      ; constructors : 'a list Ident.Map.t
      }
[@@deriving show, sexp]

let desugar_toplevel = function
  | Function { ident; typ; body } ->
    let typ = desugar typ
    and body = desugar body in
    Function { ident; typ; body }
  | Constant { ident; typ; body } ->
    let typ = desugar typ
    and body = desugar body in
    Constant { ident; typ; body }
  | RecordDecl { ident; params; fields } ->
    let params = Map.map params ~f:desugar
    and fields = Map.map fields ~f:desugar in
    RecordDecl { ident; params; fields }
  | SumDecl { ident; params; constructors } ->
    let params = Map.map params ~f:desugar
    and constructors = Map.map constructors ~f:(List.map ~f:desugar) in
    SumDecl { ident; params; constructors }
;;
