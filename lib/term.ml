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
  [@printer
    fun fmt m ->
      Format.fprintf fmt "{";
      Map.iteri m ~f:(fun ~key ~data:_ -> Format.fprintf fmt "%a " Ident.pp key);
      Format.fprintf fmt "}"]

and 'a pattern =
  | PVar of Ident.t
  | PWild
  | PCtor of Ident.t * 'a pattern list
  | PLit of 'a literal
  | PRec of (Ident.t * 'a pattern) list

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

let over_base (tf : 'a -> 'b) : 'a base -> 'b base = function
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

(* Terms have syntactic binders *)
type 'a term_binders =
  [ `Lam of Ident.t * 'a
  | `Pi of Ident.t * 'a * 'a
  | `Let of Ident.t * 'a option * 'a * 'a
  ]
[@@deriving show, sexp]

let over_term_binders (tf : 'a -> 'b) : 'a term_binders -> 'b term_binders = function
  | `Lam (x, body) -> `Lam (x, tf body)
  | `Pi (x, dom, cod) -> `Pi (x, tf dom, tf cod)
  | `Let (x, ty, v, body) -> `Let (x, Option.map ~f:tf ty, tf v, tf body)
;;

type cst =
  [ cst base
  | cst term_binders
  | `Pos of Pos.t * cst
  | `If of cst * cst * cst
  ]
[@@deriving show, sexp]

type ast =
  [ ast base
  | ast term_binders
  | `Pos of Pos.t * ast
  | `Meta of Meta.t
  ]
[@@deriving show, sexp]

let rec desugar : cst -> ast = function
  | `Pos (pos, t) -> `Pos (pos, desugar t)
  | `Infix inf -> `Infix (map_infix desugar inf)
  | `If (cond, t, f) ->
    let cond = desugar cond
    and t = desugar t
    and f = desugar f in
    `Match
      ( cond
      , [ PVar (Ident.Intern.intern "True"), t; PVar (Ident.Intern.intern "False"), f ] )
  | #base as b -> (over_base desugar b :> ast)
  | #term_binders as binder -> (over_term_binders desugar binder :> ast)
;;

(* HOAS encoding *)
type value =
  [ value base
  | `Lam of Ident.t * (value -> value)
  | `Pi of Ident.t * value * (value -> value)
  | `Neutral of neutral
  | (* TODO: These suck, this was not the way to implement structural record types 
      We should just have literals, which can exist in the type position 
    *)
    `Row of row
  | `Rec of value
  | `Opaque
  ]
[@@deriving show, sexp]

and row =
  { fields : value Ident.Map.t
        [@printer
          fun fmt m ->
            Format.fprintf fmt "{";
            Map.iteri m ~f:(fun ~key ~data ->
              Format.fprintf fmt "%a: %a; " Ident.pp key pp_value data);
            Format.fprintf fmt "}"]
  ; tail : value option
  }
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
      ; params : (Ident.t * 'a) list
      ; fields : (Ident.t * 'a) list
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
    let open Util in
    let params = List.map ~f:(Tuple.second desugar) params
    and fields = List.map ~f:(Tuple.second desugar) fields in
    RecordDecl { ident; params; fields }
;;
