open Core
module PP = PPrint

module Options = struct
  type t =
    { width : int
    ; indent : int
    ; show_implicits : bool
    }

  let default = { width = 80; indent = 2; show_implicits = false }
end

let ( ^^ ) = PP.( ^^ )
let string = PP.string
let char = PP.char
let space = PP.space
let empty = PP.empty
let hardline = PP.hardline
let nest = PP.nest
let group = PP.group
let separate = PP.separate
let concat = PP.concat
let ifflat = PP.ifflat
let arrow = string " -> "
let pipe = string "| "
let lambda = char '\\'
let dot = char '.'
let colon = string " : "
let equals = string " = "
let comma_sep = string ", "

type document = PP.document

let parens d = char '(' ^^ d ^^ char ')'
let braces d = char '{' ^^ d ^^ char '}'
let brackets d = char '[' ^^ d ^^ char ']'

let haskell_braces (fields : document list) : document =
  match fields with
  | [] -> string "{}"
  | [ single ] -> char '{' ^^ space ^^ single ^^ space ^^ char '}'
  | first :: rest ->
    let flat = char '{' ^^ space ^^ separate comma_sep fields ^^ space ^^ char '}' in
    let multi =
      char '{'
      ^^ space
      ^^ first
      ^^ concat (List.map ~f:(fun f -> hardline ^^ string ", " ^^ f) rest)
      ^^ hardline
      ^^ char '}'
    in
    group (ifflat flat multi)
;;

let annotated x t = x ^^ colon ^^ t
let ident (id : Ident.t) : document = string (Ident.Intern.lookup id)
let is_underscore id = Ident.equal id Ident.Intern.underscore

let literal (pp_inner : 'a -> document) : 'a Term.literal -> document = function
  | Term.Int n -> string (Int.to_string n)
  | Term.UInt n -> string (Int.to_string n) ^^ char 'u'
  | Term.Float f -> string (Float.to_string f)
  | Term.Bool b -> string (if b then "True" else "False")
  | Term.Record fields ->
    let field (name, value) = ident name ^^ equals ^^ pp_inner value in
    haskell_braces (List.map ~f:field (Map.to_alist fields))
;;

let literal_simple : _ Term.literal -> document = function
  | Term.Int n -> string (Int.to_string n)
  | Term.UInt n -> string (Int.to_string n) ^^ char 'u'
  | Term.Float f -> string (Float.to_string f)
  | Term.Bool b -> string (if b then "True" else "False")
  | Term.Record _ -> string "{...}"
;;

let rec pattern : _ Term.pattern -> document = function
  | Term.PVar id -> ident id
  | Term.PWild -> char '_'
  | Term.PCtor (name, []) -> ident name
  | Term.PCtor (name, args) ->
    ident name ^^ space ^^ separate space (List.map ~f:pattern_atom args)
  | Term.PLit lit -> literal_simple lit
  | Term.PRec fields ->
    let field (name, pat) = ident name ^^ equals ^^ pattern pat in
    braces (separate comma_sep (List.map ~f:field fields))

and pattern_atom : _ Term.pattern -> document = function
  | Term.PCtor (_, _ :: _) as p -> parens (pattern p)
  | p -> pattern p
;;

let plicity_wrap (plic : Term.plicity) (doc : document) : document =
  match plic with
  | Term.Implicit -> braces doc
  | Term.Instance -> brackets doc
  | Term.Explicit -> parens doc
;;

let rec cst ?(opts = Options.default) : Term.cst -> document = function
  | `Var id -> ident id
  | `App (f, x) -> cst ~opts f ^^ space ^^ cst_atom ~opts x
  | `Infix { left; op; right } ->
    cst_app ~opts left ^^ space ^^ cst ~opts op ^^ space ^^ cst ~opts right
  | `Ann (e, t) -> parens (cst ~opts e ^^ colon ^^ cst ~opts t)
  | `Type -> string "Type"
  | `Lit lit -> literal (cst ~opts) lit
  | `Proj (e, field) -> cst_atom ~opts e ^^ dot ^^ ident field
  | `Match (scrut, arms) ->
    let arm (pat, body) =
      let body_doc = cst ~opts body in
      let flat = pipe ^^ pattern pat ^^ arrow ^^ body_doc in
      let multi =
        pipe ^^ pattern pat ^^ string " ->" ^^ nest opts.indent (hardline ^^ body_doc)
      in
      group (ifflat flat multi)
    in
    string "match "
    ^^ cst ~opts scrut
    ^^ string " with"
    ^^ hardline
    ^^ separate hardline (List.map ~f:arm arms)
  | `Err _ -> string "<error>"
  | `Lam (plic, x, body) ->
    let param =
      if Term.equal_plicity plic Term.Implicit && opts.show_implicits
      then braces (ident x)
      else if Term.equal_plicity plic Term.Implicit
      then empty
      else ident x
    in
    if Term.equal_plicity plic Term.Implicit && not opts.show_implicits
    then cst ~opts body
    else lambda ^^ param ^^ arrow ^^ cst ~opts body
  | `Pi { plicity = plic; ident = id; dom; cod } ->
    let named = not (is_underscore id) in
    if named
    then plicity_wrap plic (ident id ^^ colon ^^ cst ~opts dom) ^^ arrow ^^ cst ~opts cod
    else cst_app ~opts dom ^^ arrow ^^ cst ~opts cod
  | `Let (x, ty, value, body) ->
    let annotation =
      match ty with
      | Some t -> colon ^^ cst ~opts t
      | None -> empty
    in
    let value_doc = cst ~opts value in
    let binding_flat =
      string "let " ^^ ident x ^^ annotation ^^ equals ^^ value_doc ^^ string " in"
    in
    let binding_multi =
      string "let "
      ^^ ident x
      ^^ annotation
      ^^ string " ="
      ^^ nest opts.indent (hardline ^^ value_doc)
      ^^ hardline
      ^^ string "in"
    in
    group (ifflat binding_flat binding_multi) ^^ hardline ^^ cst ~opts body
  | `Pos (_, inner) -> cst ~opts inner
  | `If (cond, t, f) ->
    group
      (string "if "
       ^^ cst ~opts cond
       ^^ string " then"
       ^^ nest opts.indent (hardline ^^ cst ~opts t)
       ^^ hardline
       ^^ string "else"
       ^^ nest opts.indent (hardline ^^ cst ~opts f))
  | `RecordType { fields; tail } ->
    let field (name, ty) = ident name ^^ colon ^^ cst ~opts ty in
    let field_docs = List.map ~f:field (Map.to_alist fields) in
    let tail_doc =
      match tail with
      | Term.ImplicitTail -> None
      | Term.ExplicitTail id -> Some (string "| " ^^ ident id)
      | Term.TailClosed -> Some (char '!')
    in
    let all_docs =
      match tail_doc with
      | None -> field_docs
      | Some t -> field_docs @ [ t ]
    in
    haskell_braces all_docs

and cst_atom ?(opts = Options.default) (term : Term.cst) : document =
  match term with
  | `Var _ | `Lit _ | `Type -> cst ~opts term
  | `Proj _ -> cst ~opts term
  | _ -> parens (cst ~opts term)

and cst_app ?(opts = Options.default) (term : Term.cst) : document =
  match term with
  | `App _ | `Var _ | `Lit _ | `Type | `Proj _ -> cst ~opts term
  | _ -> parens (cst ~opts term)
;;

let is_unit_type = function
  | `Var v -> String.equal (Ident.Intern.lookup v) "Unit"
  | _ -> false
;;

let rec collect_params : Term.cst -> (Term.plicity * Ident.t * Term.cst) list * Term.cst =
  function
  | `Pi { plicity; ident = id; dom; cod }
    when (not (is_underscore id)) || is_unit_type dom ->
    let params, ret = collect_params cod in
    (plicity, id, dom) :: params, ret
  | other -> [], other
;;

let rec collect_lambdas : Term.cst -> (Term.plicity * Ident.t) list * Term.cst = function
  | `Lam (plic, x, body) ->
    let params, inner = collect_lambdas body in
    (plic, x) :: params, inner
  | other -> [], other
;;

let is_unit_param id ty = is_underscore id && is_unit_type ty

let declaration ?(opts = Options.default) : Term.cst Term.declaration -> document
  = function
  | Term.Function { ident = name; typ; body; _ } ->
    let params, ret = collect_params typ in
    let _, body_inner = collect_lambdas body in
    let render_param (plic, id, ty) =
      if is_unit_param id ty
      then string "()"
      else plicity_wrap plic (ident id ^^ colon ^^ cst ~opts ty)
    in
    let param_doc =
      if List.is_empty params
      then string "()"
      else separate space (List.map ~f:render_param params)
    in
    group
      (string "def "
       ^^ ident name
       ^^ space
       ^^ param_doc
       ^^ arrow
       ^^ cst ~opts ret
       ^^ string " do"
       ^^ nest opts.indent (hardline ^^ cst ~opts body_inner))
  | Term.Constant { ident = name; typ; body; _ } ->
    group
      (string "const " ^^ ident name ^^ colon ^^ cst ~opts typ ^^ equals ^^ cst ~opts body)
  | Term.RecordDecl { ident = name; params; fields; _ } ->
    let params_doc =
      if Map.is_empty params
      then empty
      else
        space
        ^^ separate
             space
             (List.map
                ~f:(fun (id, ty) -> parens (ident id ^^ colon ^^ cst ~opts ty))
                (Map.to_alist params))
    in
    let field_doc (fname, fty) = ident fname ^^ colon ^^ cst ~opts fty in
    let fields_doc =
      separate (comma_sep ^^ hardline) (List.map ~f:field_doc (Map.to_alist fields))
    in
    group
      (string "data "
       ^^ ident name
       ^^ params_doc
       ^^ string " where"
       ^^ nest opts.indent (hardline ^^ fields_doc))
  | Term.SumDecl { ident = name; params; constructors; _ } ->
    let params_doc =
      if Map.is_empty params
      then empty
      else
        space
        ^^ separate
             space
             (List.map
                ~f:(fun (id, ty) -> parens (ident id ^^ colon ^^ cst ~opts ty))
                (Map.to_alist params))
    in
    let ctor_doc (cname, args) =
      pipe
      ^^ ident cname
      ^^ concat (List.map ~f:(fun ty -> space ^^ cst_atom ~opts ty) args)
    in
    let ctors_doc =
      separate hardline (List.map ~f:ctor_doc (Map.to_alist constructors))
    in
    group
      (string "data "
       ^^ ident name
       ^^ params_doc
       ^^ string " where"
       ^^ hardline
       ^^ ctors_doc)
;;

let program ?(opts = Options.default) (decls : Term.cst Term.declaration list) : document =
  separate (hardline ^^ hardline) (List.map ~f:(declaration ~opts) decls)
;;

let to_string ?(opts = Options.default) (doc : document) : string =
  let buf = Buffer.create 256 in
  PP.ToBuffer.pretty 1.0 opts.width buf doc;
  Buffer.contents buf
;;

let render_cst ?(opts = Options.default) (term : Term.cst) : string =
  to_string ~opts (cst ~opts term)
;;

let render_declaration ?(opts = Options.default) (decl : Term.cst Term.declaration)
  : string
  =
  to_string ~opts (declaration ~opts decl)
;;

let render_program ?(opts = Options.default) (decls : Term.cst Term.declaration list)
  : string
  =
  to_string ~opts (program ~opts decls)
;;
