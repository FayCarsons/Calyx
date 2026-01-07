open Term

let error : Error.t -> string = function
  | `Expected (expected, got) -> Printf.sprintf "Expected %s, got %s" expected got
  | `UnificationFailure (a, b) -> Printf.sprintf "Cannot unify '%s' with '%s'" a b
  | `InferLambda -> "Cannot infer lambda"
  | `NotFound n -> Printf.sprintf "No var '%s' in scope" n
  | `NoField (field, record) ->
    Printf.sprintf
      "No field '%s' in record shape { %s }"
      field
      (String.concat ", "
       @@ List.map (fun (label, field) -> Printf.sprintf "%s: %s" label field) record)
  | `Occurs m -> Printf.sprintf "Occurs ?'%s'" @@ string_of_int m
  | `Parser error -> Printf.sprintf "Parser:\n%s\n" error
  | `Todo -> "TODO"
;;

let record ?(tail = None) (prn : 'a -> string) (fields : (Ident.t * 'a) list) : string =
  let fields =
    List.map (fun (ident, v) -> Printf.sprintf "%s = %s" ident (prn v)) fields
    |> String.concat ","
  in
  match tail with
  | Some tail -> Printf.sprintf "{ %s | %s }" fields (prn tail)
  | None -> Printf.sprintf "{ %s }" fields
;;

let literal (prn : 'a -> string) : 'a literal -> string = function
  | Int n | UInt n -> string_of_int n
  | Float x -> string_of_float x
  | Bool b -> string_of_bool b
  | Record fields -> record prn fields
;;

let rec pattern (prn : 'a -> string) : 'a pattern -> string = function
  | PCtor (ident, args) -> String.concat " " @@ (ident :: List.map (pattern prn) args)
  | PLit lit -> literal prn lit
  | PRec fields -> record (pattern prn) fields
  | PVar v -> v
  | PWild -> "_"
;;

let base (type a) (prn : a -> string) : a base -> string = function
  | `Var ident -> ident
  | `App (f, x) -> Printf.sprintf "(%s %s)" (prn f) (prn x)
  | `Ann (x, t) -> Printf.sprintf "(%s : %s)" (prn x) (prn t)
  | `Type -> "Type"
  | `Lit lit -> literal prn lit
  | `Infix { left; op; right } ->
    Printf.sprintf "%s %s %s" (prn left) (prn op) (prn right)
  | `Proj (term, field) -> Printf.sprintf "%s.%s" (prn term) field
  | `Match (scrutinee, arms) ->
    let arm (pat, result) = Printf.sprintf "| %s -> %s" (pattern prn pat) (prn result) in
    let arms = List.map arm arms |> String.concat "\n" in
    Printf.sprintf "match %s with \n %s \n end" (prn scrutinee) arms
  | `Err e -> error e
;;

let rec value : value -> string = function
  | `Lam (x, body) ->
    print_endline "Pretty.value.Lam";
    let var = `Neutral (NVar (0, "_")) in
    let body = value @@ body var in
    Printf.sprintf "\\%s -> %s" x body
  | `Pi (x, dom, cod) ->
    print_endline "Pretty.value.Pi";
    let var = `Neutral (NVar (0, "_")) in
    let cod = cod var in
    Printf.sprintf "(%s : %s) -> %s" x (value dom) (value cod)
  | `Neutral n ->
    print_endline "Pretty.value.Neutral";
    neutral n
  | `Row { fields; tail } -> record value fields ~tail
  | `Rec row -> value row
  | `Ann (`Ann (x, t), _) -> base value (`Ann (x, t))
  | `Opaque -> "<opaque>"
  | #base as other -> base value (other :> value base)

and neutral : neutral -> string = function
  | NVar (n, ident) -> Printf.sprintf "NVar %s %s" (string_of_int n) ident
  | NApp (f, x) -> Printf.sprintf "(%s %s)" (neutral f) (value x)
  | NMeta m -> Printf.sprintf "?%s" @@ string_of_int m
  | NProj (tm, field) -> Printf.sprintf "%s.%s" (neutral tm) field
;;

let term_binders (prn : 'a -> string) : 'a term_binders -> string = function
  | `Lam (x, body) -> Printf.sprintf "\\%s -> %s" x (prn body)
  | `Pi (x, dom, cod) -> Printf.sprintf "(%s : %s) -> %s" x (prn dom) (prn cod)
  | `Let (ident, Some ty, expr, body) ->
    Printf.sprintf "let %s : %s = %s in\n%s" ident (prn ty) (prn expr) (prn body)
  | `Let (ident, None, expr, body) ->
    Printf.sprintf "let %s = %s in\n%s" ident (prn expr) (prn body)
;;

let rec cst : cst -> string = function
  | `Pos (_, tm) -> cst tm
  | `Infix { left; op; right } ->
    Printf.sprintf "%s %s %s" (cst left) (cst op) (cst right)
  | `If (x, then', else') ->
    Printf.sprintf "if %s then\n\t%selse\n\t%s" (cst x) (cst then') (cst else')
  | #base as b -> base cst b
  | #term_binders as binder -> term_binders cst binder
;;

let rec ast : ast -> string = function
  | `Pos (_, tm) -> ast tm
  | `Meta m -> Printf.sprintf "?%s" @@ string_of_int m
  | `Ann (`Ann (x, t), _) -> base ast (`Ann (x, t))
  | #term_binders as binder -> term_binders ast binder
  | #base as otherwise -> base ast otherwise
;;

let declaration (prn : 'a -> string) : 'a declaration -> string = function
  | Function { ident; typ; body } ->
    Printf.sprintf "def %s (%s) do\n%s\n\n" ident (prn typ) (prn body)
  | Constant { ident; typ; body } ->
    Printf.sprintf "const %s : %s = %s\n\n" ident (prn typ) (prn body)
  | RecordDecl { ident; params; fields } ->
    let params =
      List.fold_left String.cat ""
      @@ List.map (fun (ident, ty) -> Printf.sprintf "%s : %s" ident (prn ty)) params
    in
    let fields =
      String.concat "\n\t"
      @@ List.map (fun (field, ty) -> Printf.sprintf "%s : %s" field (prn ty)) fields
    in
    Printf.sprintf "data %s %s where\n\t%s\n\n" ident params fields
;;
