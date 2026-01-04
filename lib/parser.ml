(*
Goals:
  - Syntax should be versatile
    - forcing casing sucks
    - whitespace dependency should be opt-in
    - you don't know what conventions your community may arrive at
    - we have different taste!
  - Most languages could be more readable
    - Syntax could be more minimal like Haskell
    - But with structure (i.e. 'do .. end') like Ada or Lua
    - Natural language, unicode, operators are your friend
 *)
open Angstrom
open Angstrom.Let_syntax
open Term

let pure = return
let ( $ ) = Util.( $ )

module Prec = struct
  type t = int Ident.Map.t

  let from_list (xs : (Ident.t * int) list) =
    let module Map = Ident.Map in
    let m = Map.create 64 in
    List.iter (fun (ident, prec) -> Map.add m ident prec) xs;
    m
  ;;

  let default_precedence = 1
  let add : t -> Ident.t -> int -> unit = Ident.Map.add

  let lookup : t -> Ident.t -> int =
    fun m ident -> Option.value ~default:default_precedence $ Ident.Map.find_opt m ident
  ;;
end

let ws =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
;;

let is_ident_start c = is_lower c || is_upper c || Char.equal c '_'

let is_ident_char c =
  is_ident_start c
  || is_digit c
  || Char.equal c '\''
  || Char.equal c '_'
  || Char.equal c '-'
;;

let is_ident_legal c = is_upper c || is_lower c || is_digit c || List.mem c [ '_'; '-' ]
let lexeme p = ws *> p <* ws
let symbol s = string s <* ws *> pure ()

let keyword s =
  string s
  <* (peek_char
      >>= function
      | Some c when is_ident_legal c -> fail "keyword"
      | _ -> pure ())
  <* ws
;;

let keywords =
  [ "def"
  ; "let"
  ; "in"
  ; "do"
  ; "end"
  ; "if"
  ; "then"
  ; "else"
  ; "match"
  ; "with"
  ; "type"
  ; "data"
  ; "where"
  ; "const"
  ; "True"
  ; "False"
  ]
;;

let parens p = symbol "(" *> p <* symbol ")"
let braces p = char '{' *> p <* char '}'
let brackets p = char '[' *> p <* char ']'
let mkvar s : Term.cst = `Var (Ident.mk s)
let add = symbol "+"
let sub = symbol "-"
let mul = symbol "*"
let div = symbol "/"

(* Operator character classification *)
let is_op_char = function
  | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '&' | '|' | '^' | '%' | '~' | '?' ->
    true
  | _ -> false
;;

(* Parse infix operators *)
let infix_op = lexeme $ choice [ add; sub; mul; div; take_while1 is_op_char ]

(* Check if string starts with an operator character *)
let looks_like_operator s = (not (Int.equal (String.length s) 0)) && is_op_char s.[0]

(* Precedence climbing parser for infix expressions *)
let mk_parse_infix m =
  let rec go min_prec atom_parser =
    let* left = atom_parser in
    let rec loop left =
      peek_string 3
      >>= fun lookahead ->
      if looks_like_operator lookahead
      then
        let* op = option None (infix_op >>| Option.some) in
        match op with
        | None -> pure left
        | Some op ->
          let prec = Prec.lookup m op in
          if prec < min_prec
          then pure left
          else (
            (* TODO: Defaulting to assuming ops are left-associative 
               but in the future we should track this as well in `Prec.t` 
             *)
            let next_min_prec = succ prec in
            let* right = go next_min_prec atom_parser in
            let combined = `Infix { left; op = mkvar op; right } in
            loop combined)
      else return left
    in
    loop left
  in
  go
;;

let var_ident =
  lexeme
  $
  let* first = satisfy is_lower in
  let* rest = take_while is_ident_char in
  let name = String.make 1 first ^ rest in
  if List.mem name keywords then fail ("reserved keyword: " ^ name) else pure name
;;

let ctor_ident =
  lexeme
  $
  let* first = satisfy is_upper in
  let* rest = take_while is_ident_char in
  pure $ String.make 1 first ^ rest
;;

let any_ident = var_ident <|> ctor_ident

(* LITERALS *)

let float_lit : Term.cst t =
  let digits = take_while1 is_digit in
  let sign = option "" $ string "-" in
  let mantissa =
    let with_non_frac =
      let* i = digits in
      let* _ = char '.' in
      let* f = option "" digits in
      pure (i ^ "." ^ f)
    in
    let only_frac =
      let* _ = char '.' in
      let* f = digits in
      pure $ "0" ^ "." ^ f
    in
    choice [ with_non_frac; only_frac ]
  in
  let exponent =
    option
      ""
      (let* e = char 'e' <|> char 'E' in
       let* s = sign in
       let* d = digits in
       pure (String.make 1 e ^ s ^ d))
  in
  let* s = sign in
  let* m = mantissa in
  let* e = exponent in
  let x = Float.of_string (s ^ m ^ e) in
  pure $ `Lit (Float x)
;;

let int_lit : Term.cst t =
  let decimal = take_while1 is_digit in
  let hex = string "0x" *> take_while1 is_hex in
  let mkint n = `Lit (Int (int_of_string n)) in
  mkint <$> choice [ decimal; hex ]
;;

let bool_lit : Term.cst t =
  let* b = keyword "True" *> pure true <|> keyword "False" *> pure false in
  pure $ `Lit (Bool b)
;;

let record_lit (expr : Term.cst t) : Term.cst t =
  let entry =
    (* 
      A record field name could be uppercased, in the case of 
      implicit modules where you may have something like 
        `module type Add a where 
          Output : Type; 
          (+) : a -> a -> Output;`
      which gets desugared to
        `struct Add (a : Type) where 
          Output : Type; 
          (+) : a -> a -> Add.Output;`
      Or just because the user feels like it!
    *)
    let* ident = var_ident <|> ctor_ident in
    let* _ = char '=' in
    let* value = expr in
    pure (ident, value)
  in
  let mk fields = `Lit (Record fields) in
  mk <$> braces (sep_by1 (char ',') entry)
;;

(** JSON-like primitive parsing for attributes 
    So a declaration can have an annotation like: 
    ```haskell 
    @infix{prec = 4; assoc = "left"}
    def (<$>) (f : A -> B) [Functor T] (x : T A) -> T B do
      Functor.map f x
    ```

    Alternatively global settings or special declarations can be done like: 
    ```
    @extern putc : Char -> IO unit = <clayx_putc>
    @module{mustEraseViaPartialEvaluation = True}
    ```
 *)

module Value = struct
  type t =
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Array of t list
    | Object of (Ident.t * t) list
end

let expr : Term.cst t =
  let prec_map = Prec.from_list [] in
  let parse_infix = mk_parse_infix prec_map in
  fix (fun expr ->
    let atom =
      lexeme
      $ choice
          [ parens expr
          ; mkvar <$> var_ident
          ; mkvar <$> ctor_ident
          ; bool_lit
          ; float_lit
          ; int_lit
          ; record_lit expr
          ]
    in
    (* Field projection like 'foo.bar' or '{ foo = 4 }.foo' *)
    let proj =
      let* base = atom in
      let* fields = many (symbol "." *> any_ident) in
      pure $ List.fold_left (fun e f -> `Proj (e, f)) base fields
    in
    (* Application like 'succ 0' *)
    let app =
      let* f = proj in
      let* args = many proj in
      pure $ List.fold_left (fun f x -> `App (f, x)) f args
    in
    (* Infix operators with precedence *)
    let infix_expr = parse_infix 0 app in
    let arrow_or_pi =
      fix (fun arrow_or_pi ->
        (* Try to parse dependent pi '(x : A) -> B' *)
        let maybe_pi =
          let* _ = symbol "(" in
          let* inner = expr in
          let* _ = symbol ")" in
          let* rest = option None (symbol "->" *> arrow_or_pi >>| Option.some) in
          match rest, inner with
          | Some cod, `Ann (`Var x, a) -> pure @@ `Pi (x, a, cod)
          | Some cod, dom -> pure @@ `Pi ("_", dom, cod)
          | None, _ -> pure inner
        in
        (* If that fails, try a simple arrow 'A -> B' *)
        let simple_arrow =
          let* dom = infix_expr in
          let* rest = option None (symbol "->" *> arrow_or_pi >>| Option.some) in
          match rest with
          | None -> pure dom
          | Some cod -> pure @@ `Pi ("_", dom, cod)
        in
        maybe_pi <|> simple_arrow)
    in
    (* Annotation like (0 :: UInt) *)
    let ann base : Term.cst t =
      let* _ = symbol ":" in
      let* ty = expr in
      pure $ `Ann (base, ty)
    in
    (* Lambda like '\x -> x + 42' *)
    let lam =
      let* _ = symbol "\\" in
      let* x = var_ident in
      let* _ = symbol "->" in
      let* body = expr in
      pure $ `Lam (x, body)
    in
    (* Let expression like 'let x : Nat = Zero in x' *)
    let let_expr =
      let* _ = symbol "let" in
      let* x = var_ident in
      let* ty = Option.some <$> char ':' *> expr <|> pure None in
      let* _ = symbol "=" in
      let* bound = expr in
      let* _ = symbol "in" in
      let* body = expr in
      pure $ `Let (x, ty, bound, body)
    in
    (* If expression like `if True then Succ Zero else Zero` *)
    let if_ =
      let* _ = keyword "if" in
      let* cond = expr in
      let* _ = keyword "then" in
      let* then_ = expr in
      let* _ = keyword "else" in
      let* else_ = expr in
      pure $ `If (cond, then_, else_)
    in
    let base = choice [ lam; let_expr; if_; arrow_or_pi ] in
    let* e = base in
    option e (ann e))
;;

let toplevel =
  let arg =
    parens
    $
    let* idents = many1 any_ident in
    let* _ = symbol ":" in
    let* ty = expr in
    pure $ List.map (fun i -> i, ty) idents
  in
  let fn_decl =
    let* _ = keyword "def" in
    let* ident = any_ident in
    let* args = many arg in
    let* _ = symbol "->" in
    let* return_type = expr in
    let* _ = keyword "do" in
    let* body = expr in
    let* _ = keyword "end" in
    let typ =
      List.fold_right
        (fun (ident, ty) acc -> `Pi (ident, ty, acc))
        (List.flatten args)
        return_type
    in
    let body =
      List.fold_right (fun (x, _) acc -> `Lam (x, acc)) (List.flatten args) body
    in
    pure $ Term.Function { ident; typ; body }
  in
  let constant_decl =
    let* _ = keyword "const" in
    let* ident = any_ident in
    let* typ = expr in
    let* _ = symbol "=" in
    let* body = expr in
    pure $ Term.Function { ident; typ; body }
  in
  fn_decl <|> constant_decl
;;

let parse inp = parse_string ~consume:All (option () ws *> many toplevel) inp
