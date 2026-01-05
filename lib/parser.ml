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
let lexeme p = option () ws *> p <* option () ws
let symbol s = string s <* option () ws

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
  | '+' | '-' | '*' | '/' | '<' | '>' | '!' | '&' | '|' | '^' | '%' | '~' | '?' -> true
  | _ -> false
;;

(* Parse infix operators *)
let infix_op = lexeme $ choice [ add; sub; mul; div; take_while1 is_op_char ]

(* Check if string starts with an operator character *)
let looks_like_operator s = (not (Int.equal (String.length s) 0)) && is_op_char s.[0]

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
  let name = String.make 1 first ^ rest in
  if List.mem name keywords then fail ("reserved keyword: " ^ name) else pure name
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
  let* b = symbol "True" *> pure true <|> symbol "False" *> pure false in
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
  fix (fun expr ->
    (* === ATOMS: Only recurse through parens === *)
    let atom =
      lexeme
      @@ choice
           [ (* Parenthesized expression - this is where expr recurses safely *)
             char '(' *> ws *> expr <* ws <* char ')'
           ; mkvar <$> var_ident
           ; mkvar <$> ctor_ident
           ; bool_lit
           ; float_lit (* must come before int_lit *)
           ; int_lit
           ; record_lit expr
           ]
    in
    (* === APPLICATION: One or more atoms, left-associative === *)
    let app =
      let* head = atom in
      let* tail = many atom in
      pure @@ List.fold_left (fun f x -> `App (f, x)) head tail
    in
    (* === INFIX: Pratt-style precedence climbing over applications === *)
    let infix_expr =
      let rec go min_prec left =
        let* c = peek_char in
        match c with
        | Some c when is_op_char c ->
          (* Wrap in a fallible parser so we backtrack if op is reserved or wrong precedence *)
          let try_op =
            let* op = infix_op in
            let prec = Prec.lookup prec_map (Ident.mk op) in
            if prec < min_prec
            then fail "precedence too low"
            else
              let* right = app >>= go (prec + 1) in
              go min_prec (`Infix { left; op = mkvar op; right })
          in
          try_op <|> pure left
        | _ -> pure left
      in
      app >>= go 0
    in
    (* Pi: (x : A) -> B *)
    let pi =
      let* ident, dom =
        (char '('
         *> ws
         *>
         let* id = var_ident in
         let* _ = ws *> char ':' *> ws in
         let* ty = expr in
         pure (id, ty))
        <* ws
        <* char ')'
        <* ws
      in
      let* _ = symbol "->" in
      let* cod = expr in
      pure @@ `Pi (ident, dom, cod)
    in
    (* Arrow: A -> B *)
    let arrow_or_operand =
      let* left = infix_expr in
      let* arrow_opt = option None (symbol "->" *> expr >>| Option.some) in
      match arrow_opt with
      | Some right -> pure @@ `Pi ("_", left, right)
      | None -> pure left
    in
    (* Lam: \x -> x *)
    let lam =
      let* _ = char '\\' in
      let* x = ws *> var_ident in
      let* _ = ws *> string "->" *> ws in
      let* body = expr in
      pure @@ `Lam (x, body)
    in
    (* Let: let x : t = v in b *)
    let let_expr =
      let* _ = symbol "let" in
      let* x = var_ident in
      let* ty = option None (symbol ":" *> expr >>| Option.some) in
      let* _ = symbol "=" in
      let* bound = expr in
      let* _ = symbol "in" in
      let* body = expr in
      pure @@ `Let (x, ty, bound, body)
    in
    (* If: if x then t else f *)
    let if_ =
      let* _ = symbol "if" in
      let* cond = expr in
      let* _ = symbol "then" in
      let* then_ = expr in
      let* _ = symbol "else" in
      let* else_ = expr in
      pure @@ `If (cond, then_, else_)
    in
    choice [ lam; let_expr; if_; pi; arrow_or_operand ])
;;

let toplevel =
  let arg =
    parens
      (let* idents = many1 any_ident in
       let* _ = symbol ":" in
       let* ty = expr in
       pure $ List.map (fun i -> i, ty) idents)
  in
  let fn_decl =
    let* _ = symbol "def" in
    let* ident = any_ident in
    let* args = many arg in
    let* _ = symbol "->" in
    let* return_type = expr in
    let* _ = symbol "do" in
    let* body = expr in
    let* _ = symbol "end" in
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
    let* _ = symbol "const" in
    let* ident = any_ident in
    let* _ = symbol ":" in
    let* typ = expr in
    let* _ = symbol "=" in
    let* body = expr in
    pure $ Term.Function { ident; typ; body }
  in
  fn_decl <|> constant_decl
;;

let parse inp = parse_string ~consume:All (option () ws *> many toplevel) inp
