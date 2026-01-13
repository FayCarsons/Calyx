{
open Parser
module Intern = Ident.Intern

exception Lexing_error of string

let make_table l = 
  let t = Hashtbl.create (List.length l) in
  List.iter (fun (k, v) -> Hashtbl.add t k v) l;
  t

let keywords = make_table [
  ("def", DEF);
  ("do", DO);
  ("end", END);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  ("let", LET);
  ("in", IN);
  ("match", MATCH);
  ("with", WITH);
  ("Type", TYPE);
  ("data", DATA);
  ("where", WHERE);
  ("const", CONST);
  ("True", BOOL true);
  ("False", BOOL false);
]

let get_keyword_or_ident s = 
  match Hashtbl.find_opt keywords s with 
  | Some tok -> tok 
  | None -> IDENT (Intern.intern s)

let is_op_char = function
  | '+' | '-' | '*' | '/' | '<' | '>' | '!' | '&' | '|' 
  | '^' | '%' | '~' | '?' | '=' | ':' | '@' | '$' | '.' -> true
  | _ -> false

}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident_start = lower | upper | '_'
let ident_char = ident_start | digit | '\'' | '-'
let var_ident = lower ident_char*
let ctor_ident = upper ident_char*
let int = '-'? digit+
let hex_int = "0x" hex_digit+
let float = '-'? digit+ '.' digit* (['e' 'E'] ['+' '-']? digit+)?

rule token = parse
  | whitespace { token lexbuf }
  | newline    { Lexing.new_line lexbuf; token lexbuf }
  | "//"       { comment lexbuf }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | '['        { LBRACKET }
  | ']'        { RBRACKET }
  | ','        { COMMA }
  | ';'        { SEMICOLON }
  | '\\'       { BACKSLASH }
  | '|'        { PIPE }
  | '.'        { DOT }
  | "->"       { ARROW }
  | "=>"       { FAT_ARROW }
  | float as f { FLOAT (float_of_string f) }
  | hex_int as h { INT (int_of_string h) }
  | int as i   { INT (int_of_string i) }
  | '"'        { string (Buffer.create 16) lexbuf }
  | var_ident as s { get_keyword_or_ident s }
  | ctor_ident as s { get_keyword_or_ident s }
  | _ as c {
      if is_op_char c then
        let buf = Buffer.create 4 in
        Buffer.add_char buf c;
        operator buf lexbuf
      else
        raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c))
    }
  | eof        { EOF }

and operator buf = parse
  | _ as c {
      if is_op_char c then (
        Buffer.add_char buf c;
        operator buf lexbuf
      ) else (
        lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
        let op = Buffer.contents buf in
        match op with
        | ":" -> COLON
        | "=" -> EQUALS
        | _ -> OPERATOR (Intern.intern op)
      )
    }
  | "" { 
      let op = Buffer.contents buf in
      match op with
      | ":" -> COLON
      | "=" -> EQUALS
      | _ -> OPERATOR (Intern.intern op)
    }

and comment = parse
  | newline    { Lexing.new_line lexbuf; token lexbuf }
  | eof        { EOF }
  | _          { comment lexbuf }

and string buf = parse
  | '"'        { STRING (Buffer.contents buf) }
  | '\\' 'n'   { Buffer.add_char buf '\n'; string buf lexbuf }
  | '\\' 'r'   { Buffer.add_char buf '\r'; string buf lexbuf }
  | '\\' 't'   { Buffer.add_char buf '\t'; string buf lexbuf }
  | '\\' '\\'  { Buffer.add_char buf '\\'; string buf lexbuf }
  | '\\' '"'   { Buffer.add_char buf '"'; string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; string buf lexbuf }
  | eof        { raise (Lexing_error "Unclosed string") }
  | _ as c     { raise (Lexing_error (Printf.sprintf "Invalid character in string: %c" c)) }
