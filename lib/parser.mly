%{
open Term
module Constructors = ConstructorSet.M

let make_var (ident : Ident.t) : Term.cst = `Var ident
let make_infix left op right = 
  `Infix { left; op = `Var op; right }

%}

/* Tokens */
%token <int> INT
%token <float> FLOAT  
%token <string> STRING
%token <Ident.t> IDENT
%token <Ident.t> OPERATOR
%token <bool> BOOL

/* Keywords */
%token DEF DO END IF THEN ELSE LET IN MATCH WITH
%token TYPE DATA WHERE CONST

/* Delimiters */
%token LPAREN RPAREN UNIT
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token ARROW FAT_ARROW EQUALS COLON COMMA SEMICOLON PIPE BACKSLASH DOT BANG
%token EOF

%start <Term.cst Term.declaration list> program
%start <Term.cst> expr_top

%%

(* Use Menhir's standard library functions for cleaner grammar *)

let program := 
  | decls = list(declaration); EOF; { decls }

let declaration :=
  | def_decl
  | const_decl  
  | data_decl

let def_decl :=
  DEF; ident = IDENT; params = params; ARROW; ret = type_expr; DO; body = expr; {
    let rec build_lam params body =
      match params with
      | [] -> body
      | (_, x, _) :: rest -> `Lam (x, build_lam rest body)
    in
    let rec build_type params ret =
      match params with
      | [] -> ret
      | (plicity, ident, dom) :: rest -> `Pi {plicity; ident; dom; cod = build_type rest ret}
    in
    let body = build_lam params body in
    let typ = build_type params ret in
    Function { ident; typ; body }
  }

let const_decl := 
  | CONST; ident = IDENT; typ = preceded(COLON, type_expr); 
    EQUALS; body = expr; {
      Constant { ident; typ; body }
    }

let data_decl :=
  | DATA; ident = IDENT; params = type_param*; WHERE; fields = record_type_fields; {
    let params = Ident.Map.of_alist_exn params in
    RecordDecl { ident; params; fields }
  }
  | DATA; ident = IDENT; params = type_param*; WHERE; constructors = constructor+; {
    let constructors = Ident.Map.of_alist_exn constructors
    and params = Ident.Map.of_alist_exn params in
    SumDecl { ident; params; constructors }
  }

let type_param :=
  | LPAREN; x = IDENT; COLON; ty = type_expr; RPAREN; { (x, ty) }

let constructor :=
  | PIPE; name = IDENT; tys = type_atom*; { Constructors.add name; (name, tys) }

let params :=
  | ps = param+; { List.concat ps }
  | UNIT; { [Explicit, Ident.Intern.underscore, `Var (Ident.Intern.intern "Unit")] }

let param :=
  | LPAREN; bs = binders; RPAREN; { List.map (fun (x, ty) -> Explicit, x, ty) bs }
  | LBRACE; bs = binders; RBRACE; { List.map (fun (x, ty) -> Implicit, x, ty) bs }

let binders :=
  | x = IDENT; COLON; ty = type_expr; { [(x, ty)] }
  | x = IDENT; COMMA; rest = binders; { (x, snd (List.hd rest)) :: rest }
  | x = IDENT; { [(x, `Type)] }

(* Type expressions *)
let type_expr :=
  | type_app
  | dom = type_app; ARROW; cod = type_expr; {
      `Pi { plicity = Explicit; ident = Ident.Intern.underscore; dom; cod }
    }
  | LPAREN; ident = IDENT; COLON; dom = type_expr; RPAREN; ARROW; cod = type_expr; {
      `Pi {plicity = Explicit; ident; dom; cod}
    }
  | LBRACE; ident = IDENT; COLON; dom = type_expr; RBRACE; ARROW; cod = type_expr; {
      `Pi {plicity = Implicit; ident; dom; cod}
    }
  | LBRACE; ident = IDENT; RBRACE; ARROW; cod = type_expr; {
      `Pi {plicity = Implicit; ident; dom = `Type; cod}
    }

let type_app :=
  | type_atom
  | f = type_app; arg = type_atom; { `App (f, arg) }

let type_atom :=
  | x = IDENT; { make_var x }
  | TYPE; { `Type }
  | LPAREN; ty = type_expr; RPAREN; { ty }
  | LBRACE; fields = record_type_fields; tail = record_type_tail; RBRACE; {
      `RecordType { fields; tail }
    }

let record_type_fields :=
  | fields = separated_list(COMMA, record_type_field); { Ident.Map.of_alist_exn fields }

let record_type_field :=
  | name = IDENT; COLON; ty = type_expr; { (name, ty) }

let record_type_tail :=
  | { ImplicitTail }
  | PIPE; x = IDENT; { ExplicitTail x }
  | BANG; { TailClosed }

let expr_top :=
  | e = expr; EOF; { e }

(* Expressions *)
let expr :=
  | expr_let
  | expr_if
  | expr_match
  | expr_lambda
  | expr_ann

let expr_let :=
  | LET; x = IDENT; ty = option(preceded(COLON, type_expr)); 
    EQUALS; v = expr; IN; body = expr; {
      `Let (x, ty, v, body)
    }

let expr_if :=
    | IF; cond = expr; THEN; t = expr; ELSE; f = expr; END; {
      `If (cond, t, f)
    }

let expr_match :=
  | MATCH; e = expr; WITH; arms = match_arms; END; {
      `Match (e, arms)
    }

let match_arms :=
  | option(PIPE); arms = separated_nonempty_list(PIPE, match_arm); { arms }

let match_arm :=
  | p = pattern; ARROW; e = expr; { (p, e) }

let pattern :=
  | x = IDENT; ps = pattern_atom+; { 
      PCtor (x, ps) 
    }
  | p = pattern_atom; { p }

let pattern_atom :=
  | x = IDENT; {
      if Constructors.member x
      then PCtor (x, [])
      else PVar x
    }
  | i = INT; { PLit (Int i) }
  | b = BOOL; { PLit (Bool b) }
  | LPAREN; p = pattern; RPAREN; { p }

let expr_lambda :=
  | BACKSLASH; params = nonempty_list(IDENT); ARROW; body = expr; {
      let rec build_lam = function
        | [] -> body
        | x :: xs -> `Lam (x, build_lam xs)
      in
      build_lam params
    }

let expr_ann :=
  | e = expr_infix; COLON; ty = type_expr; { `Ann (e, ty) }
  | expr_infix

let expr_infix :=
  | e = expr_app; { e }
  | left = expr_app; op = OPERATOR; right = expr_infix; {
      make_infix left op right
    }

let expr_app :=
  | f = expr_app; arg = expr_proj; { `App (f, arg) }
  | expr_proj

let expr_proj :=
  | e = expr_proj; DOT; field = IDENT; { 
      `Proj (e, field) 
    }
  | expr_atom

let expr_atom :=
  | i = INT; { `Lit (Int i) }
  | f = FLOAT; { `Lit (Float f) }
  | b = BOOL; { `Lit (Bool b) }
  | x = IDENT; { make_var x }
  | LPAREN; e = expr; RPAREN; { e }
  | LBRACE; fields = record_fields; RBRACE; {
      `Lit (Record fields)
    }

let record_fields :=
  | fields = separated_list(COMMA, record_field); { Ident.Map.of_alist_exn fields }

let record_field :=
  | name = IDENT; EQUALS; value = expr; {
      (name, value)
    }

