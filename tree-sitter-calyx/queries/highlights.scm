; Keywords
[
  "def"
  "do"
  "end"
  "if"
  "then"
  "else"
  "let"
  "in"
  "match"
  "with"
  "data"
  "where"
  "const"
] @keyword

; Operators
"->" @operator
"=" @operator
":" @operator
"|" @operator
"\\" @operator
"." @operator
"!" @operator
(operator) @operator

; Literals
(integer) @number
(float) @number
(boolean) @constant.builtin

; Types
(type_identifier) @type
(record_type) @type
"Type" @type.builtin

; Functions
(function_def name: (identifier) @function)
(application) @function.call

; Variables and parameters
(binders (identifier) @variable.parameter)
(let_expr name: (identifier) @variable)
(pattern (identifier) @variable)
(constructor_pattern (identifier) @variable)
(identifier) @variable

; Comments
(comment) @comment

; Punctuation
["(" ")" "{" "}" "[" "]"] @punctuation.bracket
"," @punctuation.delimiter

; Lambda
(lambda "\\" @keyword.function)
