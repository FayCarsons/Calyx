module.exports = grammar({
  name: 'calyx',

  extras: $ => [/\s/, $.comment],

  rules: {
    source_file: $ => repeat($._declaration),

    _declaration: $ => choice(
      $.function_def,
      $.constant_def,
      $.data_def,
    ),

    function_def: $ => seq(
      'def',
      field('name', $.identifier),
      field('params', $._params),
      '->',
      field('return_type', $._type),
      'do',
      field('body', $._expr),
    ),

    constant_def: $ => seq(
      'const',
      field('name', $.type_identifier),
      ':',
      field('type', $._type),
      '=',
      field('value', $._expr),
    ),

    data_def: $ => seq(
      'data',
      field('name', $.type_identifier),
      repeat($._data_param),
      'where',
      choice($.record_fields, repeat1($.constructor)),
    ),

    _data_param: $ => choice(
      $.type_param,
      $.implicit_type_param,
    ),

    type_param: $ => seq('(', $.identifier, ':', $._type, ')'),
    implicit_type_param: $ => seq('{', $.identifier, '}'),

    constructor: $ => seq(
      '|',
      field('name', $.type_identifier),
      repeat($._type_atom),
    ),

    _params: $ => choice(
      $.unit,
      repeat1($.param),
    ),

    param: $ => choice($.explicit_param, $.implicit_param),

    explicit_param: $ => seq('(', $.binders, ')'),
    implicit_param: $ => seq('{', $.binders, '}'),

    binders: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier)),
      optional(seq(':', $._type)),
    ),

    unit: $ => '()',

    // Types
    _type: $ => choice(
      $.function_type,
      $.implicit_function_type,
      $._type_app,
    ),

    _type_app: $ => choice(
      $.type_application,
      $._type_atom,
    ),

    type_application: $ => prec.left(2, seq($._type_app, $._type_atom)),

    _type_atom: $ => choice(
      $.type_identifier,
      $.identifier,
      'Type',
      $.record_type,
      seq('(', $._type, ')'),
    ),

    function_type: $ => prec.right(1, seq($._type_app, '->', $._type)),

    implicit_function_type: $ => prec.right(1, seq(
      '{', $.identifier, optional(seq(':', $._type)), '}', '->', $._type
    )),

    record_type: $ => seq(
      '{',
      optional($.record_type_fields),
      optional(choice('!', seq('|', $.identifier))),
      '}',
    ),

    record_type_fields: $ => seq(
      $.record_type_field,
      repeat(seq(',', $.record_type_field)),
      optional(','),
    ),

    record_type_field: $ => seq($.identifier, ':', $._type),

    record_fields: $ => seq(
      $.record_field_def,
      repeat(seq(',', $.record_field_def)),
      optional(','),
    ),

    record_field_def: $ => seq($.identifier, ':', $._type),

    // Expressions
    _expr: $ => choice(
      $.let_expr,
      $.if_expr,
      $.match_expr,
      $.lambda,
      $._expr_infix,
    ),

    let_expr: $ => seq(
      'let',
      field('name', $.identifier),
      optional(seq(':', $._type)),
      '=',
      field('value', $._expr),
      'in',
      field('body', $._expr),
    ),

    if_expr: $ => seq(
      'if', field('condition', $._expr),
      'then', field('then', $._expr),
      'else', field('else', $._expr),
      'end',
    ),

    match_expr: $ => seq(
      'match', $._expr, 'with',
      repeat1($.match_arm),
      'end',
    ),

    match_arm: $ => seq(optional('|'), $.pattern, '->', $._expr),

    pattern: $ => choice(
      $.constructor_pattern,
      $._pattern_atom,
    ),

    constructor_pattern: $ => prec.left(seq(
      $.type_identifier,
      repeat1($._pattern_atom),
    )),

    _pattern_atom: $ => choice(
      $.identifier,
      $.type_identifier,
      $.integer,
      $.boolean,
      seq('(', $.pattern, ')'),
    ),

    lambda: $ => seq('\\', repeat1($.identifier), '->', $._expr),

    _expr_infix: $ => choice(
      $.binary_expr,
      $._expr_app,
    ),

    binary_expr: $ => prec.left(1, seq($._expr_app, $.operator, $._expr_infix)),

    _expr_app: $ => choice(
      $.application,
      $._expr_proj,
    ),

    application: $ => prec.left(2, seq($._expr_app, $._expr_proj)),

    _expr_proj: $ => choice(
      $.projection,
      $._expr_atom,
    ),

    projection: $ => prec.left(3, seq($._expr_proj, '.', $.identifier)),

    _expr_atom: $ => choice(
      $.identifier,
      $.type_identifier,
      $.integer,
      $.float,
      $.boolean,
      $.record_literal,
      $.list_literal,
      seq('(', $._expr, ')'),
    ),

    record_literal: $ => seq('{', optional($.record_field_values), '}'),

    record_field_values: $ => seq(
      $.record_field_value,
      repeat(seq(',', $.record_field_value)),
      optional(','),
    ),

    record_field_value: $ => seq($.identifier, '=', $._expr),

    list_literal: $ => seq('[', optional($.list_elements), ']'),

    list_elements: $ => seq(
      $._expr,
      repeat(seq(',', $._expr)),
      optional(','),
    ),

    // Terminals
    identifier: $ => /[a-z_][a-zA-Z0-9_']*/,
    type_identifier: $ => /[A-Z][a-zA-Z0-9_']*/,
    integer: $ => /-?[0-9]+/,
    float: $ => /-?[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?/,
    boolean: $ => choice('True', 'False'),
    operator: $ => /[+\-*/<>=!&|^%~?@$:]+/,
    comment: $ => seq('--', /.*/),
  },
});
