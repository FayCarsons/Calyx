open Core

module M = struct
  type t =
    [ `Expected of string * string
    | `UnificationFailure of string * string
    | `InferLambda
    | `NotFound of Ident.t
    | `NoField of Ident.t * (Ident.t * string) list
    | `Occurs of string
    | `Parser of string
    | `Todo
    ]
  [@@deriving show, sexp]
end

include Writer.Make (M)
include M
