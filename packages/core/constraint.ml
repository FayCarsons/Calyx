open Core

type t =
  | Equals of Term.value * Term.value
  | HasField of Term.value * Ident.t * Term.value
  | Subtype of
      { sub : Term.value
      ; super : Term.value
      }
[@@deriving sexp]

let equals a b = Equals (a, b)
let subtype ~sub ~super = Subtype { sub; super }
let has_field ~record ~field_name ~field_type = HasField (record, field_name, field_type)
let show : t -> string = Fun.compose Sexp.to_string_hum sexp_of_t

let pp : Format.formatter -> t -> unit =
  fun fmt self -> Format.fprintf fmt "%s" (show self)
;;
