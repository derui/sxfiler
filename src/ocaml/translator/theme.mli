type error =
  | Empty_name
  | Empty_color_key
  | Invalid_color_code of string
[@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Sxfiler_generated.Theme.Theme.t
     and type domain := Sxfiler_domain.Theme.t
     and type error := error
