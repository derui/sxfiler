type error =
  | Empty_name
  | Empty_color_key
  | Illegal_theme
  | Invalid_color_code of string
[@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Sxfiler_generated.Theme.ColorTheme.t
     and type domain := Sxfiler_domain.Theme.color_pairs
     and type error := error
