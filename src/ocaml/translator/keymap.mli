type error =
  | Invalid_context
  | Invalid_key     of string
[@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Sxfiler_generated.Keymap.Keymap.t
     and type domain := Sxfiler_domain.Keymap.t
     and type error := error
