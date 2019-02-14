type key =
  { key : string
  ; action : string
  ; when_ : Condition.t }

(** the type that is JSON friendly for {!Sxfiler_domain.Key_map.t} *)
type t = {bindings : key list} [@@deriving yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Key_map.t
