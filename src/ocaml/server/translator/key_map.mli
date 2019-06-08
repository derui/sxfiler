type when_ = {contexts : string list} [@@deriving show]

type binding =
  { key : string
  ; action : string
  ; when_ : when_ [@key "when"] }
[@@deriving show]

(** the type that is JSON friendly for {!Sxfiler_domain.Key_map.t} *)
type t = {bindings : binding list}
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Key_map.t
