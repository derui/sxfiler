(** the type that is JSON friendly for {!Sxfiler_domain.File_item.t} *)
type t =
  { id : string
  ; parent : string
  ; name : string
  ; stat : File_stat.t
  ; link_path : string option [@key "linkPath"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.File_item.t
