type t =
  { location : string
  ; items : File_item.t list }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
(** the type that is JSON friendly for {!Sxfiler_domain.File_list.t} *)

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.File_list.t
