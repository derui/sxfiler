(** the type that is JSON friendly for {!Sxfiler_domain.File_tree.t} *)
type t =
  { location : string
  ; nodes : Node.t list }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.File_tree.t
