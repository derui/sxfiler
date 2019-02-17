(** the type that is JSON friendly for {!Sxfiler_domain.Location_record.t} *)
type t =
  { location : string
  ; timestamp : string }
[@@deriving show, yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Location_record.t
