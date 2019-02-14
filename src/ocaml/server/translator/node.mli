(** the type that is JSON friendly for {!Sxfiler_domain.Node.t} *)
type t =
  { id : string
  ; parent : string
  ; name : string
  ; stat : File_stat.t
  ; link_path : string option [@key "linkPath"] }
[@@deriving yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Node.t
