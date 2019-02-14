(** the type that is JSON friendly for {!Sxfiler_domain.Condition.t} *)
type t = {contexts : string list} [@@deriving yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Condition.t
