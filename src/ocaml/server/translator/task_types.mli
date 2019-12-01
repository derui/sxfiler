(** Translators for {!Sxfiler_domain.Task_interaction}. This module can not support conversion from
    type defined in module to domain. *)

module Task_id : sig
  type t = string [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Task_types.id
end
