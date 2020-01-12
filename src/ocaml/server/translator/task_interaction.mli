(** Translators for {!Sxfiler_domain.Task_interaction}. This module can not support conversion from
    type defined in module to domain. *)

module Suggestion : sig
  (* match to translate JSON to OCaml data type generated protobuf *)
  type t = {
    suggestions : int list;
    itemName : string;
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator
      with type t := t
       and type domain := Sxfiler_domain.Task_interaction.Suggestion.t
end
