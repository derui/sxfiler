(** Translators for {!Sxfiler_domain.Task_interaction}. This module can not support conversion from
    type defined in module to domain. *)

module Reply : sig
  (** Type of interaction *)
  type typ =
    | Overwrite of bool [@name "overwrite"]
    | Rename of {new_name : string [@key "newName"]} [@name "rename"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  type t =
    { reply : typ [@key "reply"]
    ; task_id : string [@key "taskId"] }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator
      with type t := t
       and type domain := Sxfiler_domain.Task_interaction.Reply.t
end

module Suggestion : sig
  (** Type of interaction *)

  type typ =
    | Overwrite [@name "overwrite"]
    | Rename [@name "rename"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  type t =
    { suggestions : typ list [@key "suggestions"]
    ; item_name : string [@key "itemName"]
    ; task_id : string [@key "taskId"] }
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator
      with type t := t
       and type domain := Sxfiler_domain.Task_interaction.Suggestion.t
end
