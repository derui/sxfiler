(** Translators for {!Sxfiler_domain.Task_interaction}. This module can not support conversion from
    type defined in module to domain.
*)

(** Type of interaction *)
module Interaction : sig
  type typ =
    | Yes_no [@name "yes-no"]
    | String [@name "string"]
    | Int [@name "int"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator with type t := typ and type domain := Sxfiler_domain.Task.interaction
end

(** the type that is JSON friendly for {!Sxfiler_domain.Task_interaction.t} *)
type t =
  { id : string
  ; task_id : string
  ; filter_interaction : Interaction.typ list }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include
  Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Task_interaction.t
