(** Translators for {!Sxfiler_domain.Task_interaction}. This module can not support conversion from
    type defined in module to domain.
*)

(** Type of interaction *)
module Interaction_typ : sig
  type t =
    | Yes_no [@name "yes-no"]
        | String [@name "string"]
        | Int [@name "int"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator
    with type t := t
     and type domain := Sxfiler_domain.Task.Interaction_typ.t
end

(** interaction *)
module Interaction : sig
  type t =
    | Yes_no of bool [@name "yes-no"]
          | String of string [@name "string"]
          | Int of int [@name "int"]
  [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Task.Interaction.t
end

(** the type that is JSON friendly for {!Sxfiler_domain.Task_interaction.t} *)
type t =
  { id : string
  ; task_id : string [@key "taskId"]
  ; accept_interactions : Interaction_typ.t list [@key "acceptInteractions"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include
  Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Task_interaction.t
