module Correction : sig
  (** {!type:t} takes method to avoid error in transportation *)
  type t =
    | Rename of string
    | Overwrite
  [@@deriving yojson]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Plan.Correction.t
end

module Prediction : sig
  type t =
    | Need_fix
    | Fix of Correction.t
    | No_problem
  [@@deriving yojson]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Plan.Prediction.t
end

(** {!Target_node} defines a target node of the plan. *)
module Target_node : sig
  type t =
    { node_id : string [@key "nodeId"]
    ; prediction : Prediction.t }
  [@@deriving yojson]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Plan.Target_node.t
end

(** [!t] is result of plan. *)
type t =
  { id : string
  ; target_nodes : Target_node.t list [@key "targetNodes"] }
[@@deriving yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Plan.t
