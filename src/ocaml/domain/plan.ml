type id = string [@@deriving show]

module Correction = struct
  (** {!type:t} takes method to avoid error in transportation *)
  type t =
    | Rename of string
    | Overwrite
  [@@deriving show]
end

module Prediction = struct
  type t =
    | Need_fix
    | Fix of Correction.t
    | No_problem
  [@@deriving show]
end

(** {!Target_node} defines a target node of the plan. *)
module Target_node = struct
  type t =
    { node_id : Node.id
    ; prediction : Prediction.t }
  [@@deriving show, fields]

  let need_fix id = {node_id = id; prediction = Prediction.Need_fix}
  let fix id correction = {node_id = id; prediction = Prediction.Fix correction}
  let no_problem id = {node_id = id; prediction = Prediction.No_problem}
end

(** {!Executor} define signature to do plan. *)
module type Executor = sig
  val do_plan : Target_node.t list -> (unit, string) result Lwt.t
end

(** [!t] is result of plan. *)
type t =
  { id : id
  ; executor : (module Executor) [@printer fun _ _ -> ()]
  ; target_nodes : Target_node.t list }
[@@deriving show, fields]

(** [make ~id ~operation ~nodes] makes new plan [t] instance from. *)
let make ~id ~executor ~target_nodes = {id; executor; target_nodes}

(** [execute t] execute the plan [t] with {!Executor}. *)
let execute t =
  let module E = (val t.executor) in
  E.do_plan t.target_nodes

module type Repository = sig
  val store : t -> unit Lwt.t
  (** [store t] saves instance [t] *)

  val resolve : id -> t option Lwt.t
  (** [resolve id] get the instance having [id]. *)

  val remove : t -> unit Lwt.t
  (** [remove t] removes [t] from repository. *)
end

(** Factory interface for [type: t] *)
module Factory = struct
  module type S = sig
    val create : executor:(module Executor) -> target_nodes:Target_node.t list -> t
  end

  module Make (G : Id_generator_intf.Gen_random with type id = id) : S = struct
    let create = make ~id:(G.generate ())
  end
end
