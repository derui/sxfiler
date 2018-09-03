(** This type defines operations in simulation. *)
module Operation = struct
  type t =
    | Append
    | Delete
    | Remained

  let to_int = function Append -> 0 | Delete -> 1 | Remained -> 2
  let of_int = function 0 -> Some Append | 1 -> Some Delete | 2 -> Some Remained | _ -> None
end

(** Plan is difference between current nodes and simulated it. *)
type node_plan =
  { operation : Operation.t
  ; node : Node.t }

(** [simulated] is result of simulation. *)
type plan =
  { source : node_plan list
  ; dest : node_plan list }

module type S = sig
  val make : Workbench.t -> plan
  (** [make workbench] returns the plan with [workbench] *)
end
